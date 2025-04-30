;; dispute-resolution.clar
;; Provides a fair mechanism to resolve disputes between service providers and requestors
;; in the time-banking system. Implements a voting-based resolution system where trusted
;; community members can review evidence and vote on the outcome of disputes.

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-DISPUTE-NOT-FOUND (err u101))
(define-constant ERR-DISPUTE-ALREADY-EXISTS (err u102))
(define-constant ERR-DISPUTE-ALREADY-RESOLVED (err u103))
(define-constant ERR-INVALID-EVIDENCE (err u104))
(define-constant ERR-ALREADY-VOTED (err u105))
(define-constant ERR-NOT-ELIGIBLE-VOTER (err u106))
(define-constant ERR-VOTING-PERIOD-ENDED (err u107))
(define-constant ERR-VOTING-PERIOD-ACTIVE (err u108))
(define-constant ERR-INVALID-VOTE (err u109))
(define-constant ERR-NOT-INVOLVED-IN-DISPUTE (err u110))
(define-constant ERR-INVALID-DISPUTE-ID (err u111))

;; Data maps and variables

;; Stores information about each dispute
(define-map disputes
  { dispute-id: uint }
  {
    service-id: uint,
    provider: principal,
    requestor: principal,
    description: (string-utf8 500),
    evidence-hash: (optional (buff 32)),
    is-resolved: bool,
    resolution-in-favor-of: (optional principal),
    created-at: uint,
    voting-deadline: uint
  }
)

;; Tracks votes from eligible voters
(define-map votes
  { dispute-id: uint, voter: principal }
  { 
    vote: (string-ascii 20),  ;; "provider", "requestor", or "neither"
    timestamp: uint
  }
)

;; Tracks vote counts for each dispute
(define-map vote-counts
  { dispute-id: uint }
  {
    for-provider: uint,
    for-requestor: uint,
    for-neither: uint,
    total-votes: uint
  }
)

;; Keeps track of trusted community members who can vote
(define-map eligible-voters
  { voter: principal }
  { is-eligible: bool, reputation-score: uint }
)

;; Contract owner (initially the deployer)
(define-data-var contract-owner principal tx-sender)

;; Current dispute ID counter (increments for each new dispute)
(define-data-var current-dispute-id uint u0)

;; Duration of voting period in blocks (approximately 7 days with 10 min blocks)
(define-constant VOTING_PERIOD_BLOCKS u1008)

;; Minimum number of votes required for dispute resolution
(define-constant MIN_VOTES_REQUIRED u3)

;; Private functions

;; Check if principal is contract owner
(define-private (is-contract-owner (caller principal))
  (is-eq caller (var-get contract-owner))
)

;; Check if a user is eligible to vote
(define-private (is-eligible-to-vote (voter principal))
  (default-to false
    (get is-eligible
      (default-to { is-eligible: false, reputation-score: u0 }
        (map-get? eligible-voters { voter: voter })
      )
    )
  )
)

;; Check if a user is involved in a dispute
(define-private (is-involved-in-dispute (user principal) (dispute-id uint))
  (let ((dispute (unwrap! (map-get? disputes { dispute-id: dispute-id }) false)))
    (or (is-eq user (get provider dispute))
        (is-eq user (get requestor dispute)))
  )
)

;; Check if dispute exists
(define-private (dispute-exists (dispute-id uint))
  (is-some (map-get? disputes { dispute-id: dispute-id }))
)

;; Check if voting period is still active
(define-private (is-voting-active (dispute-id uint))
  (let ((dispute (unwrap! (map-get? disputes { dispute-id: dispute-id }) false)))
    (< block-height (get voting-deadline dispute))
  )
)

;; Initialize vote counts for a new dispute
(define-private (initialize-vote-counts (dispute-id uint))
  (map-set vote-counts
    { dispute-id: dispute-id }
    {
      for-provider: u0,
      for-requestor: u0,
      for-neither: u0,
      total-votes: u0
    }
  )
)

;; Record a vote and update vote counts
(define-private (record-vote (dispute-id uint) (voter principal) (vote-choice (string-ascii 20)))
  (let ((current-counts (unwrap! (map-get? vote-counts { dispute-id: dispute-id }) (err "Vote counts not found"))))
    (begin
      ;; Record the individual vote
      (map-set votes
        { dispute-id: dispute-id, voter: voter }
        { vote: vote-choice, timestamp: block-height }
      )
      
      ;; Update the vote counts based on the choice
      (map-set vote-counts
        { dispute-id: dispute-id }
        (merge current-counts
          (match vote-choice
            "provider" {
              for-provider: (+ (get for-provider current-counts) u1),
              total-votes: (+ (get total-votes current-counts) u1)
            }
            "requestor" {
              for-requestor: (+ (get for-requestor current-counts) u1),
              total-votes: (+ (get total-votes current-counts) u1)
            }
            "neither" {
              for-neither: (+ (get for-neither current-counts) u1),
              total-votes: (+ (get total-votes current-counts) u1)
            }
            current-counts  ;; Default case - should not reach here due to validation
          )
        )
      )
      (ok true)
    )
  )
)

;; Determine the winner based on vote counts
(define-private (determine-winner (dispute-id uint))
  (let (
    (vote-data (unwrap! (map-get? vote-counts { dispute-id: dispute-id }) (err "Vote counts not found")))
    (dispute (unwrap! (map-get? disputes { dispute-id: dispute-id }) (err "Dispute not found")))
    (provider-votes (get for-provider vote-data))
    (requestor-votes (get for-requestor vote-data))
    (neither-votes (get for-neither vote-data))
    (total-votes (get total-votes vote-data))
  )
    (if (< total-votes MIN_VOTES_REQUIRED)
      ;; Not enough votes - default to "neither"
      (some "neither")
      (cond
        ;; Clear winner: provider
        ((and (> provider-votes requestor-votes) (> provider-votes neither-votes))
         (some (get provider dispute)))
        ;; Clear winner: requestor
        ((and (> requestor-votes provider-votes) (> requestor-votes neither-votes))
         (some (get requestor dispute)))
        ;; Otherwise: neither wins (tie or "neither" has most votes)
        (true (some "neither"))
      )
    )
  )
)

;; Public functions

;; Create a new dispute
(define-public (create-dispute 
  (service-id uint) 
  (provider principal) 
  (requestor principal) 
  (description (string-utf8 500)))
  
  (let ((dispute-id (+ (var-get current-dispute-id) u1)))
    ;; Check that caller is either the provider or requestor
    (asserts! (or (is-eq tx-sender provider) (is-eq tx-sender requestor)) ERR-NOT-AUTHORIZED)
    
    ;; Create the dispute
    (map-set disputes
      { dispute-id: dispute-id }
      {
        service-id: service-id,
        provider: provider,
        requestor: requestor,
        description: description,
        evidence-hash: none,
        is-resolved: false,
        resolution-in-favor-of: none,
        created-at: block-height,
        voting-deadline: (+ block-height VOTING_PERIOD_BLOCKS)
      }
    )
    
    ;; Initialize vote counts
    (initialize-vote-counts dispute-id)
    
    ;; Increment the dispute ID counter
    (var-set current-dispute-id dispute-id)
    
    ;; Return the new dispute ID
    (ok dispute-id)
  )
)

;; Submit evidence for a dispute
(define-public (submit-evidence (dispute-id uint) (evidence-hash (buff 32)))
  (let ((dispute (unwrap! (map-get? disputes { dispute-id: dispute-id }) ERR-DISPUTE-NOT-FOUND)))
    ;; Check that caller is involved in the dispute
    (asserts! (is-involved-in-dispute tx-sender dispute-id) ERR-NOT-AUTHORIZED)
    
    ;; Check that dispute is not already resolved
    (asserts! (not (get is-resolved dispute)) ERR-DISPUTE-ALREADY-RESOLVED)
    
    ;; Check that voting period is still active
    (asserts! (is-voting-active dispute-id) ERR-VOTING-PERIOD-ENDED)
    
    ;; Update the dispute with evidence
    (map-set disputes
      { dispute-id: dispute-id }
      (merge dispute { evidence-hash: (some evidence-hash) })
    )
    
    (ok true)
  )
)

;; Vote on a dispute
(define-public (vote-on-dispute (dispute-id uint) (vote-choice (string-ascii 20)))
  (let ((dispute (unwrap! (map-get? disputes { dispute-id: dispute-id }) ERR-DISPUTE-NOT-FOUND)))
    ;; Check that voter is eligible
    (asserts! (is-eligible-to-vote tx-sender) ERR-NOT-ELIGIBLE-VOTER)
    
    ;; Check that voter is not involved in the dispute
    (asserts! (not (is-involved-in-dispute tx-sender dispute-id)) ERR-NOT-INVOLVED-IN-DISPUTE)
    
    ;; Check that dispute is not already resolved
    (asserts! (not (get is-resolved dispute)) ERR-DISPUTE-ALREADY-RESOLVED)
    
    ;; Check that voting period is still active
    (asserts! (is-voting-active dispute-id) ERR-VOTING-PERIOD-ENDED)
    
    ;; Check that voter hasn't already voted
    (asserts! (is-none (map-get? votes { dispute-id: dispute-id, voter: tx-sender })) ERR-ALREADY-VOTED)
    
    ;; Validate vote choice
    (asserts! (or (is-eq vote-choice "provider") 
                 (is-eq vote-choice "requestor") 
                 (is-eq vote-choice "neither")) 
             ERR-INVALID-VOTE)
    
    ;; Record the vote
    (record-vote dispute-id tx-sender vote-choice)
  )
)

;; Finalize a dispute after voting period ends
(define-public (finalize-dispute (dispute-id uint))
  (let (
    (dispute (unwrap! (map-get? disputes { dispute-id: dispute-id }) ERR-DISPUTE-NOT-FOUND))
  )
    ;; Check that dispute exists and is not already resolved
    (asserts! (not (get is-resolved dispute)) ERR-DISPUTE-ALREADY-RESOLVED)
    
    ;; Check that voting period has ended
    (asserts! (>= block-height (get voting-deadline dispute)) ERR-VOTING-PERIOD-ACTIVE)
    
    ;; Determine the winner
    (let ((winner (determine-winner dispute-id)))
      ;; Update the dispute as resolved
      (map-set disputes
        { dispute-id: dispute-id }
        (merge dispute 
          { 
            is-resolved: true,
            resolution-in-favor-of: winner
          }
        )
      )
      
      (ok winner)
    )
  )
)

;; Administrative functions

;; Add or update an eligible voter
(define-public (set-eligible-voter (voter principal) (is-eligible bool) (reputation-score uint))
  ;; Only contract owner can add/update voters
  (asserts! (is-contract-owner tx-sender) ERR-NOT-AUTHORIZED)
  
  (map-set eligible-voters
    { voter: voter }
    { is-eligible: is-eligible, reputation-score: reputation-score }
  )
  
  (ok true)
)

;; Transfer contract ownership
(define-public (transfer-ownership (new-owner principal))
  ;; Only current owner can transfer ownership
  (asserts! (is-contract-owner tx-sender) ERR-NOT-AUTHORIZED)
  
  (var-set contract-owner new-owner)
  (ok true)
)

;; Read-only functions

;; Get dispute details
(define-read-only (get-dispute (dispute-id uint))
  (map-get? disputes { dispute-id: dispute-id })
)

;; Get vote counts for a dispute
(define-read-only (get-vote-counts (dispute-id uint))
  (map-get? vote-counts { dispute-id: dispute-id })
)

;; Check if a user has voted on a dispute
(define-read-only (has-voted (dispute-id uint) (voter principal))
  (is-some (map-get? votes { dispute-id: dispute-id, voter: voter }))
)

;; Check if a user is eligible to vote
(define-read-only (check-voter-eligibility (voter principal))
  (is-eligible-to-vote voter)
)

;; Get a user's vote on a specific dispute
(define-read-only (get-user-vote (dispute-id uint) (voter principal))
  (map-get? votes { dispute-id: dispute-id, voter: voter })
)

;; Check if dispute is still in voting period
(define-read-only (check-voting-active (dispute-id uint))
  (if (dispute-exists dispute-id)
    (is-voting-active dispute-id)
    false
  )
)