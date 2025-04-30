;; community-governance
;; 
;; This contract implements a decentralized governance system for a time banking platform,
;; allowing community members to propose and vote on critical platform parameters and rules.
;; It enables collective decision-making through a structured proposal and voting mechanism,
;; fostering community ownership and sustainable self-governance.

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-PROPOSAL-DOES-NOT-EXIST (err u101))
(define-constant ERR-PROPOSAL-EXPIRED (err u102))
(define-constant ERR-PROPOSAL-ACTIVE (err u103))
(define-constant ERR-ALREADY-VOTED (err u104))
(define-constant ERR-INSUFFICIENT-REPUTATION (err u105))
(define-constant ERR-INVALID-VOTE (err u106))
(define-constant ERR-PROPOSAL-LIMIT-REACHED (err u107))
(define-constant ERR-INVALID-PARAMETERS (err u108))

;; Constants
(define-constant VOTING_PERIOD_BLOCKS u1440) ;; Approximately 10 days based on ~10 min block time
(define-constant MIN_REPUTATION_TO_PROPOSE u100) ;; Minimum reputation needed to create proposals
(define-constant MIN_REPUTATION_TO_VOTE u10) ;; Minimum reputation needed to vote
(define-constant MAX_ACTIVE_PROPOSALS u10) ;; Maximum number of active proposals allowed

;; Governance parameters
(define-data-var initial-credit-allocation uint u10) ;; Initial time credits for new users
(define-data-var dispute-resolution-threshold uint u5) ;; Number of votes needed to resolve a dispute
(define-data-var minimum-activity-period uint u30) ;; Days of inactivity before penalties
(define-data-var governance-version uint u1) ;; Version control for governance rules

;; Data structures
(define-map proposals
  { proposal-id: uint }
  {
    creator: principal,
    title: (string-ascii 100),
    description: (string-utf8 1000),
    proposed-changes: (list 10 { param-name: (string-ascii 50), param-value: uint }),
    created-at-block: uint,
    expires-at-block: uint,
    status: (string-ascii 20), ;; "active", "passed", "rejected", "executed"
    yes-votes: uint,
    no-votes: uint,
    total-voting-power: uint
  }
)

(define-map user-votes
  { proposal-id: uint, voter: principal }
  { vote: (string-ascii 3), voting-power: uint }
)

(define-map user-reputation
  { user: principal }
  { score: uint, last-action-height: uint }
)

(define-data-var proposal-count uint u0)
(define-data-var active-proposal-count uint u0)

;; Private functions

(define-private (is-authorized-voter (user principal))
  (let ((reputation (default-to { score: u0, last-action-height: u0 } 
                              (map-get? user-reputation { user: user }))))
    (>= (get score reputation) MIN_REPUTATION_TO_VOTE)
  )
)

(define-private (is-authorized-proposer (user principal))
  (let ((reputation (default-to { score: u0, last-action-height: u0 } 
                              (map-get? user-reputation { user: user }))))
    (>= (get score reputation) MIN_REPUTATION_TO_PROPOSE)
  )
)

(define-private (update-reputation (user principal) (points int))
  (let ((current-rep (default-to { score: u0, last-action-height: u0 } 
                                (map-get? user-reputation { user: user }))))
    (map-set user-reputation 
      { user: user }
      { 
        score: (if (< points i0)
                  (if (> (get score current-rep) (to-uint (abs points)))
                    (- (get score current-rep) (to-uint (abs points)))
                    u0)
                  (+ (get score current-rep) (to-uint points))),
        last-action-height: block-height
      }
    )
  )
)

(define-private (calculate-voting-power (user principal))
  (let ((reputation (default-to { score: u0, last-action-height: u0 } 
                              (map-get? user-reputation { user: user }))))
    ;; Simple linear voting power based on reputation
    (get score reputation)
  )
)

(define-private (execute-proposal (proposal-id uint))
  (let (
    (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-PROPOSAL-DOES-NOT-EXIST))
    (yes-votes (get yes-votes proposal))
    (no-votes (get no-votes proposal))
    (total-votes (+ yes-votes no-votes))
  )
    (if (and (> total-votes u0) (> yes-votes no-votes))
      (begin
        ;; Apply the proposed changes to governance parameters
        (map apply-parameter-change (get proposed-changes proposal))
        
        ;; Update proposal status
        (map-set proposals 
          { proposal-id: proposal-id }
          (merge proposal { status: "executed" })
        )
        (var-set active-proposal-count (- (var-get active-proposal-count) u1))
        (ok true)
      )
      (begin
        ;; Mark the proposal as rejected
        (map-set proposals 
          { proposal-id: proposal-id }
          (merge proposal { status: "rejected" })
        )
        (var-set active-proposal-count (- (var-get active-proposal-count) u1))
        (ok false)
      )
    )
  )
)

(define-private (apply-parameter-change (change { param-name: (string-ascii 50), param-value: uint }))
  (let (
    (param-name (get param-name change))
    (param-value (get param-value change))
  )
    (match param-name
      "initial-credit-allocation" (var-set initial-credit-allocation param-value)
      "dispute-resolution-threshold" (var-set dispute-resolution-threshold param-value)
      "minimum-activity-period" (var-set minimum-activity-period param-value)
      ;; Add more parameters as needed
      param-name ;; Default case, no action
    )
  )
)

(define-private (close-expired-proposals)
  (let ((current-block block-height))
    (map close-if-expired 
      (list-proposals-function))
    (ok true)
  )
)

(define-private (close-if-expired (proposal-id uint))
  (let ((proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-PROPOSAL-DOES-NOT-EXIST)))
    (if (and 
          (is-eq (get status proposal) "active") 
          (>= current-block (get expires-at-block proposal)))
      (execute-proposal proposal-id)
      (ok true)
    )
  )
)

;; This is a stub as Clarity doesn't support returning dynamic lists from maps
;; In a real implementation, you'd need to use a different approach, like storing
;; a list of active proposal IDs separately
(define-private (list-proposals-function)
  (list u0) ;; Placeholder
)

;; Read-only functions

(define-read-only (get-governance-parameters)
  {
    initial-credit-allocation: (var-get initial-credit-allocation),
    dispute-resolution-threshold: (var-get dispute-resolution-threshold),
    minimum-activity-period: (var-get minimum-activity-period),
    governance-version: (var-get governance-version)
  }
)

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals { proposal-id: proposal-id })
)

(define-read-only (get-user-vote (proposal-id uint) (user principal))
  (map-get? user-votes { proposal-id: proposal-id, voter: user })
)

(define-read-only (get-user-reputation (user principal))
  (default-to { score: u0, last-action-height: u0 } 
             (map-get? user-reputation { user: user }))
)

(define-read-only (can-propose (user principal))
  (is-authorized-proposer user)
)

(define-read-only (can-vote (user principal))
  (is-authorized-voter user)
)

;; Public functions

;; Create a new governance proposal
(define-public (create-proposal 
  (title (string-ascii 100))
  (description (string-utf8 1000))
  (proposed-changes (list 10 { param-name: (string-ascii 50), param-value: uint }))
)
  (let (
    (proposer tx-sender)
    (proposal-id (+ (var-get proposal-count) u1))
  )
    ;; Check authorization
    (asserts! (is-authorized-proposer proposer) ERR-INSUFFICIENT-REPUTATION)
    
    ;; Check we haven't hit the maximum number of active proposals
    (asserts! (< (var-get active-proposal-count) MAX_ACTIVE_PROPOSALS) ERR-PROPOSAL-LIMIT-REACHED)
    
    ;; Create the proposal
    (map-set proposals
      { proposal-id: proposal-id }
      {
        creator: proposer,
        title: title,
        description: description,
        proposed-changes: proposed-changes,
        created-at-block: block-height,
        expires-at-block: (+ block-height VOTING_PERIOD_BLOCKS),
        status: "active",
        yes-votes: u0,
        no-votes: u0,
        total-voting-power: u0
      }
    )
    
    ;; Update counters
    (var-set proposal-count proposal-id)
    (var-set active-proposal-count (+ (var-get active-proposal-count) u1))
    
    ;; Reward the proposer with reputation
    (update-reputation proposer i5)
    
    (ok proposal-id)
  )
)

;; Vote on an existing proposal
(define-public (vote-on-proposal (proposal-id uint) (vote-value (string-ascii 3)))
  (let (
    (voter tx-sender)
    (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-PROPOSAL-DOES-NOT-EXIST))
    (voting-power (calculate-voting-power voter))
  )
    ;; Check authorization
    (asserts! (is-authorized-voter voter) ERR-INSUFFICIENT-REPUTATION)
    
    ;; Ensure the proposal is still active
    (asserts! (is-eq (get status proposal) "active") ERR-PROPOSAL-EXPIRED)
    (asserts! (< block-height (get expires-at-block proposal)) ERR-PROPOSAL-EXPIRED)
    
    ;; Ensure the user hasn't already voted
    (asserts! (is-none (map-get? user-votes { proposal-id: proposal-id, voter: voter })) ERR-ALREADY-VOTED)
    
    ;; Ensure valid vote value
    (asserts! (or (is-eq vote-value "yes") (is-eq vote-value "no")) ERR-INVALID-VOTE)
    
    ;; Record the vote
    (map-set user-votes
      { proposal-id: proposal-id, voter: voter }
      { vote: vote-value, voting-power: voting-power }
    )
    
    ;; Update the vote counts on the proposal
    (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal { 
        yes-votes: (if (is-eq vote-value "yes") 
                      (+ (get yes-votes proposal) voting-power)
                      (get yes-votes proposal)),
        no-votes: (if (is-eq vote-value "no") 
                    (+ (get no-votes proposal) voting-power)
                    (get no-votes proposal)),
        total-voting-power: (+ (get total-voting-power proposal) voting-power)
      })
    )
    
    ;; Reward voter with a small reputation increase
    (update-reputation voter i1)
    
    (ok true)
  )
)

;; Execute a proposal if it's past its expiry and hasn't been executed yet
(define-public (finalize-proposal (proposal-id uint))
  (let ((proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-PROPOSAL-DOES-NOT-EXIST)))
    ;; Check if the proposal can be executed
    (asserts! (is-eq (get status proposal) "active") ERR-PROPOSAL-EXPIRED)
    (asserts! (>= block-height (get expires-at-block proposal)) ERR-PROPOSAL-ACTIVE)
    
    ;; Execute the proposal (this will handle passed/rejected status update)
    (execute-proposal proposal-id)
  )
)

;; Administrative function to award initial reputation
;; In a production system, this might be controlled by a DAO or multisig
(define-public (award-initial-reputation (user principal) (amount uint))
  (begin
    ;; Only contract owner can award initial reputation
    ;; In a more developed system, this would be a DAO vote
    (asserts! (is-eq tx-sender (as-contract tx-sender)) ERR-NOT-AUTHORIZED)
    
    (map-set user-reputation
      { user: user }
      { score: amount, last-action-height: block-height }
    )
    
    (ok true)
  )
)

;; Update governance version when significant changes are made
(define-public (update-governance-version (new-version uint))
  (begin
    ;; This would typically be controlled by a governance vote
    (asserts! (is-eq tx-sender (as-contract tx-sender)) ERR-NOT-AUTHORIZED)
    (var-set governance-version new-version)
    (ok new-version)
  )
)