```clarity
;; Time Banking System Contract
(define-constant ERR_UNAUTHORIZED (err u1))
(define-constant ERR_NOT_FOUND (err u2))
(define-constant ERR_INSUFFICIENT_HOURS (err u3))
(define-constant ERR_INVALID_RATING (err u4))

;; Define time credit token
(define-fungible-token time-credit)

;; Data Maps
(define-map user-profiles
    principal
    {
        name: (string-utf8 50),
        skills: (string-utf8 200),
        reliability-rating: uint,
        skill-rating: uint,
        communication-rating: uint,
        reviews-count: uint
    }
)

(define-map service-requests 
    uint 
    {
        requestor: principal,
        title: (string-utf8 100),
        description: (string-utf8 500),
        category: (string-utf8 50),
        hours-offered: uint,
        status: (string-ascii 20),
        provider: (optional principal)
    }
)

(define-data-var request-nonce uint u0)

;; Register new user
(define-public (register-user (name (string-utf8 50)) (skills (string-utf8 200)))
    (ok (map-set user-profiles tx-sender {
        name: name,
        skills: skills,
        reliability-rating: u0,
        skill-rating: u0,
        communication-rating: u0,
        reviews-count: u0
    }))
)

;; Create service request
(define-public (create-request (title (string-utf8 100)) (description (string-utf8 500)) (category (string-utf8 50)) (hours uint))
    (let
        ((request-id (var-get request-nonce)))
        (try! (ft-get-balance time-credit tx-sender))
        (map-set service-requests request-id {
            requestor: tx-sender,
            title: title,
            description: description,
            category: category,
            hours-offered: hours,
            status: "OPEN",
            provider: none
        })
        (var-set request-nonce (+ request-id u1))
        (ok request-id)
    )
)

;; Accept service request
(define-public (accept-request (request-id uint))
    (let ((request (unwrap! (map-get? service-requests request-id) ERR_NOT_FOUND)))
        (if (is-eq (get status request) "OPEN")
            (begin
                (map-set service-requests request-id 
                    (merge request {
                        status: "ACCEPTED",
                        provider: (some tx-sender)
                    })
                )
                (ok true)
            )
            ERR_UNAUTHORIZED
        )
    )
)

;; Complete service and transfer time credits
(define-public (complete-service (request-id uint))
    (let (
        (request (unwrap! (map-get? service-requests request-id) ERR_NOT_FOUND))
        (hours (get hours-offered request))
        (requestor (get requestor request))
    )
        (asserts! (is-eq (some tx-sender) (get provider request)) ERR_UNAUTHORIZED)
        (try! (ft-transfer? time-credit hours requestor tx-sender))
        (map-set service-requests request-id 
            (merge request { status: "COMPLETED" })
        )
        (ok true)
    )
)

;; Mint initial time credits for new users
(define-public (mint-initial-credits (recipient principal))
    ;; Give 10 hours of initial credits
    (ft-mint? time-credit u10 recipient)
)

;; Rate service provider with multiple dimensions
(define-public (rate-provider-detailed (provider principal) (reliability uint) (skill uint) (communication uint))
    (let (
        (profile (unwrap! (map-get? user-profiles provider) ERR_NOT_FOUND))
    )
        ;; Ratings should be between 1 and 5
        (asserts! (and (>= reliability u1) (<= reliability u5)) ERR_INVALID_RATING)
        (asserts! (and (>= skill u1) (<= skill u5)) ERR_INVALID_RATING)
        (asserts! (and (>= communication u1) (<= communication u5)) ERR_INVALID_RATING)
        
        (map-set user-profiles provider
            (merge profile {
                reliability-rating: (+ (get reliability-rating profile) reliability),
                skill-rating: (+ (get skill-rating profile) skill),
                communication-rating: (+ (get communication-rating profile) communication),
                reviews-count: (+ (get reviews-count profile) u1)
            })
        )
        (ok true)
    )
)

;; Legacy rate provider function for backwards compatibility
(define-public (rate-provider (provider principal) (rating uint))
    ;; This function now calls the detailed rating with the same value for all dimensions
    (rate-provider-detailed provider rating rating rating)
)

;; Read-only functions
(define-read-only (get-user-profile (user principal))
    (map-get? user-profiles user)
)

(define-read-only (get-request (request-id uint))
    (map-get? service-requests request-id)
)

;; Get all rating aspects for a provider
(define-read-only (get-provider-detailed-rating (provider principal))
    (let (
        (profile (unwrap! (map-get? user-profiles provider) ERR_NOT_FOUND))
        (reviews (get reviews-count profile))
    )
        (if (> reviews u0)
            (ok {
                reliability: (/ (get reliability-rating profile) reviews),
                skill: (/ (get skill-rating profile) reviews),
                communication: (/ (get communication-rating profile) reviews),
                reviews-count: reviews
            })
            (ok {
                reliability: u0,
                skill: u0,
                communication: u0,
                reviews-count: u0
            })
        )
    )
)

;; Legacy function for backwards compatibility
(define-read-only (get-provider-rating (provider principal))
    (let (
        (profile (unwrap! (map-get? user-profiles provider) ERR_NOT_FOUND))
        (reviews (get reviews-count profile))
    )
        (if (> reviews u0)
            (ok (/ (+ (get reliability-rating profile) (get skill-rating profile) (get communication-rating profile)) (* reviews u3)))
            (ok u0)
        )
    )
)

;; Get service requests by category
(define-read-only (get-requests-by-category (category (string-utf8 50)))
    (filter get-requests-of-category (map-to-list service-requests))
    
    (define-private (map-to-list (map-name (map uint {requestor: principal, title: (string-utf8 100), description: (string-utf8 500), category: (string-utf8 50), hours-offered: uint, status: (string-ascii 20), provider: (optional principal)})))
        (unwrap-panic (map-keys map-name))
    )
    
    (define-private (get-requests-of-category (request-id uint))
        (let ((request (unwrap-panic (map-get? service-requests request-id))))
            (is-eq (get category request) category)
        )
    )
)
```