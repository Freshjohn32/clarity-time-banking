;; Time Banking System Contract
(define-constant ERR_UNAUTHORIZED (err u1))
(define-constant ERR_NOT_FOUND (err u2))
(define-constant ERR_INSUFFICIENT_HOURS (err u3))

;; Define time credit token
(define-fungible-token time-credit)

;; Data Maps
(define-map user-profiles
    principal
    {
        name: (string-utf8 50),
        skills: (string-utf8 200),
        rating: uint,
        reviews-count: uint
    }
)

(define-map service-requests 
    uint 
    {
        requestor: principal,
        title: (string-utf8 100),
        description: (string-utf8 500),
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
        rating: u0,
        reviews-count: u0
    }))
)

;; Create service request
(define-public (create-request (title (string-utf8 100)) (description (string-utf8 500)) (hours uint))
    (let
        ((request-id (var-get request-nonce)))
        (try! (ft-get-balance time-credit tx-sender))
        (map-set service-requests request-id {
            requestor: tx-sender,
            title: title,
            description: description,
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

;; Rate service provider
(define-public (rate-provider (provider principal) (rating uint))
    (let (
        (profile (unwrap! (map-get? user-profiles provider) ERR_NOT_FOUND))
        (current-rating (get rating profile))
        (reviews (get reviews-count profile))
    )
        (map-set user-profiles provider
            (merge profile {
                rating: (+ current-rating rating),
                reviews-count: (+ reviews u1)
            })
        )
        (ok true)
    )
)

;; Read-only functions
(define-read-only (get-user-profile (user principal))
    (map-get? user-profiles user)
)

(define-read-only (get-request (request-id uint))
    (map-get? service-requests request-id)
)

(define-read-only (get-provider-rating (provider principal))
    (let (
        (profile (unwrap! (map-get? user-profiles provider) ERR_NOT_FOUND))
    )
        (ok (/ (get rating profile) (get reviews-count profile)))
    )
)
