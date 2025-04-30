;; =========================================================================
;; Service Verification Contract
;; =========================================================================
;;
;; This contract enables verification of service completion through a
;; multi-signature confirmation system. It supports:
;; - Registration of service agreements between providers and recipients
;; - Optional third-party verification
;; - Time tracking for service delivery
;; - Partial completion acknowledgment
;; - Multiple verification states (pending, verified, disputed)
;;
;; The contract serves as a trust layer for time-banking applications,
;; providing reliable verification of service exchange completion.
;; =========================================================================

;; =========================================================================
;; Error codes
;; =========================================================================
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-SERVICE-NOT-FOUND (err u101))
(define-constant ERR-ALREADY-VERIFIED (err u102))
(define-constant ERR-ALREADY-REGISTERED (err u103))
(define-constant ERR-INVALID-HOURS (err u104))
(define-constant ERR-RECIPIENT-IS-PROVIDER (err u105))
(define-constant ERR-UNKNOWN-VERIFICATION-STATUS (err u106))
(define-constant ERR-VERIFICATION-NOT-PENDING (err u107))
(define-constant ERR-THIRD-PARTY-ALREADY-SET (err u108))
(define-constant ERR-THIRD-PARTY-REQUIRED (err u109))
(define-constant ERR-SELF-VERIFICATION (err u110))

;; =========================================================================
;; Data maps and variables
;; =========================================================================

;; Status enum values:
;; 0 - pending
;; 1 - verified
;; 2 - disputed
;; 3 - partially-verified

;; Main service registry with details about each service
(define-map services
  { service-id: uint }
  {
    provider: principal,
    recipient: principal,
    description: (string-utf8 256),
    expected-hours: uint,
    actual-hours: uint,
    third-party-verifier: (optional principal),
    verification-status: uint,
    provider-confirmed: bool,
    recipient-confirmed: bool,
    third-party-confirmed: bool,
    timestamp: uint
  }
)

;; Counter to generate unique service IDs
(define-data-var next-service-id uint u1)

;; =========================================================================
;; Private functions
;; =========================================================================

;; Get the current service ID and increment for the next use
(define-private (get-and-increment-service-id)
  (let ((current-id (var-get next-service-id)))
    (var-set next-service-id (+ current-id u1))
    current-id
  )
)

;; Check if the principal is either the provider or recipient
(define-private (is-service-participant (service-id uint) (participant principal))
  (match (map-get? services { service-id: service-id })
    service (or
              (is-eq (get provider service) participant)
              (is-eq (get recipient service) participant)
            )
    false
  )
)

;; Check if the service is in pending status
(define-private (is-service-pending (service-id uint))
  (match (map-get? services { service-id: service-id })
    service (is-eq (get verification-status service) u0)
    false
  )
)

;; Update the verification status based on confirmation state
(define-private (update-verification-status (service-id uint))
  (match (map-get? services { service-id: service-id })
    service
      (let
        (
          (provider-confirmed (get provider-confirmed service))
          (recipient-confirmed (get recipient-confirmed service))
          (third-party-confirmed (get third-party-confirmed service))
          (third-party-verifier (get third-party-verifier service))
          (expected-hours (get expected-hours service))
          (actual-hours (get actual-hours service))
        )
        ;; Different verification logic depending on whether a third party is involved
        (if (is-some third-party-verifier)
          ;; With third-party: all three must confirm
          (if (and provider-confirmed recipient-confirmed third-party-confirmed)
            (if (< actual-hours expected-hours)
              (map-set services { service-id: service-id }
                (merge service { verification-status: u3 })) ;; partially-verified
              (map-set services { service-id: service-id }
                (merge service { verification-status: u1 })) ;; verified
            )
            service
          )
          ;; Without third-party: just provider and recipient must confirm
          (if (and provider-confirmed recipient-confirmed)
            (if (< actual-hours expected-hours)
              (map-set services { service-id: service-id }
                (merge service { verification-status: u3 })) ;; partially-verified
              (map-set services { service-id: service-id }
                (merge service { verification-status: u1 })) ;; verified
            )
            service
          )
        )
      )
    service-id
  )
  (ok true)
)

;; =========================================================================
;; Read-only functions
;; =========================================================================

;; Get details of a specific service
(define-read-only (get-service (service-id uint))
  (match (map-get? services { service-id: service-id })
    service (ok service)
    ERR-SERVICE-NOT-FOUND
  )
)

;; Check if a principal is authorized to confirm a service
(define-read-only (can-confirm-service (service-id uint) (confirmer principal))
  (match (map-get? services { service-id: service-id })
    service
      (or
        (is-eq (get provider service) confirmer)
        (is-eq (get recipient service) confirmer)
        (match (get third-party-verifier service)
          third-party (is-eq third-party confirmer)
          false
        )
      )
    false
  )
)

;; Get verification status as a string
(define-read-only (get-verification-status-string (status-code uint))
  (match status-code
    u0 "pending"
    u1 "verified"
    u2 "disputed"
    u3 "partially-verified"
    ERR-UNKNOWN-VERIFICATION-STATUS
  )
)

;; =========================================================================
;; Public functions
;; =========================================================================

;; Register a new service agreement
(define-public (register-service 
  (recipient principal)
  (description (string-utf8 256))
  (expected-hours uint)
  (third-party-verifier (optional principal))
)
  (let
    (
      (provider tx-sender)
      (service-id (get-and-increment-service-id))
      (current-time-ms (unwrap-panic (get-block-info? time (- block-height u1))))
    )
    
    ;; Basic validation
    (asserts! (> expected-hours u0) ERR-INVALID-HOURS)
    (asserts! (not (is-eq provider recipient)) ERR-RECIPIENT-IS-PROVIDER)
    
    ;; Validate third-party isn't the provider or recipient
    (match third-party-verifier
      verifier (asserts! (and 
                          (not (is-eq verifier provider))
                          (not (is-eq verifier recipient))
                        ) ERR-SELF-VERIFICATION)
      true
    )
    
    ;; Create the service record
    (map-set services
      { service-id: service-id }
      {
        provider: provider,
        recipient: recipient,
        description: description,
        expected-hours: expected-hours,
        actual-hours: u0,
        third-party-verifier: third-party-verifier,
        verification-status: u0, ;; pending
        provider-confirmed: false,
        recipient-confirmed: false,
        third-party-confirmed: false,
        timestamp: current-time-ms
      }
    )
    
    (ok service-id)
  )
)

;; Add a third-party verifier to an existing service
(define-public (add-third-party-verifier (service-id uint) (verifier principal))
  (let
    (
      (sender tx-sender)
    )
    (match (map-get? services { service-id: service-id })
      service
        (begin
          ;; Authorization check
          (asserts! (is-eq (get provider service) sender) ERR-UNAUTHORIZED)
          
          ;; Verify service is still pending
          (asserts! (is-eq (get verification-status service) u0) ERR-VERIFICATION-NOT-PENDING)
          
          ;; Verify no third-party is already set
          (asserts! (is-none (get third-party-verifier service)) ERR-THIRD-PARTY-ALREADY-SET)
          
          ;; Verify third-party isn't the provider or recipient
          (asserts! (and 
                      (not (is-eq verifier (get provider service)))
                      (not (is-eq verifier (get recipient service)))
                    ) ERR-SELF-VERIFICATION)
          
          ;; Update the service with the third-party
          (map-set services
            { service-id: service-id }
            (merge service { third-party-verifier: (some verifier) })
          )
          
          (ok true)
        )
      ERR-SERVICE-NOT-FOUND
    )
  )
)

;; Confirm service completion as provider
(define-public (confirm-as-provider (service-id uint) (actual-hours uint))
  (let
    (
      (sender tx-sender)
    )
    (match (map-get? services { service-id: service-id })
      service
        (begin
          ;; Authorization check
          (asserts! (is-eq (get provider service) sender) ERR-UNAUTHORIZED)
          
          ;; Verify service is still pending
          (asserts! (is-service-pending service-id) ERR-VERIFICATION-NOT-PENDING)
          
          ;; Verify actual hours are valid
          (asserts! (and 
                      (> actual-hours u0)
                      (<= actual-hours (get expected-hours service))
                    ) ERR-INVALID-HOURS)
          
          ;; Update the service with provider confirmation
          (map-set services
            { service-id: service-id }
            (merge service
              {
                provider-confirmed: true,
                actual-hours: actual-hours
              }
            )
          )
          
          (update-verification-status service-id)
        )
      ERR-SERVICE-NOT-FOUND
    )
  )
)

;; Confirm service completion as recipient
(define-public (confirm-as-recipient (service-id uint))
  (let
    (
      (sender tx-sender)
    )
    (match (map-get? services { service-id: service-id })
      service
        (begin
          ;; Authorization check
          (asserts! (is-eq (get recipient service) sender) ERR-UNAUTHORIZED)
          
          ;; Verify service is still pending
          (asserts! (is-service-pending service-id) ERR-VERIFICATION-NOT-PENDING)
          
          ;; Update the service with recipient confirmation
          (map-set services
            { service-id: service-id }
            (merge service { recipient-confirmed: true })
          )
          
          (update-verification-status service-id)
        )
      ERR-SERVICE-NOT-FOUND
    )
  )
)

;; Confirm service completion as third-party verifier
(define-public (confirm-as-third-party (service-id uint))
  (let
    (
      (sender tx-sender)
    )
    (match (map-get? services { service-id: service-id })
      service
        (begin
          ;; Authorization check
          (match (get third-party-verifier service)
            verifier (asserts! (is-eq verifier sender) ERR-UNAUTHORIZED)
            ERR-THIRD-PARTY-REQUIRED
          )
          
          ;; Verify service is still pending
          (asserts! (is-service-pending service-id) ERR-VERIFICATION-NOT-PENDING)
          
          ;; Update the service with third-party confirmation
          (map-set services
            { service-id: service-id }
            (merge service { third-party-confirmed: true })
          )
          
          (update-verification-status service-id)
        )
      ERR-SERVICE-NOT-FOUND
    )
  )
)

;; Dispute a service verification
(define-public (dispute-service (service-id uint))
  (let
    (
      (sender tx-sender)
    )
    (match (map-get? services { service-id: service-id })
      service
        (begin
          ;; Authorization check - only provider or recipient can dispute
          (asserts! (or
                      (is-eq (get provider service) sender)
                      (is-eq (get recipient service) sender)
                    ) ERR-UNAUTHORIZED)
          
          ;; Can only dispute a pending service
          (asserts! (is-service-pending service-id) ERR-VERIFICATION-NOT-PENDING)
          
          ;; Update the service as disputed
          (map-set services
            { service-id: service-id }
            (merge service { verification-status: u2 }) ;; disputed
          )
          
          (ok true)
        )
      ERR-SERVICE-NOT-FOUND
    )
  )
)