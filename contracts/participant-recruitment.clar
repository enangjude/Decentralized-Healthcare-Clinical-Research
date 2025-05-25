;; Participant Recruitment Contract
;; Manages study enrollment and participant consent

(define-constant ERR-NOT-AUTHORIZED (err u300))
(define-constant ERR-NOT-FOUND (err u301))
(define-constant ERR-ALREADY-ENROLLED (err u302))
(define-constant ERR-RECRUITMENT-CLOSED (err u303))
(define-constant ERR-INVALID-CONSENT (err u304))

;; Participant enrollment
(define-map study-participants
  { protocol-id: uint, participant-id: principal }
  {
    enrollment-date: uint,
    consent-hash: (string-ascii 64),
    status: (string-ascii 20),
    enrolled-by: principal
  }
)

;; Study recruitment status
(define-map recruitment-status
  { protocol-id: uint }
  {
    target-participants: uint,
    current-participants: uint,
    recruitment-open: bool,
    recruitment-start: uint,
    recruitment-end: uint
  }
)

;; Consent forms
(define-map consent-forms
  { protocol-id: uint }
  {
    form-hash: (string-ascii 64),
    version: uint,
    created-date: uint,
    created-by: principal
  }
)

;; Read-only functions
(define-read-only (get-participant-status (protocol-id uint) (participant principal))
  (map-get? study-participants { protocol-id: protocol-id, participant-id: participant })
)

(define-read-only (get-recruitment-info (protocol-id uint))
  (map-get? recruitment-status { protocol-id: protocol-id })
)

(define-read-only (get-consent-form (protocol-id uint))
  (map-get? consent-forms { protocol-id: protocol-id })
)

(define-read-only (is-recruitment-open (protocol-id uint))
  (match (map-get? recruitment-status { protocol-id: protocol-id })
    recruitment (get recruitment-open recruitment)
    false
  )
)

;; Public functions
(define-public (initialize-recruitment
  (protocol-id uint)
  (target-participants uint)
  (consent-form-hash (string-ascii 64))
)
  (begin
    (map-set recruitment-status
      { protocol-id: protocol-id }
      {
        target-participants: target-participants,
        current-participants: u0,
        recruitment-open: true,
        recruitment-start: block-height,
        recruitment-end: u0
      }
    )
    (ok (map-set consent-forms
      { protocol-id: protocol-id }
      {
        form-hash: consent-form-hash,
        version: u1,
        created-date: block-height,
        created-by: tx-sender
      }
    ))
  )
)

(define-public (enroll-participant (protocol-id uint) (consent-hash (string-ascii 64)))
  (let ((recruitment-info (unwrap! (map-get? recruitment-status { protocol-id: protocol-id }) ERR-NOT-FOUND)))
    (begin
      (asserts! (get recruitment-open recruitment-info) ERR-RECRUITMENT-CLOSED)
      (asserts! (is-none (map-get? study-participants { protocol-id: protocol-id, participant-id: tx-sender })) ERR-ALREADY-ENROLLED)

      ;; Update participant count
      (map-set recruitment-status
        { protocol-id: protocol-id }
        (merge recruitment-info { current-participants: (+ (get current-participants recruitment-info) u1) })
      )

      ;; Enroll participant
      (ok (map-set study-participants
        { protocol-id: protocol-id, participant-id: tx-sender }
        {
          enrollment-date: block-height,
          consent-hash: consent-hash,
          status: "enrolled",
          enrolled-by: tx-sender
        }
      ))
    )
  )
)

(define-public (withdraw-participant (protocol-id uint))
  (match (map-get? study-participants { protocol-id: protocol-id, participant-id: tx-sender })
    participant
      (ok (map-set study-participants
        { protocol-id: protocol-id, participant-id: tx-sender }
        (merge participant { status: "withdrawn" })
      ))
    ERR-NOT-FOUND
  )
)

(define-public (close-recruitment (protocol-id uint))
  (match (map-get? recruitment-status { protocol-id: protocol-id })
    recruitment
      (ok (map-set recruitment-status
        { protocol-id: protocol-id }
        (merge recruitment { recruitment-open: false, recruitment-end: block-height })
      ))
    ERR-NOT-FOUND
  )
)
