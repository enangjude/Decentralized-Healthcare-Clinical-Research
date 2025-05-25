;; Protocol Registration Contract
;; Records and manages research methodologies and protocols

(define-constant ERR-NOT-AUTHORIZED (err u200))
(define-constant ERR-NOT-FOUND (err u201))
(define-constant ERR-INVALID-STATUS (err u202))
(define-constant ERR-PROTOCOL-EXISTS (err u203))

;; Protocol registry
(define-map research-protocols
  { protocol-id: uint }
  {
    institution-id: uint,
    title: (string-ascii 200),
    description: (string-ascii 500),
    methodology: (string-ascii 1000),
    registration-date: uint,
    status: (string-ascii 20),
    principal-investigator: principal,
    estimated-duration: uint,
    participant-count: uint
  }
)

;; Protocol counter
(define-data-var protocol-counter uint u0)

;; Protocol amendments
(define-map protocol-amendments
  { protocol-id: uint, amendment-id: uint }
  {
    description: (string-ascii 500),
    amendment-date: uint,
    amended-by: principal
  }
)

;; Amendment counter per protocol
(define-map amendment-counters uint uint)

;; Read-only functions
(define-read-only (get-protocol (protocol-id uint))
  (map-get? research-protocols { protocol-id: protocol-id })
)

(define-read-only (get-amendment (protocol-id uint) (amendment-id uint))
  (map-get? protocol-amendments { protocol-id: protocol-id, amendment-id: amendment-id })
)

(define-read-only (get-amendment-count (protocol-id uint))
  (default-to u0 (map-get? amendment-counters protocol-id))
)

;; Public functions
(define-public (register-protocol
  (institution-id uint)
  (title (string-ascii 200))
  (description (string-ascii 500))
  (methodology (string-ascii 1000))
  (estimated-duration uint)
  (participant-count uint)
)
  (let ((protocol-id (+ (var-get protocol-counter) u1)))
    (begin
      (var-set protocol-counter protocol-id)
      (map-set amendment-counters protocol-id u0)
      (ok (map-set research-protocols
        { protocol-id: protocol-id }
        {
          institution-id: institution-id,
          title: title,
          description: description,
          methodology: methodology,
          registration-date: block-height,
          status: "registered",
          principal-investigator: tx-sender,
          estimated-duration: estimated-duration,
          participant-count: participant-count
        }
      ))
    )
  )
)

(define-public (update-protocol-status (protocol-id uint) (new-status (string-ascii 20)))
  (match (map-get? research-protocols { protocol-id: protocol-id })
    protocol
      (begin
        (asserts! (is-eq tx-sender (get principal-investigator protocol)) ERR-NOT-AUTHORIZED)
        (ok (map-set research-protocols
          { protocol-id: protocol-id }
          (merge protocol { status: new-status })
        ))
      )
    ERR-NOT-FOUND
  )
)

(define-public (add-protocol-amendment (protocol-id uint) (description (string-ascii 500)))
  (match (map-get? research-protocols { protocol-id: protocol-id })
    protocol
      (let ((amendment-id (+ (get-amendment-count protocol-id) u1)))
        (begin
          (asserts! (is-eq tx-sender (get principal-investigator protocol)) ERR-NOT-AUTHORIZED)
          (map-set amendment-counters protocol-id amendment-id)
          (ok (map-set protocol-amendments
            { protocol-id: protocol-id, amendment-id: amendment-id }
            {
              description: description,
              amendment-date: block-height,
              amended-by: tx-sender
            }
          ))
        )
      )
    ERR-NOT-FOUND
  )
)
