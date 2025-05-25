;; Data Collection Contract
;;

(define-constant ERR-NOT-AUTHORIZED (err u400))
(define-constant ERR-NOT-FOUND (err u401))
(define-constant ERR-INVALID-DATA (err u402))
(define-constant ERR-DATA-LOCKED (err u403))

;; Research data entries
(define-map research-data
  { protocol-id: uint, data-id: uint }
  {
    participant-hash: (string-ascii 64),
    data-hash: (string-ascii 64),
    collection-date: uint,
    data-type: (string-ascii 50),
    collected-by: principal,
    verified: bool,
    locked: bool
  }
)

;; Data counter per protocol
(define-map data-counters uint uint)

;; Data verification
(define-map data-verifications
  { protocol-id: uint, data-id: uint, verifier: principal }
  {
    verification-date: uint,
    verification-status: (string-ascii 20),
    notes: (string-ascii 200)
  }
)

;; Authorized data collectors per protocol
(define-map authorized-collectors
  { protocol-id: uint, collector: principal }
  bool
)

;; Read-only functions
(define-read-only (get-data-entry (protocol-id uint) (data-id uint))
  (map-get? research-data { protocol-id: protocol-id, data-id: data-id })
)

(define-read-only (get-data-count (protocol-id uint))
  (default-to u0 (map-get? data-counters protocol-id))
)

(define-read-only (is-authorized-collector (protocol-id uint) (collector principal))
  (default-to false (map-get? authorized-collectors { protocol-id: protocol-id, collector: collector }))
)

(define-read-only (get-verification (protocol-id uint) (data-id uint) (verifier principal))
  (map-get? data-verifications { protocol-id: protocol-id, data-id: data-id, verifier: verifier })
)

;; Public functions
(define-public (authorize-collector (protocol-id uint) (collector principal))
  (ok (map-set authorized-collectors { protocol-id: protocol-id, collector: collector } true))
)

(define-public (collect-data
  (protocol-id uint)
  (participant-hash (string-ascii 64))
  (data-hash (string-ascii 64))
  (data-type (string-ascii 50))
)
  (let ((data-id (+ (get-data-count protocol-id) u1)))
    (begin
      (asserts! (is-authorized-collector protocol-id tx-sender) ERR-NOT-AUTHORIZED)
      (map-set data-counters protocol-id data-id)
      (ok (map-set research-data
        { protocol-id: protocol-id, data-id: data-id }
        {
          participant-hash: participant-hash,
          data-hash: data-hash,
          collection-date: block-height,
          data-type: data-type,
          collected-by: tx-sender,
          verified: false,
          locked: false
        }
      ))
    )
  )
)

(define-public (verify-data
  (protocol-id uint)
  (data-id uint)
  (verification-status (string-ascii 20))
  (notes (string-ascii 200))
)
  (match (map-get? research-data { protocol-id: protocol-id, data-id: data-id })
    data-entry
      (begin
        (asserts! (not (get locked data-entry)) ERR-DATA-LOCKED)
        (map-set data-verifications
          { protocol-id: protocol-id, data-id: data-id, verifier: tx-sender }
          {
            verification-date: block-height,
            verification-status: verification-status,
            notes: notes
          }
        )
        (ok (map-set research-data
          { protocol-id: protocol-id, data-id: data-id }
          (merge data-entry { verified: (is-eq verification-status "approved") })
        ))
      )
    ERR-NOT-FOUND
  )
)

(define-public (lock-data (protocol-id uint) (data-id uint))
  (match (map-get? research-data { protocol-id: protocol-id, data-id: data-id })
    data-entry
      (begin
        (asserts! (is-authorized-collector protocol-id tx-sender) ERR-NOT-AUTHORIZED)
        (ok (map-set research-data
          { protocol-id: protocol-id, data-id: data-id }
          (merge data-entry { locked: true })
        ))
      )
    ERR-NOT-FOUND
  )
)
