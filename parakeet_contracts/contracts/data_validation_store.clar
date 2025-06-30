;; Advanced Storage with Data Validation Smart Contract
;; Enhanced contract with comprehensive features and functionality

;; Define error constants
(define-constant ERR-NOT-EVEN (err u100))
(define-constant ERR-UNAUTHORIZED (err u101))
(define-constant ERR-VALUE-TOO-LARGE (err u102))
(define-constant ERR-VALUE-TOO-SMALL (err u103))
(define-constant ERR-ALREADY-EXISTS (err u104))
(define-constant ERR-NOT-FOUND (err u105))
(define-constant ERR-INVALID-RANGE (err u106))
(define-constant ERR-CONTRACT-PAUSED (err u107))
(define-constant ERR-DAILY-LIMIT-EXCEEDED (err u108))
(define-constant ERR-COOLDOWN-ACTIVE (err u109))

;; Define constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant MAX-VALUE u1000000)
(define-constant MIN-VALUE u0)
(define-constant DAILY-LIMIT u10)
(define-constant COOLDOWN-BLOCKS u144) ;; ~24 hours assuming 10-minute blocks

;; Define data storage variables
(define-data-var stored-value uint u0)
(define-data-var contract-paused bool false)
(define-data-var total-operations uint u0)
(define-data-var validation-mode (string-ascii 20) "strict")

;; Define maps for enhanced functionality
(define-map authorized-users principal bool)
(define-map user-permissions principal { can-read: bool, can-write: bool, can-admin: bool })
(define-map value-history uint { value: uint, timestamp: uint, user: principal })
(define-map user-daily-operations principal { count: uint, last-reset: uint })
(define-map user-last-operation principal uint)
(define-map named-values (string-ascii 50) uint)
(define-map value-metadata uint { description: (string-ascii 200), tags: (list 5 (string-ascii 20)) })

;; Initialize the contract
(map-set authorized-users CONTRACT-OWNER true)
(map-set user-permissions CONTRACT-OWNER { can-read: true, can-write: true, can-admin: true })

;; Enhanced validation functions
(define-private (is-even (value uint))
  (is-eq (mod value u2) u0))

;; Simple square root approximation without recursion
(define-private (sqrt-approx (n uint))
  (if (<= n u1)
    n
    (if (<= n u4)
      u2
      (if (<= n u9)
        u3
        (if (<= n u16)
          u4
          (if (<= n u25)
            u5
            (if (<= n u36)
              u6
              (if (<= n u49)
                u7
                (if (<= n u64)
                  u8
                  (if (<= n u81)
                    u9
                    (if (<= n u100)
                      u10
                      (if (<= n u144)
                        u12
                        (if (<= n u196)
                          u14
                          (if (<= n u256)
                            u16
                            (if (<= n u324)
                              u18
                              (if (<= n u400)
                                u20
                                (if (<= n u625)
                                  u25
                                  (if (<= n u900)
                                    u30
                                    (if (<= n u1600)
                                      u40
                                      (if (<= n u2500)
                                        u50
                                        (if (<= n u10000)
                                          u100
                                          (if (<= n u40000)
                                            u200
                                            (if (<= n u160000)
                                              u400
                                              (if (<= n u1000000)
                                                u1000
                                                u1000))))))))))))))))))))))))

;; Simple prime checking without recursion - checks common small factors
(define-private (is-prime (n uint))
  (if (<= n u1)
    false
    (if (is-eq n u2)
      true
      (if (is-eq n u3)
        true
        (if (is-even n)
          false
          (if (is-eq (mod n u3) u0)
            false
            (if (is-eq (mod n u5) u0)
              (is-eq n u5)
              (if (is-eq (mod n u7) u0)
                (is-eq n u7)
                (if (is-eq (mod n u11) u0)
                  (is-eq n u11)
                  (if (is-eq (mod n u13) u0)
                    (is-eq n u13)
                    (if (is-eq (mod n u17) u0)
                      (is-eq n u17)
                      (if (is-eq (mod n u19) u0)
                        (is-eq n u19)
                        (if (is-eq (mod n u23) u0)
                          (is-eq n u23)
                          (if (> n u529) ;; 23^2 = 529
                            false ;; For large numbers, assume composite to avoid complexity
                            true))))))))))))))

;; Check if a number is a perfect square
(define-private (is-perfect-square (n uint))
  (let ((root (sqrt-approx n)))
    (is-eq (* root root) n)))

;; Check if a number is in fibonacci sequence
(define-private (is-fibonacci (n uint))
  (or (is-perfect-square (+ (* u5 n n) u4))
      (is-perfect-square (- (* u5 n n) u4))))

(define-private (validate-value-advanced (value uint))
  (let ((mode (var-get validation-mode)))
    (if (is-eq mode "strict")
      (validate-strict value)
      (if (is-eq mode "moderate")
        (validate-moderate value)
        (validate-basic value)))))

(define-private (validate-strict (value uint))
  (begin
    (asserts! (is-even value) ERR-NOT-EVEN)
    (asserts! (>= value MIN-VALUE) ERR-VALUE-TOO-SMALL)
    (asserts! (<= value MAX-VALUE) ERR-VALUE-TOO-LARGE)
    (asserts! (not (is-prime value)) (err u110)) ;; Strict mode: no primes
    (ok true)))

(define-private (validate-moderate (value uint))
  (begin
    (asserts! (is-even value) ERR-NOT-EVEN)
    (asserts! (>= value MIN-VALUE) ERR-VALUE-TOO-SMALL)
    (asserts! (<= value MAX-VALUE) ERR-VALUE-TOO-LARGE)
    (ok true)))

(define-private (validate-basic (value uint))
  (begin
    (asserts! (>= value MIN-VALUE) ERR-VALUE-TOO-SMALL)
    (asserts! (<= value MAX-VALUE) ERR-VALUE-TOO-LARGE)
    (ok true)))

;; Permission and authorization functions
(define-private (has-permission (user principal) (permission (string-ascii 10)))
  (match (map-get? user-permissions user)
    perms (if (is-eq permission "read")
            (get can-read perms)
            (if (is-eq permission "write")
              (get can-write perms)
              (get can-admin perms)))
    false))

(define-private (check-daily-limit (user principal))
  (let ((today (/ block-height u144))
        (user-ops (default-to { count: u0, last-reset: u0 } 
                               (map-get? user-daily-operations user))))
    (if (not (is-eq (get last-reset user-ops) today))
      (begin
        (map-set user-daily-operations user { count: u0, last-reset: today })
        (ok true))
      (if (< (get count user-ops) DAILY-LIMIT)
        (ok true)
        ERR-DAILY-LIMIT-EXCEEDED))))

(define-private (check-cooldown (user principal))
  (match (map-get? user-last-operation user)
    last-block (if (< (- block-height last-block) COOLDOWN-BLOCKS)
                 ERR-COOLDOWN-ACTIVE
                 (ok true))
    (ok true)))

(define-private (update-operation-tracking (user principal))
  (let ((today (/ block-height u144))
        (user-ops (default-to { count: u0, last-reset: u0 } 
                               (map-get? user-daily-operations user))))
    (if (not (is-eq (get last-reset user-ops) today))
      (map-set user-daily-operations user { count: u1, last-reset: today })
      (map-set user-daily-operations user 
               { count: (+ (get count user-ops) u1), last-reset: today }))
    (map-set user-last-operation user block-height)))

;; Core getter functions
(define-read-only (get-stored-value)
  (begin
    (asserts! (has-permission tx-sender "read") ERR-UNAUTHORIZED)
    (ok (var-get stored-value))))

(define-read-only (get-value-with-metadata)
  (let ((value (var-get stored-value)))
    (ok {
      value: value,
      metadata: (map-get? value-metadata value),
      is-even: (is-even value),
      is-prime: (is-prime value),
      is-fibonacci: (is-fibonacci value)
    })))

;; Enhanced setter functions
(define-public (set-stored-value (new-value uint))
  (begin
    (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)
    (asserts! (has-permission tx-sender "write") ERR-UNAUTHORIZED)
    (try! (check-daily-limit tx-sender))
    (try! (validate-value-advanced new-value))
    
    ;; Record history
    (let ((operation-id (var-get total-operations)))
      (map-set value-history operation-id {
        value: new-value,
        timestamp: block-height,
        user: tx-sender
      })
      (var-set total-operations (+ operation-id u1)))
    
    ;; Update tracking
    (update-operation-tracking tx-sender)
    (var-set stored-value new-value)
    (ok new-value)))

(define-public (set-value-with-metadata (new-value uint) 
                                       (description (string-ascii 200))
                                       (tags (list 5 (string-ascii 20))))
  (begin
    (try! (set-stored-value new-value))
    (map-set value-metadata new-value {
      description: description,
      tags: tags
    })
    (ok new-value)))

(define-public (batch-set-values (values (list 10 uint)))
  (begin
    (asserts! (has-permission tx-sender "write") ERR-UNAUTHORIZED)
    (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)
    (try! (fold check-and-validate values (ok (list))))
    (ok true)))

(define-private (check-and-validate (value uint) (acc (response (list 10 uint) uint)))
  (match acc
    success (match (validate-value-advanced value)
              valid (ok (unwrap-panic (as-max-len? (append success value) u10)))
              error (err error))
    error (err error)))

;; Named value storage
(define-public (set-named-value (name (string-ascii 50)) (value uint))
  (begin
    (asserts! (has-permission tx-sender "write") ERR-UNAUTHORIZED)
    (try! (validate-value-advanced value))
    (map-set named-values name value)
    (ok value)))

(define-read-only (get-named-value (name (string-ascii 50)))
  (ok (map-get? named-values name)))

;; Mathematical operations
(define-public (add-to-stored-value (amount uint))
  (let ((current (var-get stored-value)))
    (set-stored-value (+ current amount))))

(define-public (multiply-stored-value (factor uint))
  (let ((current (var-get stored-value)))
    (set-stored-value (* current factor))))

(define-public (apply-function (operation (string-ascii 20)) (operand uint))
  (let ((current (var-get stored-value)))
    (if (is-eq operation "add")
      (set-stored-value (+ current operand))
      (if (is-eq operation "multiply")
        (set-stored-value (* current operand))
        (if (is-eq operation "power")
          (set-stored-value (pow current operand))
          (err u200))))))

;; History and analytics
(define-read-only (get-value-history (operation-id uint))
  (ok (map-get? value-history operation-id)))

(define-read-only (get-user-statistics (user principal))
  (ok {
    daily-operations: (map-get? user-daily-operations user),
    last-operation: (map-get? user-last-operation user),
    permissions: (map-get? user-permissions user)
  }))

(define-read-only (get-contract-statistics)
  (ok {
    total-operations: (var-get total-operations),
    current-value: (var-get stored-value),
    validation-mode: (var-get validation-mode),
    is-paused: (var-get contract-paused)
  }))

;; Advanced queries
(define-read-only (find-values-in-range (min-val uint) (max-val uint))
  (begin
    (asserts! (<= min-val max-val) ERR-INVALID-RANGE)
    (ok (filter check-range-match (list u0 u2 u4 u6 u8 u10 u12 u14 u16 u18 u20)))))

(define-private (check-range-match (value uint))
  (and (>= value (var-get stored-value)) (<= value (var-get stored-value))))

;; Administrative functions
(define-public (set-validation-mode (mode (string-ascii 20)))
  (begin
    (asserts! (has-permission tx-sender "admin") ERR-UNAUTHORIZED)
    (var-set validation-mode mode)
    (ok mode)))

(define-public (pause-contract)
  (begin
    (asserts! (has-permission tx-sender "admin") ERR-UNAUTHORIZED)
    (var-set contract-paused true)
    (ok true)))

(define-public (unpause-contract)
  (begin
    (asserts! (has-permission tx-sender "admin") ERR-UNAUTHORIZED)
    (var-set contract-paused false)
    (ok true)))

(define-public (grant-permissions (user principal) 
                                 (can-read bool) 
                                 (can-write bool) 
                                 (can-admin bool))
  (begin
    (asserts! (has-permission tx-sender "admin") ERR-UNAUTHORIZED)
    (map-set user-permissions user {
      can-read: can-read,
      can-write: can-write,
      can-admin: can-admin
    })
    (ok true)))

(define-public (emergency-reset)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (var-set stored-value u0)
    (var-set contract-paused false)
    (var-set total-operations u0)
    (ok true)))

;; Utility and helper functions
(define-read-only (get-validation-info)
  {
    current-mode: (var-get validation-mode),
    rules: {
      strict: "Even numbers only, no primes, range 0-1000000",
      moderate: "Even numbers only, range 0-1000000", 
      basic: "Range 0-1000000 only"
    },
    limits: {
      max-value: MAX-VALUE,
      min-value: MIN-VALUE,
      daily-limit: DAILY-LIMIT,
      cooldown-blocks: COOLDOWN-BLOCKS
    }
  })

(define-read-only (check-value-properties (value uint))
  {
    is-even: (is-even value),
    is-prime: (is-prime value),
    is-fibonacci: (is-fibonacci value),
    passes-strict: (is-ok (validate-strict value)),
    passes-moderate: (is-ok (validate-moderate value)),
    passes-basic: (is-ok (validate-basic value))
  })

(define-read-only (get-system-status)
  {
    contract-paused: (var-get contract-paused),
    total-operations: (var-get total-operations),
    current-value: (var-get stored-value),
    validation-mode: (var-get validation-mode),
    current-block: block-height
  })