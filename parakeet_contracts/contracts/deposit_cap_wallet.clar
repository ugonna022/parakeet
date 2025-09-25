;; Deposit Cap Wallet Smart Contract
;; Limits maximum deposits per user and rejects deposits exceeding the cap

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-deposit-exceeds-cap (err u101))
(define-constant err-insufficient-balance (err u102))
(define-constant err-transfer-failed (err u103))

;; Data Variables
(define-data-var deposit-cap uint u1000000) ;; Default cap: 1,000,000 microSTX (1 STX)

;; Data Maps
(define-map user-deposits principal uint)
(define-map user-balances principal uint)

;; Read-only functions

;; Get the current deposit cap
(define-read-only (get-deposit-cap)
  (var-get deposit-cap)
)

;; Get user's total deposits
(define-read-only (get-user-deposits (user principal))
  (default-to u0 (map-get? user-deposits user))
)

;; Get user's current balance in the wallet
(define-read-only (get-user-balance (user principal))
  (default-to u0 (map-get? user-balances user))
)

;; Check if a deposit amount would exceed the cap for a user
(define-read-only (check-deposit-allowed (user principal) (amount uint))
  (let ((current-deposits (get-user-deposits user))
        (cap (var-get deposit-cap)))
    (<= (+ current-deposits amount) cap)
  )
)

;; Get remaining deposit capacity for a user
(define-read-only (get-remaining-capacity (user principal))
  (let ((current-deposits (get-user-deposits user))
        (cap (var-get deposit-cap)))
    (if (>= current-deposits cap)
        u0
        (- cap current-deposits)
    )
  )
)

;; Public functions

;; Deposit STX to the wallet (with cap check)
(define-public (deposit (amount uint))
  (let ((user tx-sender)
        (current-deposits (get-user-deposits user))
        (current-balance (get-user-balance user))
        (cap (var-get deposit-cap)))
    
    ;; Check if deposit would exceed cap
    (asserts! (check-deposit-allowed user amount) err-deposit-exceeds-cap)
    
    ;; Transfer STX from user to contract
    (match (stx-transfer? amount user (as-contract tx-sender))
      success (begin
        ;; Update user's total deposits
        (map-set user-deposits user (+ current-deposits amount))
        ;; Update user's balance in wallet
        (map-set user-balances user (+ current-balance amount))
        (ok {
          deposited: amount,
          total-deposits: (+ current-deposits amount),
          balance: (+ current-balance amount),
          remaining-capacity: (- cap (+ current-deposits amount))
        })
      )
      error err-transfer-failed
    )
  )
)

;; Withdraw STX from the wallet
(define-public (withdraw (amount uint))
  (let ((user tx-sender)
        (current-balance (get-user-balance user)))
    
    ;; Check if user has sufficient balance
    (asserts! (>= current-balance amount) err-insufficient-balance)
    
    ;; Transfer STX from contract to user
    (match (as-contract (stx-transfer? amount tx-sender user))
      success (begin
        ;; Update user's balance in wallet
        (map-set user-balances user (- current-balance amount))
        (ok {
          withdrawn: amount,
          remaining-balance: (- current-balance amount)
        })
      )
      error err-transfer-failed
    )
  )
)

;; Admin functions

;; Set new deposit cap (only contract owner)
(define-public (set-deposit-cap (new-cap uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set deposit-cap new-cap)
    (ok new-cap)
  )
)

;; Get contract info
(define-read-only (get-contract-info)
  {
    owner: contract-owner,
    deposit-cap: (var-get deposit-cap),
    contract-balance: (stx-get-balance (as-contract tx-sender))
  }
)