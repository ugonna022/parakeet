;; Multi-Send with Multi-Signature Smart Contract
;; Enhanced security for batch fund transfers

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-THRESHOLD (err u101))
(define-constant ERR-INSUFFICIENT-SIGNATURES (err u102))
(define-constant ERR-TRANSACTION-NOT-FOUND (err u103))
(define-constant ERR-ALREADY-SIGNED (err u104))
(define-constant ERR-ALREADY-EXECUTED (err u105))
(define-constant ERR-INVALID-RECIPIENT (err u106))
(define-constant ERR-INSUFFICIENT-BALANCE (err u107))
(define-constant ERR-TRANSFER-FAILED (err u108))
(define-constant ERR-INVALID-AMOUNT (err u109))

;; Contract owner
(define-constant CONTRACT-OWNER tx-sender)

;; Multi-signature configuration
(define-data-var signature-threshold uint u2)
(define-data-var total-signers uint u0)

;; Transaction counter for unique IDs
(define-data-var transaction-nonce uint u0)

;; Authorized signers list
(define-map signers principal bool)

;; Multi-send transaction structure
(define-map transactions
  uint
  {
    initiator: principal,
    recipients: (list 20 {recipient: principal, amount: uint}),
    total-amount: uint,
    executed: bool,
    created-at: uint,
    signatures-count: uint
  }
)

;; Track signatures for each transaction
(define-map transaction-signatures
  {tx-id: uint, signer: principal}
  bool
)

;; Initialize contract with initial signers and threshold
(define-public (initialize (initial-signers (list 10 principal)) (threshold uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (and (> threshold u0) (<= threshold (len initial-signers))) ERR-INVALID-THRESHOLD)
    
    ;; Add initial signers
    (map add-signer-internal initial-signers)
    
    ;; Set configuration
    (var-set signature-threshold threshold)
    (var-set total-signers (len initial-signers))
    
    (ok true)
  )
)

;; Internal function to add a signer
(define-private (add-signer-internal (signer principal))
  (map-set signers signer true)
)

;; Add a new signer (only contract owner)
(define-public (add-signer (new-signer principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set signers new-signer true)
    (var-set total-signers (+ (var-get total-signers) u1))
    (ok true)
  )
)

;; Remove a signer (only contract owner)
(define-public (remove-signer (signer principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-delete signers signer)
    (var-set total-signers (- (var-get total-signers) u1))
    (ok true)
  )
)

;; Update signature threshold (only contract owner)
(define-public (update-threshold (new-threshold uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (and (> new-threshold u0) (<= new-threshold (var-get total-signers))) ERR-INVALID-THRESHOLD)
    (var-set signature-threshold new-threshold)
    (ok true)
  )
)

;; Create a new multi-send transaction
(define-public (create-multi-send-transaction (recipients (list 20 {recipient: principal, amount: uint})))
  (let
    (
      (tx-id (+ (var-get transaction-nonce) u1))
      (total-amount (fold + (map get-amount recipients) u0))
    )
    
    ;; Validate inputs
    (asserts! (default-to false (map-get? signers tx-sender)) ERR-NOT-AUTHORIZED)
    (asserts! (> (len recipients) u0) ERR-INVALID-RECIPIENT)
    (asserts! (> total-amount u0) ERR-INVALID-AMOUNT)
    (asserts! (validate-recipients recipients) ERR-INVALID-RECIPIENT)
    
    ;; Check contract balance
    (asserts! (>= (stx-get-balance (as-contract tx-sender)) total-amount) ERR-INSUFFICIENT-BALANCE)
    
    ;; Create transaction record
    (map-set transactions tx-id
      {
        initiator: tx-sender,
        recipients: recipients,
        total-amount: total-amount,
        executed: false,
        created-at: block-height,
        signatures-count: u0
      }
    )
    
    ;; Update nonce
    (var-set transaction-nonce tx-id)
    
    (ok tx-id)
  )
)

;; Helper function to extract amount from recipient tuple
(define-private (get-amount (recipient-data {recipient: principal, amount: uint}))
  (get amount recipient-data)
)

;; Validate all recipients and amounts
(define-private (validate-recipients (recipients (list 20 {recipient: principal, amount: uint})))
  (fold validate-recipient-and recipients true)
)

;; Validate individual recipient
(define-private (validate-recipient-and (recipient-data {recipient: principal, amount: uint}) (prev-valid bool))
  (and prev-valid 
       (not (is-eq (get recipient recipient-data) (as-contract tx-sender)))
       (> (get amount recipient-data) u0))
)

;; Sign a transaction
(define-public (sign-transaction (tx-id uint))
  (let
    (
      (transaction (unwrap! (map-get? transactions tx-id) ERR-TRANSACTION-NOT-FOUND))
      (signature-key {tx-id: tx-id, signer: tx-sender})
    )
    
    ;; Validate signer
    (asserts! (default-to false (map-get? signers tx-sender)) ERR-NOT-AUTHORIZED)
    
    ;; Check if transaction exists and not executed
    (asserts! (not (get executed transaction)) ERR-ALREADY-EXECUTED)
    
    ;; Check if already signed
    (asserts! (not (default-to false (map-get? transaction-signatures signature-key))) ERR-ALREADY-SIGNED)
    
    ;; Add signature
    (map-set transaction-signatures signature-key true)
    
    ;; Update signature count
    (map-set transactions tx-id
      (merge transaction {signatures-count: (+ (get signatures-count transaction) u1)})
    )
    
    (ok true)
  )
)

;; Execute multi-send transaction after sufficient signatures
(define-public (execute-transaction (tx-id uint))
  (let
    (
      (transaction (unwrap! (map-get? transactions tx-id) ERR-TRANSACTION-NOT-FOUND))
    )
    
    ;; Validate execution conditions
    (asserts! (default-to false (map-get? signers tx-sender)) ERR-NOT-AUTHORIZED)
    (asserts! (not (get executed transaction)) ERR-ALREADY-EXECUTED)
    (asserts! (>= (get signatures-count transaction) (var-get signature-threshold)) ERR-INSUFFICIENT-SIGNATURES)
    
    ;; Check contract still has sufficient balance
    (asserts! (>= (stx-get-balance (as-contract tx-sender)) (get total-amount transaction)) ERR-INSUFFICIENT-BALANCE)
    
    ;; Execute transfers
    (unwrap! (execute-transfers (get recipients transaction)) ERR-TRANSFER-FAILED)
    
    ;; Mark as executed
    (map-set transactions tx-id
      (merge transaction {executed: true})
    )
    
    (ok true)
  )
)

;; Execute all transfers in the batch
(define-private (execute-transfers (recipients (list 20 {recipient: principal, amount: uint})))
  (fold execute-single-transfer recipients (ok true))
)

;; Execute a single transfer
(define-private (execute-single-transfer 
  (recipient-data {recipient: principal, amount: uint}) 
  (prev-result (response bool uint)))
  (match prev-result
    success (as-contract (stx-transfer? (get amount recipient-data) tx-sender (get recipient recipient-data)))
    error (err error)
  )
)

;; Deposit STX to contract (for funding multi-sends)
(define-public (deposit (amount uint))
  (stx-transfer? amount tx-sender (as-contract tx-sender))
)

;; Emergency withdrawal (only contract owner)
(define-public (emergency-withdraw (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (as-contract (stx-transfer? amount tx-sender CONTRACT-OWNER))
  )
)

;; Read-only functions

;; Get transaction details
(define-read-only (get-transaction (tx-id uint))
  (map-get? transactions tx-id)
)

;; Check if address is authorized signer
(define-read-only (is-signer (address principal))
  (default-to false (map-get? signers address))
)

;; Get signature threshold
(define-read-only (get-signature-threshold)
  (var-get signature-threshold)
)

;; Get total number of signers
(define-read-only (get-total-signers)
  (var-get total-signers)
)

;; Check if transaction is signed by specific signer
(define-read-only (has-signed (tx-id uint) (signer principal))
  (default-to false (map-get? transaction-signatures {tx-id: tx-id, signer: signer}))
)

;; Get contract balance
(define-read-only (get-contract-balance)
  (stx-get-balance (as-contract tx-sender))
)

;; Get current transaction nonce
(define-read-only (get-current-nonce)
  (var-get transaction-nonce)
)