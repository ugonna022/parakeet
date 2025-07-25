;; Enhanced Voting Smart Contract with Advanced Features
;; Comprehensive voting system with receipts, delegation, multi-round voting, and governance

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_VOTING_ENDED (err u101))
(define-constant ERR_VOTING_NOT_STARTED (err u102))
(define-constant ERR_ALREADY_VOTED (err u103))
(define-constant ERR_INVALID_CANDIDATE (err u104))
(define-constant ERR_VOTE_NOT_FOUND (err u105))
(define-constant ERR_INSUFFICIENT_STAKE (err u106))
(define-constant ERR_ALREADY_DELEGATED (err u107))
(define-constant ERR_SELF_DELEGATION (err u108))
(define-constant ERR_INVALID_PROPOSAL (err u109))
(define-constant ERR_VOTING_ROUND_ENDED (err u110))
(define-constant ERR_WHITELIST_REQUIRED (err u111))
(define-constant ERR_BLACKLISTED (err u112))
(define-constant ERR_COOLDOWN_ACTIVE (err u113))
(define-constant ERR_QUORUM_NOT_MET (err u114))
(define-constant ERR_INVALID_VOTE_WEIGHT (err u115))

;; Voting Types
(define-constant VOTE_TYPE_SINGLE u1)
(define-constant VOTE_TYPE_MULTIPLE u2)
(define-constant VOTE_TYPE_RANKED u3)
(define-constant VOTE_TYPE_WEIGHTED u4)

;; Data Variables
(define-data-var voting-active bool false)
(define-data-var voting-start-block uint u0)
(define-data-var voting-end-block uint u0)
(define-data-var total-votes uint u0)
(define-data-var current-round uint u1)
(define-data-var voting-type uint VOTE_TYPE_SINGLE)
(define-data-var min-stake-required uint u0)
(define-data-var quorum-threshold uint u0)
(define-data-var use-whitelist bool false)
(define-data-var emergency-stop bool false)
(define-data-var vote-reveal-phase bool false)
(define-data-var voting-fee uint u0)
(define-data-var max-candidates-per-vote uint u1)

;; Data Maps
(define-map candidates 
  principal 
  {
    votes: uint,
    description: (string-ascii 256),
    active: bool,
    stake: uint
  }
)

(define-map voter-receipts principal (list 10 (buff 32)))
(define-map vote-records 
  (buff 32) 
  {
    voter: principal,
    candidate: principal,
    block-height: uint,
    timestamp: uint,
    weight: uint,
    round: uint,
    vote-type: uint
  }
)

(define-map voter-status 
  principal 
  {
    voted: bool,
    round: uint,
    vote-count: uint,
    last-vote-block: uint
  }
)

;; Vote Delegation System
(define-map vote-delegations 
  principal 
  {
    delegate: principal,
    weight: uint,
    active: bool
  }
)

(define-map delegate-power 
  principal 
  {
    total-weight: uint,
    delegator-count: uint
  }
)

;; Governance and Proposals
(define-map proposals 
  uint 
  {
    title: (string-ascii 256),
    description: (string-ascii 1024),
    proposer: principal,
    votes-for: uint,
    votes-against: uint,
    start-block: uint,
    end-block: uint,
    executed: bool,
    proposal-type: uint
  }
)

(define-data-var proposal-counter uint u0)

;; Access Control
(define-map voter-whitelist principal bool)
(define-map voter-blacklist principal bool)
(define-map admin-roles principal uint) ;; 1=admin, 2=moderator

;; Staking System
(define-map voter-stakes principal uint)
(define-map candidate-stakes principal uint)

;; Commit-Reveal Voting
(define-map vote-commits 
  principal 
  {
    commit-hash: (buff 32),
    revealed: bool,
    commit-block: uint
  }
)

;; Multi-round Voting
(define-map round-results 
  uint 
  {
    winner: (optional principal),
    total-votes: uint,
    ended: bool
  }
)

;; Vote History and Analytics
(define-map voting-history 
  principal 
  (list 50 {
    round: uint,
    candidate: principal,
    timestamp: uint,
    weight: uint
  })
)

;; Private Functions

;; Helper function for minimum of two numbers
(define-private (min-uint (a uint) (b uint))
  (if (<= a b) a b)
)

;; Generate enhanced vote receipt with more metadata
(define-private (generate-vote-receipt (voter principal) (candidate principal) (weight uint) (round uint))
  (keccak256 
    (concat 
      (concat 
        (concat (unwrap-panic (to-consensus-buff? voter)) (unwrap-panic (to-consensus-buff? candidate)))
        (concat (unwrap-panic (to-consensus-buff? weight)) (unwrap-panic (to-consensus-buff? round)))
      )
      (concat (unwrap-panic (to-consensus-buff? block-height)) (unwrap-panic (to-consensus-buff? stx-liquid-supply)))
    )
  )
)

;; Check if voting is currently active
(define-private (is-voting-active)
  (and 
    (var-get voting-active)
    (>= block-height (var-get voting-start-block))
    (<= block-height (var-get voting-end-block))
    (not (var-get emergency-stop))
  )
)

;; Calculate vote weight based on stake and delegation
(define-private (calculate-vote-weight (voter principal))
  (let 
    (
      (base-weight u1)
      (stake (default-to u0 (map-get? voter-stakes voter)))
      (delegation (map-get? delegate-power voter))
    )
    (+ base-weight 
       (if (>= stake (var-get min-stake-required)) stake u0)
       (match delegation del (get total-weight del) u0)
    )
  )
)

;; Check voter eligibility
(define-private (is-eligible-voter (voter principal))
  (and 
    (or (not (var-get use-whitelist)) (default-to false (map-get? voter-whitelist voter)))
    (not (default-to false (map-get? voter-blacklist voter)))
    (>= (calculate-vote-weight voter) u1)
  )
)

;; Public Functions

;; Enhanced voting initialization with more parameters
(define-public (start-voting (duration uint) (voting-type-param uint) (quorum uint) (min-stake uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (not (var-get voting-active)) ERR_VOTING_ENDED)
    
    (var-set voting-active true)
    (var-set voting-start-block block-height)
    (var-set voting-end-block (+ block-height duration))
    (var-set voting-type voting-type-param)
    (var-set quorum-threshold quorum)
    (var-set min-stake-required min-stake)
    (var-set current-round (+ (var-get current-round) u1))
    
    (ok (var-get current-round))
  )
)

;; Register candidate with description and stake requirement
(define-public (register-candidate (candidate principal) (description (string-ascii 256)) (stake uint))
  (begin
    (asserts! (or (is-eq tx-sender CONTRACT_OWNER) (is-eq tx-sender candidate)) ERR_NOT_AUTHORIZED)
    (asserts! (>= stake (var-get min-stake-required)) ERR_INSUFFICIENT_STAKE)
    
    ;; Transfer stake (simplified - in real implementation would use STX transfer)
    (map-set candidate-stakes candidate stake)
    (map-set candidates candidate {
      votes: u0,
      description: description,
      active: true,
      stake: stake
    })
    (ok true)
  )
)

;; Enhanced vote casting with multiple voting types
(define-public (cast-vote (candidate principal) (vote-weight uint))
  (let 
    (
      (voter tx-sender)
      (current-block block-height)
      (voting-round (var-get current-round))
      (calculated-weight (min-uint vote-weight (calculate-vote-weight voter)))
      (receipt-hash (generate-vote-receipt voter candidate calculated-weight voting-round))
    )
    (begin
      ;; Eligibility checks
      (asserts! (is-voting-active) ERR_VOTING_NOT_STARTED)
      (asserts! (is-eligible-voter voter) ERR_NOT_AUTHORIZED)
      (asserts! (<= calculated-weight (calculate-vote-weight voter)) ERR_INVALID_VOTE_WEIGHT)
      
      ;; Check if candidate exists and is active
      (match (map-get? candidates candidate)
        candidate-info (asserts! (get active candidate-info) ERR_INVALID_CANDIDATE)
        (asserts! false ERR_INVALID_CANDIDATE)
      )
      
      ;; Update vote records
      (map-set vote-records receipt-hash {
        voter: voter,
        candidate: candidate,
        block-height: current-block,
        timestamp: stx-liquid-supply,
        weight: calculated-weight,
        round: voting-round,
        vote-type: (var-get voting-type)
      })
      
      ;; Update voter receipts (append to list)
      (map-set voter-receipts voter 
        (unwrap-panic (as-max-len? 
          (append (default-to (list) (map-get? voter-receipts voter)) receipt-hash) 
          u10
        ))
      )
      
      ;; Update voter status
      (let ((current-status (default-to {voted: false, round: u0, vote-count: u0, last-vote-block: u0} (map-get? voter-status voter))))
        (map-set voter-status voter {
          voted: true,
          round: voting-round,
          vote-count: (+ (get vote-count current-status) u1),
          last-vote-block: current-block
        })
      )
      
      ;; Update candidate votes
      (match (map-get? candidates candidate)
        candidate-info 
        (begin
          (map-set candidates candidate 
            (merge candidate-info {votes: (+ (get votes candidate-info) calculated-weight)}))
          true
        )
        (asserts! false ERR_INVALID_CANDIDATE)
      )
      
      ;; Update totals
      (var-set total-votes (+ (var-get total-votes) calculated-weight))
      
      ;; Add to voting history
      (let ((history (default-to (list) (map-get? voting-history voter))))
        (map-set voting-history voter 
          (unwrap-panic (as-max-len? 
            (append history {
              round: voting-round,
              candidate: candidate,
              timestamp: stx-liquid-supply,
              weight: calculated-weight
            }) 
            u50
          ))
        )
      )
      
      (ok receipt-hash)
    )
  )
)

;; Delegate voting power
(define-public (delegate-vote (delegate principal) (weight uint))
  (let ((delegator tx-sender))
    (begin
      (asserts! (not (is-eq delegator delegate)) ERR_SELF_DELEGATION)
      (asserts! (is-none (map-get? vote-delegations delegator)) ERR_ALREADY_DELEGATED)
      (asserts! (<= weight (calculate-vote-weight delegator)) ERR_INVALID_VOTE_WEIGHT)
      
      ;; Create delegation
      (map-set vote-delegations delegator {
        delegate: delegate,
        weight: weight,
        active: true
      })
      
      ;; Update delegate power
      (match (map-get? delegate-power delegate)
        existing-power 
        (map-set delegate-power delegate {
          total-weight: (+ (get total-weight existing-power) weight),
          delegator-count: (+ (get delegator-count existing-power) u1)
        })
        (map-set delegate-power delegate {
          total-weight: weight,
          delegator-count: u1
        })
      )
      
      (ok true)
    )
  )
)

;; Revoke delegation
(define-public (revoke-delegation)
  (let ((delegator tx-sender))
    (match (map-get? vote-delegations delegator)
      delegation
      (let ((delegate (get delegate delegation))
            (weight (get weight delegation)))
        (begin
          ;; Remove delegation
          (map-delete vote-delegations delegator)
          
          ;; Update delegate power
          (match (map-get? delegate-power delegate)
            existing-power 
            (map-set delegate-power delegate {
              total-weight: (- (get total-weight existing-power) weight),
              delegator-count: (- (get delegator-count existing-power) u1)
            })
            false
          )
          
          (ok true)
        )
      )
      (err u404)
    )
  )
)

;; Stake tokens for voting weight
(define-public (stake-tokens (amount uint))
  (begin
    ;; In real implementation, would transfer STX tokens to contract
    (map-set voter-stakes tx-sender 
      (+ (default-to u0 (map-get? voter-stakes tx-sender)) amount))
    (ok true)
  )
)

;; Create governance proposal
(define-public (create-proposal (title (string-ascii 256)) (description (string-ascii 1024)) (duration uint))
  (let ((proposal-id (+ (var-get proposal-counter) u1)))
    (begin
      (asserts! (>= (calculate-vote-weight tx-sender) u10) ERR_INSUFFICIENT_STAKE)
      
      (map-set proposals proposal-id {
        title: title,
        description: description,
        proposer: tx-sender,
        votes-for: u0,
        votes-against: u0,
        start-block: block-height,
        end-block: (+ block-height duration),
        executed: false,
        proposal-type: u1
      })
      
      (var-set proposal-counter proposal-id)
      (ok proposal-id)
    )
  )
)

;; Vote on governance proposal
(define-public (vote-on-proposal (proposal-id uint) (support bool))
  (let ((voter-weight (calculate-vote-weight tx-sender)))
    (match (map-get? proposals proposal-id)
      proposal
      (begin
        (asserts! (<= block-height (get end-block proposal)) ERR_VOTING_ENDED)
        (asserts! (>= block-height (get start-block proposal)) ERR_VOTING_NOT_STARTED)
        
        (if support
          (map-set proposals proposal-id 
            (merge proposal {votes-for: (+ (get votes-for proposal) voter-weight)}))
          (map-set proposals proposal-id 
            (merge proposal {votes-against: (+ (get votes-against proposal) voter-weight)}))
        )
        
        (ok true)
      )
      ERR_INVALID_PROPOSAL
    )
  )
)

;; Emergency functions
(define-public (activate-emergency-stop)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (var-set emergency-stop true)
    (ok true)
  )
)

(define-public (resume-voting)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (var-set emergency-stop false)
    (ok true)
  )
)

;; Whitelist management
(define-public (add-to-whitelist (voter principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (map-set voter-whitelist voter true)
    (ok true)
  )
)

(define-public (remove-from-whitelist (voter principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (map-delete voter-whitelist voter)
    (ok true)
  )
)

;; Blacklist management
(define-public (add-to-blacklist (voter principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (map-set voter-blacklist voter true)
    (ok true)
  )
)

;; Commit-reveal voting
(define-public (commit-vote (commit-hash (buff 32)))
  (begin
    (asserts! (is-voting-active) ERR_VOTING_NOT_STARTED)
    (asserts! (not (var-get vote-reveal-phase)) ERR_VOTING_ROUND_ENDED)
    
    (map-set vote-commits tx-sender {
      commit-hash: commit-hash,
      revealed: false,
      commit-block: block-height
    })
    
    (ok true)
  )
)

(define-public (reveal-vote (candidate principal) (nonce uint))
  (let 
    (
      (expected-hash (keccak256 (concat (unwrap-panic (to-consensus-buff? candidate)) (unwrap-panic (to-consensus-buff? nonce)))))
      (voter tx-sender)
    )
    (match (map-get? vote-commits voter)
      commit-data
      (begin
        (asserts! (var-get vote-reveal-phase) ERR_VOTING_NOT_STARTED)
        (asserts! (is-eq (get commit-hash commit-data) expected-hash) ERR_NOT_AUTHORIZED)
        (asserts! (not (get revealed commit-data)) ERR_ALREADY_VOTED)
        
        ;; Mark as revealed and process vote
        (map-set vote-commits voter (merge commit-data {revealed: true}))
        (cast-vote candidate u1)
      )
      ERR_VOTE_NOT_FOUND
    )
  )
)

;; Read-only Functions

;; Get enhanced candidate information
(define-read-only (get-candidate-info (candidate principal))
  (map-get? candidates candidate)
)

;; Get voter's complete voting history
(define-read-only (get-voting-history (voter principal))
  (map-get? voting-history voter)
)

;; Get delegation information
(define-read-only (get-delegation-info (voter principal))
  (map-get? vote-delegations voter)
)

;; Get delegate power
(define-read-only (get-delegate-power (delegate principal))
  (map-get? delegate-power delegate)
)

;; Get proposal information
(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals proposal-id)
)

;; Get voter eligibility status
(define-read-only (get-voter-eligibility (voter principal))
  {
    eligible: (is-eligible-voter voter),
    vote-weight: (calculate-vote-weight voter),
    stake: (default-to u0 (map-get? voter-stakes voter)),
    whitelisted: (if (var-get use-whitelist) (default-to false (map-get? voter-whitelist voter)) true),
    blacklisted: (default-to false (map-get? voter-blacklist voter))
  }
)

;; Get comprehensive voting statistics
(define-read-only (get-voting-stats)
  {
    total-votes: (var-get total-votes),
    current-round: (var-get current-round),
    voting-type: (var-get voting-type),
    quorum-threshold: (var-get quorum-threshold),
    quorum-met: (>= (var-get total-votes) (var-get quorum-threshold)),
    active: (is-voting-active),
    emergency-stopped: (var-get emergency-stop)
  }
)

;; Get round results
(define-read-only (get-round-results (round uint))
  (map-get? round-results round)
)

;; Verify vote with enhanced information
(define-read-only (verify-vote-enhanced (receipt (buff 32)))
  (match (map-get? vote-records receipt)
    vote-record 
    (ok {
      vote-record: vote-record,
      valid: true,
      round-active: (is-eq (get round vote-record) (var-get current-round))
    })
    ERR_VOTE_NOT_FOUND
  )
)

;; Get all voter receipts
(define-read-only (get-all-voter-receipts (voter principal))
  (default-to (list) (map-get? voter-receipts voter))
)

;; Check if quorum is met
(define-read-only (is-quorum-met)
  (>= (var-get total-votes) (var-get quorum-threshold))
)