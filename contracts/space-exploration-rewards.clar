;; Space Exploration Rewards Contract
;; Tokens for contributing to space exploration and interplanetary development

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_OWNER (err u400))
(define-constant ERR_INSUFFICIENT_BALANCE (err u401))
(define-constant ERR_INVALID_AMOUNT (err u402))
(define-constant ERR_UNAUTHORIZED (err u403))
(define-constant ERR_REWARD_NOT_FOUND (err u404))
(define-constant ERR_ALREADY_CLAIMED (err u405))
(define-constant ERR_STAKING_LOCKED (err u406))
(define-constant ERR_INVALID_PARAMETERS (err u407))

;; Token constants
(define-constant TOKEN_NAME "SpaceExploration")
(define-constant TOKEN_SYMBOL "SPACE")
(define-constant TOKEN_DECIMALS u6)
(define-constant MAX_SUPPLY u1000000000) ;; 1 billion tokens
(define-constant INITIAL_SUPPLY u100000000) ;; 100 million initial supply

;; Reward multipliers
(define-constant DISCOVERY_REWARD_MULTIPLIER u1000)
(define-constant ANALYSIS_REWARD_MULTIPLIER u100)
(define-constant SATELLITE_REWARD_MULTIPLIER u50)
(define-constant MISSION_REWARD_MULTIPLIER u500)

;; Staking parameters
(define-constant MIN_STAKE_AMOUNT u1000)
(define-constant STAKING_LOCK_PERIOD u144) ;; ~24 hours in blocks

;; Data Variables
(define-data-var total-supply uint INITIAL_SUPPLY)
(define-data-var total-rewards-distributed uint u0)
(define-data-var total-staked uint u0)
(define-data-var reward-counter uint u0)
(define-data-var governance-proposals uint u0)

;; Data Maps
(define-map balances principal uint)
(define-map allowances {owner: principal, spender: principal} uint)

(define-map reward-claims
  uint
  {
    recipient: principal,
    amount: uint,
    reward-type: (string-ascii 50),
    source-contract: (string-ascii 100),
    source-id: uint,
    claimed: bool,
    created-at: uint,
    claimed-at: (optional uint)
  }
)

(define-map staking-positions
  principal
  {
    staked-amount: uint,
    stake-start-block: uint,
    lock-end-block: uint,
    rewards-earned: uint,
    last-reward-block: uint
  }
)

(define-map achievement-rewards
  {recipient: principal, achievement-type: (string-ascii 50)}
  {
    total-earned: uint,
    milestones: (list 10 uint),
    last-awarded: uint
  }
)

(define-map governance-votes
  {proposal-id: uint, voter: principal}
  {
    vote-power: uint,
    vote-choice: bool,
    voted-at: uint
  }
)

(define-map validator-rewards
  principal
  {
    validation-count: uint,
    successful-validations: uint,
    total-rewards: uint,
    reputation-score: uint,
    last-validation: uint
  }
)

;; Private Functions
(define-private (mint-tokens (recipient principal) (amount uint))
  (let (
    (current-balance (default-to u0 (map-get? balances recipient)))
    (new-total-supply (+ (var-get total-supply) amount))
  )
    (asserts! (<= new-total-supply MAX_SUPPLY) ERR_INVALID_AMOUNT)
    
    (map-set balances recipient (+ current-balance amount))
    (var-set total-supply new-total-supply)
    (ok amount)
  )
)

(define-private (burn-tokens (holder principal) (amount uint))
  (let (
    (current-balance (default-to u0 (map-get? balances holder)))
  )
    (asserts! (>= current-balance amount) ERR_INSUFFICIENT_BALANCE)
    
    (map-set balances holder (- current-balance amount))
    (var-set total-supply (- (var-get total-supply) amount))
    (ok amount)
  )
)

(define-private (calculate-staking-rewards (staker principal) (current-block uint))
  (match (map-get? staking-positions staker)
    staking-info
      (let (
        (blocks-staked (- current-block (get last-reward-block staking-info)))
        (reward-rate (/ (get staked-amount staking-info) u10000)) ;; 0.01% per block
      )
        (* blocks-staked reward-rate)
      )
    u0
  )
)

;; Initialize contract owner balance
(map-set balances CONTRACT_OWNER INITIAL_SUPPLY)

;; Public Functions - Token Standard
(define-public (transfer (amount uint) (sender principal) (recipient principal) (memo (optional (buff 34))))
  (let (
    (sender-balance (default-to u0 (map-get? balances sender)))
  )
    (asserts! (>= sender-balance amount) ERR_INSUFFICIENT_BALANCE)
    (asserts! (or (is-eq tx-sender sender) (is-eq tx-sender CONTRACT_OWNER)) ERR_UNAUTHORIZED)
    
    (map-set balances sender (- sender-balance amount))
    (map-set balances recipient (+ (default-to u0 (map-get? balances recipient)) amount))
    
    (ok true)
  )
)

(define-public (approve (spender principal) (amount uint))
  (begin
    (map-set allowances {owner: tx-sender, spender: spender} amount)
    (ok true)
  )
)

(define-public (transfer-from (amount uint) (owner principal) (recipient principal))
  (let (
    (allowance (default-to u0 (map-get? allowances {owner: owner, spender: tx-sender})))
    (owner-balance (default-to u0 (map-get? balances owner)))
  )
    (asserts! (>= allowance amount) ERR_UNAUTHORIZED)
    (asserts! (>= owner-balance amount) ERR_INSUFFICIENT_BALANCE)
    
    (map-set allowances {owner: owner, spender: tx-sender} (- allowance amount))
    (map-set balances owner (- owner-balance amount))
    (map-set balances recipient (+ (default-to u0 (map-get? balances recipient)) amount))
    
    (ok true)
  )
)

;; Reward Distribution Functions
(define-public (create-reward-claim
  (recipient principal)
  (amount uint)
  (reward-type (string-ascii 50))
  (source-contract (string-ascii 100))
  (source-id uint)
)
  (let (
    (reward-id (+ (var-get reward-counter) u1))
  )
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    
    (map-set reward-claims reward-id {
      recipient: recipient,
      amount: amount,
      reward-type: reward-type,
      source-contract: source-contract,
      source-id: source-id,
      claimed: false,
      created-at: stacks-block-height,
      claimed-at: none
    })
    
    (var-set reward-counter reward-id)
    (ok reward-id)
  )
)

(define-public (claim-reward (reward-id uint))
  (let (
    (reward (unwrap! (map-get? reward-claims reward-id) ERR_REWARD_NOT_FOUND))
  )
    (asserts! (is-eq (get recipient reward) tx-sender) ERR_UNAUTHORIZED)
    (asserts! (not (get claimed reward)) ERR_ALREADY_CLAIMED)
    
    ;; Mint tokens for the reward
    (unwrap-panic (mint-tokens tx-sender (get amount reward)))
    
    ;; Mark as claimed
    (map-set reward-claims reward-id
      (merge reward {
        claimed: true,
        claimed-at: (some stacks-block-height)
      })
    )
    
    (var-set total-rewards-distributed (+ (var-get total-rewards-distributed) (get amount reward)))
    (ok (get amount reward))
  )
)

;; Achievement and Discovery Rewards
(define-public (award-discovery-reward (recipient principal) (discovery-type (string-ascii 50)))
  (let (
    (reward-amount (* DISCOVERY_REWARD_MULTIPLIER u1))
    (achievement-key {recipient: recipient, achievement-type: discovery-type})
    (current-achievements (default-to {
      total-earned: u0,
      milestones: (list),
      last-awarded: u0
    } (map-get? achievement-rewards achievement-key)))
  )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED) ;; Only contract owner can award
    
    ;; Update achievement record
    (map-set achievement-rewards achievement-key
      (merge current-achievements {
        total-earned: (+ (get total-earned current-achievements) reward-amount),
        milestones: (unwrap-panic (as-max-len? (append (get milestones current-achievements) stacks-block-height) u10)),
        last-awarded: stacks-block-height
      })
    )
    
    ;; Mint reward tokens
    (unwrap-panic (mint-tokens recipient reward-amount))
    
    (ok reward-amount)
  )
)

;; Staking Functions
(define-public (stake-tokens (amount uint))
  (let (
    (current-balance (default-to u0 (map-get? balances tx-sender)))
    (current-stake (default-to {
      staked-amount: u0,
      stake-start-block: u0,
      lock-end-block: u0,
      rewards-earned: u0,
      last-reward-block: u0
    } (map-get? staking-positions tx-sender)))
  )
    (asserts! (>= amount MIN_STAKE_AMOUNT) ERR_INVALID_AMOUNT)
    (asserts! (>= current-balance amount) ERR_INSUFFICIENT_BALANCE)
    
    ;; Transfer tokens from balance to staking
    (map-set balances tx-sender (- current-balance amount))
    
    ;; Update staking position
    (map-set staking-positions tx-sender {
      staked-amount: (+ (get staked-amount current-stake) amount),
      stake-start-block: stacks-block-height,
      lock-end-block: (+ stacks-block-height STAKING_LOCK_PERIOD),
      rewards-earned: (get rewards-earned current-stake),
      last-reward-block: stacks-block-height
    })
    
    (var-set total-staked (+ (var-get total-staked) amount))
    (ok amount)
  )
)

(define-public (unstake-tokens (amount uint))
  (let (
    (current-stake (unwrap! (map-get? staking-positions tx-sender) ERR_INSUFFICIENT_BALANCE))
    (current-balance (default-to u0 (map-get? balances tx-sender)))
  )
    (asserts! (>= (get staked-amount current-stake) amount) ERR_INSUFFICIENT_BALANCE)
    (asserts! (>= stacks-block-height (get lock-end-block current-stake)) ERR_STAKING_LOCKED)
    
    ;; Calculate and claim pending rewards
    (let (
      (pending-rewards (calculate-staking-rewards tx-sender stacks-block-height))
    )
      (if (> pending-rewards u0)
        (unwrap-panic (mint-tokens tx-sender pending-rewards))
        u0
      )
      
      ;; Update staking position
      (map-set staking-positions tx-sender
        (merge current-stake {
          staked-amount: (- (get staked-amount current-stake) amount),
          rewards-earned: (+ (get rewards-earned current-stake) pending-rewards),
          last-reward-block: stacks-block-height
        })
      )
      
      ;; Return tokens to balance
      (map-set balances tx-sender (+ current-balance amount))
      (var-set total-staked (- (var-get total-staked) amount))
      
      (ok amount)
    )
  )
)

(define-public (claim-staking-rewards)
  (let (
    (staking-info (unwrap! (map-get? staking-positions tx-sender) ERR_INSUFFICIENT_BALANCE))
    (pending-rewards (calculate-staking-rewards tx-sender stacks-block-height))
  )
    (asserts! (> pending-rewards u0) ERR_INVALID_AMOUNT)
    
    ;; Mint reward tokens
    (unwrap-panic (mint-tokens tx-sender pending-rewards))
    
    ;; Update staking position
    (map-set staking-positions tx-sender
      (merge staking-info {
        rewards-earned: (+ (get rewards-earned staking-info) pending-rewards),
        last-reward-block: stacks-block-height
      })
    )
    
    (ok pending-rewards)
  )
)

;; Read-only Functions
(define-read-only (get-balance (account principal))
  (default-to u0 (map-get? balances account))
)

(define-read-only (get-allowance (owner principal) (spender principal))
  (default-to u0 (map-get? allowances {owner: owner, spender: spender}))
)

(define-read-only (get-total-supply)
  (var-get total-supply)
)

(define-read-only (get-reward-claim (reward-id uint))
  (map-get? reward-claims reward-id)
)

(define-read-only (get-staking-position (staker principal))
  (map-get? staking-positions staker)
)

(define-read-only (get-achievement-rewards (recipient principal) (achievement-type (string-ascii 50)))
  (map-get? achievement-rewards {recipient: recipient, achievement-type: achievement-type})
)

(define-read-only (get-validator-stats (validator principal))
  (map-get? validator-rewards validator)
)

(define-read-only (get-token-info)
  {
    name: TOKEN_NAME,
    symbol: TOKEN_SYMBOL,
    decimals: TOKEN_DECIMALS,
    total-supply: (var-get total-supply),
    max-supply: MAX_SUPPLY
  }
)

(define-read-only (get-system-stats)
  {
    total-rewards-distributed: (var-get total-rewards-distributed),
    total-staked: (var-get total-staked),
    total-reward-claims: (var-get reward-counter),
    governance-proposals: (var-get governance-proposals)
  }
)

;; title: space-exploration-rewards
;; version:
;; summary:
;; description:

;; traits
;;

;; token definitions
;;

;; constants
;;

;; data vars
;;

;; data maps
;;

;; public functions
;;

;; read only functions
;;

;; private functions
;;

