;; Space Mission Coordinator Contract
;; Collaborative planning and resource sharing for Mars and asteroid missions

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_OWNER (err u200))
(define-constant ERR_MISSION_NOT_FOUND (err u201))
(define-constant ERR_MISSION_ALREADY_EXISTS (err u202))
(define-constant ERR_UNAUTHORIZED (err u203))
(define-constant ERR_INVALID_PHASE (err u204))
(define-constant ERR_INSUFFICIENT_RESOURCES (err u205))
(define-constant ERR_MISSION_COMPLETED (err u206))
(define-constant ERR_INVALID_CONTRIBUTION (err u207))

;; Mission phases
(define-constant PHASE_PLANNING u1)
(define-constant PHASE_PREPARATION u2)
(define-constant PHASE_LAUNCH u3)
(define-constant PHASE_IN_TRANSIT u4)
(define-constant PHASE_OPERATIONAL u5)
(define-constant PHASE_COMPLETED u6)
(define-constant PHASE_FAILED u7)

;; Mission types
(define-constant TYPE_MARS_EXPLORATION u1)
(define-constant TYPE_ASTEROID_MINING u2)
(define-constant TYPE_DEEP_SPACE_PROBE u3)
(define-constant TYPE_LUNAR_BASE u4)

;; Data Variables
(define-data-var mission-counter uint u0)
(define-data-var total-missions-completed uint u0)
(define-data-var total-resources-allocated uint u0)

;; Data Maps
(define-map missions
  uint
  {
    name: (string-ascii 100),
    mission-type: uint,
    lead-agency: principal,
    current-phase: uint,
    target-destination: (string-ascii 50),
    launch-window-start: uint,
    launch-window-end: uint,
    estimated-duration: uint,
    required-resources: uint,
    allocated-resources: uint,
    participating-agencies: (list 20 principal),
    success-probability: uint,
    created-at: uint,
    completed-at: (optional uint)
  }
)

(define-map mission-resources
  {mission-id: uint, resource-type: (string-ascii 30)}
  {
    required-amount: uint,
    allocated-amount: uint,
    contributors: (list 50 principal),
    last-updated: uint
  }
)

(define-map agency-contributions
  {agency: principal, mission-id: uint}
  {
    resource-contributions: (list 10 {resource-type: (string-ascii 30), amount: uint}),
    expertise-areas: (list 5 (string-ascii 50)),
    commitment-level: uint,
    contribution-value: uint,
    joined-at: uint
  }
)

(define-map mission-milestones
  {mission-id: uint, milestone-id: uint}
  {
    description: (string-ascii 200),
    target-date: uint,
    completed: bool,
    completion-date: (optional uint),
    responsible-agency: principal
  }
)

(define-map agency-profiles
  principal
  {
    name: (string-ascii 100),
    specialization: (list 5 (string-ascii 50)),
    reputation-score: uint,
    missions-participated: (list 100 uint),
    total-contributions: uint,
    success-rate: uint,
    registered-at: uint
  }
)

;; Private Functions
(define-private (is-valid-mission-type (mission-type uint))
  (and (>= mission-type TYPE_MARS_EXPLORATION) (<= mission-type TYPE_LUNAR_BASE))
)

(define-private (is-valid-phase (phase uint))
  (and (>= phase PHASE_PLANNING) (<= phase PHASE_FAILED))
)

(define-private (calculate-success-probability (allocated-resources uint) (required-resources uint))
  (if (>= allocated-resources required-resources)
    u95
    (/ (* allocated-resources u95) required-resources)
  )
)

(define-private (update-agency-reputation (agency principal) (mission-success bool))
  (match (map-get? agency-profiles agency)
    existing-profile
      (let (
        (current-score (get reputation-score existing-profile))
        (new-score (if mission-success (+ current-score u10) (- current-score u5)))
      )
        (map-set agency-profiles agency
          (merge existing-profile {
            reputation-score: new-score
          })
        )
        true
      )
    false
  )
)

;; Public Functions
(define-public (create-mission
  (name (string-ascii 100))
  (mission-type uint)
  (target-destination (string-ascii 50))
  (launch-window-start uint)
  (launch-window-end uint)
  (estimated-duration uint)
  (required-resources uint)
)
  (let (
    (mission-id (+ (var-get mission-counter) u1))
  )
    (asserts! (is-valid-mission-type mission-type) ERR_INVALID_PHASE)
    (asserts! (> launch-window-end launch-window-start) ERR_INVALID_PHASE)
    (asserts! (> required-resources u0) ERR_INSUFFICIENT_RESOURCES)
    
    (map-set missions mission-id {
      name: name,
      mission-type: mission-type,
      lead-agency: tx-sender,
      current-phase: PHASE_PLANNING,
      target-destination: target-destination,
      launch-window-start: launch-window-start,
      launch-window-end: launch-window-end,
      estimated-duration: estimated-duration,
      required-resources: required-resources,
      allocated-resources: u0,
      participating-agencies: (list tx-sender),
      success-probability: u0,
      created-at: stacks-block-height,
      completed-at: none
    })
    
    (var-set mission-counter mission-id)
    (ok mission-id)
  )
)

(define-public (join-mission
  (mission-id uint)
  (resource-contributions (list 10 {resource-type: (string-ascii 30), amount: uint}))
  (expertise-areas (list 5 (string-ascii 50)))
  (commitment-level uint)
)
  (let (
    (mission (unwrap! (map-get? missions mission-id) ERR_MISSION_NOT_FOUND))
    (contribution-value (fold calculate-total-contribution resource-contributions u0))
  )
    (asserts! (< (get current-phase mission) PHASE_LAUNCH) ERR_MISSION_COMPLETED)
    (asserts! (> commitment-level u0) ERR_INVALID_CONTRIBUTION)
    
    ;; Update mission with new participating agency
    (map-set missions mission-id
      (merge mission {
        participating-agencies: (unwrap-panic (as-max-len? 
          (append (get participating-agencies mission) tx-sender) u20)),
        allocated-resources: (+ (get allocated-resources mission) contribution-value)
      })
    )
    
    ;; Record agency contribution
    (map-set agency-contributions {agency: tx-sender, mission-id: mission-id} {
      resource-contributions: resource-contributions,
      expertise-areas: expertise-areas,
      commitment-level: commitment-level,
      contribution-value: contribution-value,
      joined-at: stacks-block-height
    })
    
    (ok true)
  )
)

(define-private (calculate-total-contribution 
  (contribution {resource-type: (string-ascii 30), amount: uint})
  (current-total uint)
)
  (+ current-total (get amount contribution))
)

(define-public (update-mission-phase (mission-id uint) (new-phase uint))
  (let (
    (mission (unwrap! (map-get? missions mission-id) ERR_MISSION_NOT_FOUND))
  )
    (asserts! (is-eq (get lead-agency mission) tx-sender) ERR_UNAUTHORIZED)
    (asserts! (is-valid-phase new-phase) ERR_INVALID_PHASE)
    (asserts! (> new-phase (get current-phase mission)) ERR_INVALID_PHASE)
    
    (map-set missions mission-id (merge mission {
      current-phase: new-phase,
      success-probability: (calculate-success-probability 
        (get allocated-resources mission) 
        (get required-resources mission)
      )
    }))
    
    ;; If mission completed, update stats and reputation
    (if (or (is-eq new-phase PHASE_COMPLETED) (is-eq new-phase PHASE_FAILED))
      (begin
        (var-set total-missions-completed (+ (var-get total-missions-completed) u1))
        (map-set missions mission-id 
          (merge mission {completed-at: (some stacks-block-height)}))
        ;; Update reputation for all participating agencies
        (begin
          (update-agency-reputation tx-sender (is-eq new-phase PHASE_COMPLETED))
          true
        )
      )
      true
    )
    
    (ok true)
  )
)

(define-public (add-mission-milestone
  (mission-id uint)
  (milestone-id uint)
  (description (string-ascii 200))
  (target-date uint)
  (responsible-agency principal)
)
  (let (
    (mission (unwrap! (map-get? missions mission-id) ERR_MISSION_NOT_FOUND))
  )
    (asserts! (is-eq (get lead-agency mission) tx-sender) ERR_UNAUTHORIZED)
    
    (map-set mission-milestones {mission-id: mission-id, milestone-id: milestone-id} {
      description: description,
      target-date: target-date,
      completed: false,
      completion-date: none,
      responsible-agency: responsible-agency
    })
    
    (ok true)
  )
)

(define-public (complete-milestone
  (mission-id uint)
  (milestone-id uint)
)
  (let (
    (milestone-key {mission-id: mission-id, milestone-id: milestone-id})
    (milestone (unwrap! (map-get? mission-milestones milestone-key) ERR_MISSION_NOT_FOUND))
  )
    (asserts! (is-eq (get responsible-agency milestone) tx-sender) ERR_UNAUTHORIZED)
    (asserts! (not (get completed milestone)) ERR_MISSION_COMPLETED)
    
    (map-set mission-milestones milestone-key
      (merge milestone {
        completed: true,
        completion-date: (some stacks-block-height)
      })
    )
    
    (ok true)
  )
)

(define-public (register-agency
  (name (string-ascii 100))
  (specialization (list 5 (string-ascii 50)))
)
  (begin
    (map-set agency-profiles tx-sender {
      name: name,
      specialization: specialization,
      reputation-score: u100,
      missions-participated: (list),
      total-contributions: u0,
      success-rate: u100,
      registered-at: stacks-block-height
    })
    (ok true)
  )
)

;; Read-only Functions
(define-read-only (get-mission (mission-id uint))
  (map-get? missions mission-id)
)

(define-read-only (get-agency-contribution (agency principal) (mission-id uint))
  (map-get? agency-contributions {agency: agency, mission-id: mission-id})
)

(define-read-only (get-mission-milestone (mission-id uint) (milestone-id uint))
  (map-get? mission-milestones {mission-id: mission-id, milestone-id: milestone-id})
)

(define-read-only (get-agency-profile (agency principal))
  (map-get? agency-profiles agency)
)

(define-read-only (get-mission-stats)
  {
    total-missions: (var-get mission-counter),
    completed-missions: (var-get total-missions-completed),
    total-resources-allocated: (var-get total-resources-allocated)
  }
)

;; title: space-mission-coordinator
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

