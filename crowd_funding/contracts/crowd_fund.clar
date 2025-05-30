;; Decentralized Crowdfunding Platform

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-deadline-passed (err u104))
(define-constant err-goal-not-reached (err u105))
(define-constant err-already-claimed (err u106))
(define-constant  err-transfer-failed (err u107))

;; Additional Constants
(define-constant err-campaign-active (err u108))
(define-constant err-minimum-contribution (err u109))
(define-constant err-already-reported (err u110))
(define-constant err-milestone-not-found (err u111))

;; Additional Maps
(define-map campaign-milestones
    { campaign-id: uint, milestone-id: uint }
    {
        title: (string-utf8 100),
        description: (string-utf8 500),
        target-amount: uint,
        completed: bool,
        deadline: uint
    }
)

(define-map campaign-updates
    { campaign-id: uint, update-id: uint }
    {
        title: (string-utf8 100),
        content: (string-utf8 1000),
        timestamp: uint
    }
)

(define-map campaign-reports
    { campaign-id: uint, reporter: principal }
    {
        reason: (string-utf8 500),
        timestamp: uint,
        status: (string-ascii 20)
    }
)

(define-map campaign-stats 
    { campaign-id: uint }
    {
        unique-contributors: uint,
        avg-contribution: uint,
        largest-contribution: uint,
        updates-count: uint
    }
)

;; Data Variables for tracking
(define-data-var minimum-contribution uint u1000000) ;; 1 STX
(define-data-var platform-fee-percentage uint u25) ;; 0.25%

;; Data Maps
(define-map campaigns
  { campaign-id: uint }
  {
    owner: principal,
    goal: uint,
    raised: uint,
    deadline: uint,
    claimed: bool
  }
)

(define-map contributions
  { campaign-id: uint, contributor: principal }
  { amount: uint }
)

(define-map campaign-descriptions
  { campaign-id: uint }
  { description: (string-utf8 500) })


;; Variables
(define-data-var campaign-nonce uint u0)

;; Private Functions
(define-private (is-owner)
  (is-eq tx-sender contract-owner))

(define-private (current-time)
  (unwrap-panic (get-block-info? time u0)))

;; Read-only Functions
(define-read-only (get-campaign-details (campaign-id uint))
  (map-get? campaigns { campaign-id: campaign-id }))

(define-read-only (get-contribution (campaign-id uint) (contributor principal))
  (map-get? contributions { campaign-id: campaign-id, contributor: contributor }))

;; Read-only function to get milestone details
(define-read-only (get-milestone-details (campaign-id uint) (milestone-id uint))
    (map-get? campaign-milestones { campaign-id: campaign-id, milestone-id: milestone-id })
)

;; Read-only function to get campaign stats
(define-read-only (get-campaign-statistics (campaign-id uint))
    (map-get? campaign-stats { campaign-id: campaign-id })
)

;; Read-only function to calculate platform fees
(define-read-only (calculate-platform-fee (amount uint))
    (/ (* amount (var-get platform-fee-percentage)) u10000)
)


;; Public Functions
(define-public (create-campaign (goal uint) (deadline uint))
  (let
    (
      (campaign-id (var-get campaign-nonce))
    )
    (asserts! (> goal u0) (err err-invalid-amount))
    (asserts! (> deadline (current-time)) (err err-deadline-passed))
    (map-insert campaigns
      { campaign-id: campaign-id }
      {
        owner: tx-sender,
        goal: goal,
        raised: u0,
        deadline: deadline,
        claimed: false
      }
    )
    (var-set campaign-nonce (+ campaign-id u1))
    (ok campaign-id)
  )
)

(define-public (contribute (campaign-id uint) (amount uint))
  (let
    (
      (campaign (unwrap! (map-get? campaigns { campaign-id: campaign-id }) (err err-not-found)))
      (current-raised (get raised campaign))
      (new-raised (+ current-raised amount))
    )
    (asserts! (< (current-time) (get deadline campaign)) (err err-deadline-passed))
    (asserts! (> amount u0) (err err-invalid-amount))
    (match (stx-transfer? amount tx-sender (as-contract tx-sender))
      success
        (begin
          (map-set campaigns
            { campaign-id: campaign-id }
            (merge campaign { raised: new-raised })
          )
          (map-set contributions
            { campaign-id: campaign-id, contributor: tx-sender }
            { amount: (+ amount (default-to u0 (get amount (map-get? contributions { campaign-id: campaign-id, contributor: tx-sender })))) }
          )
          (ok true)
        )
      error (err err-transfer-failed)
    )
  )
)

(define-public (claim-funds (campaign-id uint))
  (let
    (
      (campaign (unwrap! (map-get? campaigns { campaign-id: campaign-id }) (err err-not-found)))
    )
    (asserts! (is-eq (get owner campaign) tx-sender) (err err-owner-only))
    (asserts! (>= (current-time) (get deadline campaign)) (err err-deadline-passed))
    (asserts! (>= (get raised campaign) (get goal campaign)) (err err-goal-not-reached))
    (asserts! (not (get claimed campaign)) (err err-already-claimed))
    (match (as-contract (stx-transfer? (get raised campaign) tx-sender (get owner campaign)))
      success
        (begin
          (map-set campaigns
            { campaign-id: campaign-id }
            (merge campaign { claimed: true })
          )
          (ok true)
        )
      error (err err-transfer-failed)
    )
  )
)

(define-public (refund (campaign-id uint))
  (let
    (
      (campaign (unwrap! (map-get? campaigns { campaign-id: campaign-id }) (err err-not-found)))
      (contribution (unwrap! (map-get? contributions { campaign-id: campaign-id, contributor: tx-sender }) (err err-not-found)))
    )
    (asserts! (>= (current-time) (get deadline campaign)) (err err-deadline-passed))
    (asserts! (< (get raised campaign) (get goal campaign)) (err err-goal-not-reached))
    (match (as-contract (stx-transfer? (get amount contribution) tx-sender tx-sender))
      success
        (begin
          (map-delete contributions { campaign-id: campaign-id, contributor: tx-sender })
          (ok true)
        )
      error (err err-transfer-failed)
    )
  )
)

(define-read-only (get-total-campaigns)
  (var-get campaign-nonce))

(define-read-only (is-campaign-successful (campaign-id uint))
  (match (get-campaign-details campaign-id)
    campaign (and 
              (>= (get raised campaign) (get goal campaign))
              (>= (current-time) (get deadline campaign)))
    false))

(define-read-only (get-campaign-time-left (campaign-id uint))
  (match (get-campaign-details campaign-id)
    campaign (let ((time-left (- (get deadline campaign) (current-time))))
              (if (< (current-time) (get deadline campaign))
                (ok time-left)
                (ok u0)))
    (err err-not-found)))

(define-read-only (get-campaign-progress (campaign-id uint))
  (match (get-campaign-details campaign-id)
    campaign (let ((progress (* (/ (get raised campaign) (get goal campaign)) u100)))
              (ok progress))
    (err err-not-found)))

;; Read-only function to get campaign description
(define-read-only (get-campaign-description (campaign-id uint))
  (map-get? campaign-descriptions { campaign-id: campaign-id }))

;; Public function to add campaign milestone
(define-public (add-campaign-milestone 
    (campaign-id uint) 
    (title (string-utf8 100))
    (description (string-utf8 500))
    (target-amount uint)
    (deadline uint))
    (let
        (
            (campaign (unwrap! (map-get? campaigns { campaign-id: campaign-id }) (err err-not-found)))
            (campaign-stat (default-to 
                { unique-contributors: u0, avg-contribution: u0, largest-contribution: u0, updates-count: u0 }
                (map-get? campaign-stats { campaign-id: campaign-id })))
        )
        ;; Verify caller is campaign owner
        (asserts! (is-eq tx-sender (get owner campaign)) (err err-owner-only))
        ;; Verify campaign is still active
        (asserts! (< (current-time) (get deadline campaign)) (err err-deadline-passed))
        
        (ok (map-set campaign-milestones
            { campaign-id: campaign-id, milestone-id: (get updates-count campaign-stat) }
            {
                title: title,
                description: description,
                target-amount: target-amount,
                completed: false,
                deadline: deadline
            }))
    )
)

;; Public function to post campaign update
(define-public (post-campaign-update 
    (campaign-id uint) 
    (title (string-utf8 100))
    (content (string-utf8 1000)))
    (let
        (
            (campaign (unwrap! (map-get? campaigns { campaign-id: campaign-id }) (err err-not-found)))
            (current-stats (default-to { unique-contributors: u0, avg-contribution: u0, largest-contribution: u0, updates-count: u0 } 
                (map-get? campaign-stats { campaign-id: campaign-id })))
        )
        ;; Verify caller is campaign owner
        (asserts! (is-eq tx-sender (get owner campaign)) (err err-owner-only))
        
        ;; Add update
        (map-set campaign-updates
            { campaign-id: campaign-id, update-id: (get updates-count current-stats) }
            {
                title: title,
                content: content,
                timestamp: (current-time)
            })
        
        ;; Update stats
        (ok (map-set campaign-stats
            { campaign-id: campaign-id }
            (merge current-stats { updates-count: (+ (get updates-count current-stats) u1) })))
    )
)


;; Public function to report campaign
(define-public (report-campaign 
    (campaign-id uint) 
    (reason (string-utf8 500)))
    (let
        (
            (campaign (unwrap! (map-get? campaigns { campaign-id: campaign-id }) (err err-not-found)))
        )
        ;; Check if already reported by this user
        (asserts! (is-none (map-get? campaign-reports { campaign-id: campaign-id, reporter: tx-sender })) (err err-already-reported))
        
        (ok (map-set campaign-reports
            { campaign-id: campaign-id, reporter: tx-sender }
            {
                reason: reason,
                timestamp: (current-time),
                status: "PENDING"
            }))
    )
)

;; Public function to update minimum contribution
(define-public (update-minimum-contribution (new-minimum uint))
    (begin
        (asserts! (is-owner) (err err-owner-only))
        (var-set minimum-contribution new-minimum)
        (ok true)
    )
)

;; Public function to update platform fee
(define-public (update-platform-fee (new-fee uint))
    (begin
        (asserts! (is-owner) (err err-owner-only))
        (asserts! (<= new-fee u1000) (err err-invalid-amount)) ;; Max 10%
        (var-set platform-fee-percentage new-fee)
        (ok true)
    )
)


;; Read-only function to get campaign update
(define-read-only (get-campaign-update (campaign-id uint) (update-id uint))
    (map-get? campaign-updates { campaign-id: campaign-id, update-id: update-id })
)

;; Read-only function to get campaign report status
(define-read-only (get-campaign-report-status (campaign-id uint) (reporter principal))
    (map-get? campaign-reports { campaign-id: campaign-id, reporter: reporter })
)

;; Public function to mark milestone as completed
(define-public (complete-milestone (campaign-id uint) (milestone-id uint))
    (let
        (
            (campaign (unwrap! (map-get? campaigns { campaign-id: campaign-id }) (err err-not-found)))
            (milestone (unwrap! (map-get? campaign-milestones { campaign-id: campaign-id, milestone-id: milestone-id }) (err err-milestone-not-found)))
        )
        (asserts! (is-eq tx-sender (get owner campaign)) (err err-owner-only))
        
        (ok (map-set campaign-milestones
            { campaign-id: campaign-id, milestone-id: milestone-id }
            (merge milestone { completed: true })))
    )
)