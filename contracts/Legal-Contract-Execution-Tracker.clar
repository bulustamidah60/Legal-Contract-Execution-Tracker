(define-constant contract-owner tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-CONTRACT (err u101))
(define-constant ERR-INVALID-MILESTONE (err u102))
(define-constant ERR-ALREADY-COMPLETED (err u103))
(define-constant ERR-NOT-FOUND (err u104))

(define-data-var contract-counter uint u0)

(define-map legal-contracts
    uint 
    {
        creator: principal,
        party-a: principal,
        party-b: principal,
        start-date: uint,
        end-date: uint,
        status: (string-ascii 20),
        value: uint
    }
)

(define-map contract-milestones
    { contract-id: uint, milestone-id: uint }
    {
        description: (string-ascii 100),
        deadline: uint,
        completed: bool,
        verified: bool,
        value: uint
    }
)

(define-public (create-contract 
    (party-a principal)
    (party-b principal)
    (start-date uint)
    (end-date uint)
    (value uint))
    (let ((contract-id (+ (var-get contract-counter) u1)))
        (try! (stx-transfer? value tx-sender (as-contract tx-sender)))
        (map-set legal-contracts contract-id
            {
                creator: tx-sender,
                party-a: party-a,
                party-b: party-b,
                start-date: start-date,
                end-date: end-date,
                status: "ACTIVE",
                value: value
            }
        )
        (var-set contract-counter contract-id)
        (ok contract-id)
    )
)

;; This function was replaced by the updated add-milestone function below
;; (define-public (add-milestone 
;;     (contract-id uint)
;;     (description (string-ascii 100))
;;     (deadline uint)
;;     (value uint))
;;     (let ((contract (unwrap! (map-get? legal-contracts contract-id) ERR-NOT-FOUND)))
;;         (asserts! (is-eq tx-sender (get creator contract)) ERR-NOT-AUTHORIZED)
;;         (map-set contract-milestones 
;;             { contract-id: contract-id, milestone-id: u1 }
;;             {
;;                 description: description,
;;                 deadline: deadline,
;;                 completed: false,
;;                 verified: false,
;;                 value: value
;;             }
;;         )
;;         (ok true)
;;     )
;; )

(define-public (complete-milestone
    (contract-id uint)
    (milestone-id uint))
    (let (
        (contract (unwrap! (map-get? legal-contracts contract-id) ERR-NOT-FOUND))
        (milestone (unwrap! (map-get? contract-milestones { contract-id: contract-id, milestone-id: milestone-id }) ERR-NOT-FOUND))
    )
        (asserts! (or (is-eq tx-sender (get party-a contract)) (is-eq tx-sender (get party-b contract))) ERR-NOT-AUTHORIZED)
        (asserts! (not (get completed milestone)) ERR-ALREADY-COMPLETED)
        (map-set contract-milestones
            { contract-id: contract-id, milestone-id: milestone-id }
            (merge milestone { completed: true })
        )
        (ok true)
    )
)

(define-public (verify-milestone
    (contract-id uint)
    (milestone-id uint))
    (let (
        (contract (unwrap! (map-get? legal-contracts contract-id) ERR-NOT-FOUND))
        (milestone (unwrap! (map-get? contract-milestones { contract-id: contract-id, milestone-id: milestone-id }) ERR-NOT-FOUND))
    )
        (asserts! (is-eq tx-sender (get creator contract)) ERR-NOT-AUTHORIZED)
        (asserts! (get completed milestone) ERR-INVALID-MILESTONE)
        (map-set contract-milestones
            { contract-id: contract-id, milestone-id: milestone-id }
            (merge milestone { verified: true })
        )
        (try! (stx-transfer? (get value milestone) (as-contract tx-sender) (get party-b contract)))
        (ok true)
    )
)

(define-read-only (get-contract (contract-id uint))
    (ok (unwrap! (map-get? legal-contracts contract-id) ERR-NOT-FOUND))
)

(define-read-only (get-milestone (contract-id uint) (milestone-id uint))
    (ok (unwrap! (map-get? contract-milestones { contract-id: contract-id, milestone-id: milestone-id }) ERR-NOT-FOUND))
)

(define-map contract-milestone-counters uint uint)

(define-public (add-milestone 
    (contract-id uint)
    (description (string-ascii 100))
    (deadline uint)
    (value uint))
    (let (
        (contract (unwrap! (map-get? legal-contracts contract-id) ERR-NOT-FOUND))
        (current-milestone-count (default-to u0 (map-get? contract-milestone-counters contract-id)))
        (new-milestone-id (+ current-milestone-count u1))
    )
        (asserts! (is-eq tx-sender (get creator contract)) ERR-NOT-AUTHORIZED)
        (map-set contract-milestones 
            { contract-id: contract-id, milestone-id: new-milestone-id }
            {
                description: description,
                deadline: deadline,
                completed: false,
                verified: false,
                value: value
            }
        )
        (map-set contract-milestone-counters contract-id new-milestone-id)
        (ok new-milestone-id)
    )
)

(define-read-only (get-milestone-count (contract-id uint))
    (ok (default-to u0 (map-get? contract-milestone-counters contract-id)))
)

(define-read-only (get-all-milestones (contract-id uint))
    (let ((milestone-count (default-to u0 (map-get? contract-milestone-counters contract-id))))
        (ok milestone-count)
    )
)

(define-constant ERR-DISPUTE-EXISTS (err u105))
(define-constant ERR-NO-DISPUTE (err u106))
(define-constant ERR-DISPUTE-RESOLVED (err u107))
(define-constant ERR-REFINANCE-EXISTS (err u108))
(define-constant ERR-NO-REFINANCE (err u109))
(define-constant ERR-ALREADY-APPROVED (err u110))
(define-constant ERR-INSUFFICIENT-FUNDS (err u111))
(define-constant ERR-CONTRACT-EXPIRED (err u112))
(define-constant ERR-NOTIFICATION-EXISTS (err u113))
(define-constant ERR-AMENDMENT-EXISTS (err u114))
(define-constant ERR-NO-AMENDMENT (err u115))
(define-constant ERR-AMENDMENT-EXECUTED (err u116))
(define-constant ERR-INVALID-PERCENTAGE (err u117))
(define-constant ERR-PAYMENT-RELEASED (err u118))
(define-constant ERR-INSUFFICIENT-BALANCE (err u119))

(define-map milestone-disputes
    { contract-id: uint, milestone-id: uint }
    {
        initiator: principal,
        reason: (string-ascii 200),
        status: (string-ascii 20),
        resolution: (string-ascii 200),
        created-at: uint
    }
)

(define-map dispute-votes
    { contract-id: uint, milestone-id: uint, voter: principal }
    {
        vote: bool,
        timestamp: uint
    }
)

(define-public (initiate-dispute
    (contract-id uint)
    (milestone-id uint)
    (reason (string-ascii 200)))
    (let (
        (contract (unwrap! (map-get? legal-contracts contract-id) ERR-NOT-FOUND))
        (milestone (unwrap! (map-get? contract-milestones { contract-id: contract-id, milestone-id: milestone-id }) ERR-NOT-FOUND))
        (existing-dispute (map-get? milestone-disputes { contract-id: contract-id, milestone-id: milestone-id }))
    )
        (asserts! (or (is-eq tx-sender (get party-a contract)) (is-eq tx-sender (get party-b contract))) ERR-NOT-AUTHORIZED)
        (asserts! (is-none existing-dispute) ERR-DISPUTE-EXISTS)
        (map-set milestone-disputes
            { contract-id: contract-id, milestone-id: milestone-id }
            {
                initiator: tx-sender,
                reason: reason,
                status: "OPEN",
                resolution: "",
                created-at: stacks-block-height
            }
        )
        (ok true)
    )
)

(define-public (vote-on-dispute
    (contract-id uint)
    (milestone-id uint)
    (vote bool))
    (let (
        (contract (unwrap! (map-get? legal-contracts contract-id) ERR-NOT-FOUND))
        (dispute (unwrap! (map-get? milestone-disputes { contract-id: contract-id, milestone-id: milestone-id }) ERR-NO-DISPUTE))
    )
        (asserts! (or (is-eq tx-sender (get party-a contract)) (is-eq tx-sender (get party-b contract))) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status dispute) "OPEN") ERR-DISPUTE-RESOLVED)
        (map-set dispute-votes
            { contract-id: contract-id, milestone-id: milestone-id, voter: tx-sender }
            {
                vote: vote,
                timestamp: stacks-block-height
            }
        )
        (ok true)
    )
)

(define-public (resolve-dispute
    (contract-id uint)
    (milestone-id uint)
    (resolution (string-ascii 200)))
    (let (
        (contract (unwrap! (map-get? legal-contracts contract-id) ERR-NOT-FOUND))
        (dispute (unwrap! (map-get? milestone-disputes { contract-id: contract-id, milestone-id: milestone-id }) ERR-NO-DISPUTE))
        (milestone (unwrap! (map-get? contract-milestones { contract-id: contract-id, milestone-id: milestone-id }) ERR-NOT-FOUND))
    )
        (asserts! (is-eq tx-sender (get creator contract)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status dispute) "OPEN") ERR-DISPUTE-RESOLVED)
        (map-set milestone-disputes
            { contract-id: contract-id, milestone-id: milestone-id }
            (merge dispute { status: "RESOLVED", resolution: resolution })
        )
        (try! (stx-transfer? (get value milestone) (as-contract tx-sender) (get creator contract)))
        (ok true)
    )
)

(define-read-only (get-dispute (contract-id uint) (milestone-id uint))
    (ok (map-get? milestone-disputes { contract-id: contract-id, milestone-id: milestone-id }))
)

(define-read-only (get-dispute-vote (contract-id uint) (milestone-id uint) (voter principal))
    (ok (map-get? dispute-votes { contract-id: contract-id, milestone-id: milestone-id, voter: voter }))
)

(define-map milestone-refinance-requests
    { contract-id: uint, milestone-id: uint }
    {
        requester: principal,
        new-value: uint,
        reason: (string-ascii 200),
        party-a-approved: bool,
        party-b-approved: bool,
        created-at: uint,
        status: (string-ascii 20)
    }
)

(define-public (request-milestone-refinance
    (contract-id uint)
    (milestone-id uint)
    (new-value uint)
    (reason (string-ascii 200)))
    (let (
        (contract (unwrap! (map-get? legal-contracts contract-id) ERR-NOT-FOUND))
        (milestone (unwrap! (map-get? contract-milestones { contract-id: contract-id, milestone-id: milestone-id }) ERR-NOT-FOUND))
        (existing-refinance (map-get? milestone-refinance-requests { contract-id: contract-id, milestone-id: milestone-id }))
    )
        (asserts! (or (is-eq tx-sender (get party-a contract)) (is-eq tx-sender (get party-b contract))) ERR-NOT-AUTHORIZED)
        (asserts! (not (get completed milestone)) ERR-ALREADY-COMPLETED)
        (asserts! (is-none existing-refinance) ERR-REFINANCE-EXISTS)
        (map-set milestone-refinance-requests
            { contract-id: contract-id, milestone-id: milestone-id }
            {
                requester: tx-sender,
                new-value: new-value,
                reason: reason,
                party-a-approved: (is-eq tx-sender (get party-a contract)),
                party-b-approved: (is-eq tx-sender (get party-b contract)),
                created-at: stacks-block-height,
                status: "PENDING"
            }
        )
        (ok true)
    )
)

(define-public (approve-refinance
    (contract-id uint)
    (milestone-id uint))
    (let (
        (contract (unwrap! (map-get? legal-contracts contract-id) ERR-NOT-FOUND))
        (refinance (unwrap! (map-get? milestone-refinance-requests { contract-id: contract-id, milestone-id: milestone-id }) ERR-NO-REFINANCE))
        (is-party-a (is-eq tx-sender (get party-a contract)))
        (is-party-b (is-eq tx-sender (get party-b contract)))
        (already-approved-a (get party-a-approved refinance))
        (already-approved-b (get party-b-approved refinance))
    )
        (asserts! (or is-party-a is-party-b) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status refinance) "PENDING") ERR-DISPUTE-RESOLVED)
        (asserts! (not (and is-party-a already-approved-a)) ERR-ALREADY-APPROVED)
        (asserts! (not (and is-party-b already-approved-b)) ERR-ALREADY-APPROVED)
        (let ((updated-refinance (merge refinance {
            party-a-approved: (or already-approved-a is-party-a),
            party-b-approved: (or already-approved-b is-party-b)
        })))
            (map-set milestone-refinance-requests
                { contract-id: contract-id, milestone-id: milestone-id }
                updated-refinance
            )
            (if (and (get party-a-approved updated-refinance) (get party-b-approved updated-refinance))
                (begin 
                    (try! (execute-refinance contract-id milestone-id))
                    (ok true)
                )
                (ok true)
            )
        )
    )
)

(define-private (execute-refinance (contract-id uint) (milestone-id uint))
    (let (
        (refinance (unwrap! (map-get? milestone-refinance-requests { contract-id: contract-id, milestone-id: milestone-id }) ERR-NO-REFINANCE))
        (milestone (unwrap! (map-get? contract-milestones { contract-id: contract-id, milestone-id: milestone-id }) ERR-NOT-FOUND))
        (current-value (get value milestone))
        (new-value (get new-value refinance))
        (value-difference (if (> new-value current-value) (- new-value current-value) u0))
        (refund-amount (if (> current-value new-value) (- current-value new-value) u0))
        (contract (unwrap! (map-get? legal-contracts contract-id) ERR-NOT-FOUND))
    )
        (if (> new-value current-value)
            (try! (stx-transfer? value-difference (get party-a contract) (as-contract tx-sender)))
            (if (> current-value new-value)
                (try! (stx-transfer? refund-amount (as-contract tx-sender) (get party-a contract)))
                true
            )
        )
        (map-set contract-milestones
            { contract-id: contract-id, milestone-id: milestone-id }
            (merge milestone { value: new-value })
        )
        (map-set milestone-refinance-requests
            { contract-id: contract-id, milestone-id: milestone-id }
            (merge refinance { status: "APPROVED" })
        )
        (ok true)
    )
)

(define-read-only (get-refinance-request (contract-id uint) (milestone-id uint))
    (ok (map-get? milestone-refinance-requests { contract-id: contract-id, milestone-id: milestone-id }))
)

(define-map contract-expiry-notifications
    uint
    {
        notification-blocks-before: uint,
        notification-sent: bool,
        last-checked: uint
    }
)

(define-public (set-expiry-notification
    (contract-id uint)
    (blocks-before-expiry uint))
    (let (
        (contract (unwrap! (map-get? legal-contracts contract-id) ERR-NOT-FOUND))
        (existing-notification (map-get? contract-expiry-notifications contract-id))
    )
        (asserts! (or (is-eq tx-sender (get party-a contract)) (is-eq tx-sender (get party-b contract))) ERR-NOT-AUTHORIZED)
        (asserts! (is-none existing-notification) ERR-NOTIFICATION-EXISTS)
        (map-set contract-expiry-notifications contract-id
            {
                notification-blocks-before: blocks-before-expiry,
                notification-sent: false,
                last-checked: stacks-block-height
            }
        )
        (ok true)
    )
)

(define-public (check-contract-expiry (contract-id uint))
    (let (
        (contract (unwrap! (map-get? legal-contracts contract-id) ERR-NOT-FOUND))
        (current-height stacks-block-height)
        (end-date (get end-date contract))
        (current-status (get status contract))
    )
        (if (and (>= current-height end-date) (is-eq current-status "ACTIVE"))
            (begin
                (map-set legal-contracts contract-id
                    (merge contract { status: "EXPIRED" })
                )
                (ok "EXPIRED")
            )
            (if (is-eq current-status "ACTIVE")
                (ok "ACTIVE")
                (ok current-status)
            )
        )
    )
)

(define-public (update-notification-status (contract-id uint))
    (let (
        (contract (unwrap! (map-get? legal-contracts contract-id) ERR-NOT-FOUND))
        (notification (map-get? contract-expiry-notifications contract-id))
        (current-height stacks-block-height)
        (end-date (get end-date contract))
    )
        (match notification
            notif
            (let (
                (notification-threshold (- end-date (get notification-blocks-before notif)))
                (should-notify (and (>= current-height notification-threshold) (not (get notification-sent notif))))
            )
                (if should-notify
                    (begin
                        (map-set contract-expiry-notifications contract-id
                            (merge notif { notification-sent: true, last-checked: current-height })
                        )
                        (ok true)
                    )
                    (begin
                        (map-set contract-expiry-notifications contract-id
                            (merge notif { last-checked: current-height })
                        )
                        (ok false)
                    )
                )
            )
            (ok false)
        )
    )
)

(define-read-only (get-expiry-notification (contract-id uint))
    (ok (map-get? contract-expiry-notifications contract-id))
)

(define-read-only (is-contract-expired (contract-id uint))
    (let (
        (contract (unwrap! (map-get? legal-contracts contract-id) ERR-NOT-FOUND))
        (current-height stacks-block-height)
        (end-date (get end-date contract))
    )
        (ok (>= current-height end-date))
    )
)

(define-map contract-amendments
    uint
    {
        proposer: principal,
        new-party-a: (optional principal),
        new-party-b: (optional principal),
        new-start-date: (optional uint),
        new-end-date: (optional uint),
        new-value: (optional uint),
        reason: (string-ascii 200),
        party-a-approved: bool,
        party-b-approved: bool,
        creator-approved: bool,
        status: (string-ascii 20),
        created-at: uint
    }
)

(define-public (propose-contract-amendment
    (contract-id uint)
    (new-party-a (optional principal))
    (new-party-b (optional principal))
    (new-start-date (optional uint))
    (new-end-date (optional uint))
    (new-value (optional uint))
    (reason (string-ascii 200)))
    (let (
        (contract (unwrap! (map-get? legal-contracts contract-id) ERR-NOT-FOUND))
        (existing-amendment (map-get? contract-amendments contract-id))
    )
        (asserts! (or 
            (is-eq tx-sender (get party-a contract)) 
            (is-eq tx-sender (get party-b contract))
            (is-eq tx-sender (get creator contract))
        ) ERR-NOT-AUTHORIZED)
        (asserts! (is-none existing-amendment) ERR-AMENDMENT-EXISTS)
        (map-set contract-amendments contract-id
            {
                proposer: tx-sender,
                new-party-a: new-party-a,
                new-party-b: new-party-b,
                new-start-date: new-start-date,
                new-end-date: new-end-date,
                new-value: new-value,
                reason: reason,
                party-a-approved: (is-eq tx-sender (get party-a contract)),
                party-b-approved: (is-eq tx-sender (get party-b contract)),
                creator-approved: (is-eq tx-sender (get creator contract)),
                status: "PENDING",
                created-at: stacks-block-height
            }
        )
        (ok true)
    )
)

(define-public (approve-contract-amendment (contract-id uint))
    (let (
        (contract (unwrap! (map-get? legal-contracts contract-id) ERR-NOT-FOUND))
        (amendment (unwrap! (map-get? contract-amendments contract-id) ERR-NO-AMENDMENT))
        (is-party-a (is-eq tx-sender (get party-a contract)))
        (is-party-b (is-eq tx-sender (get party-b contract)))
        (is-creator (is-eq tx-sender (get creator contract)))
    )
        (asserts! (or is-party-a is-party-b is-creator) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status amendment) "PENDING") ERR-AMENDMENT-EXECUTED)
        (let ((updated-amendment (merge amendment {
            party-a-approved: (or (get party-a-approved amendment) is-party-a),
            party-b-approved: (or (get party-b-approved amendment) is-party-b),
            creator-approved: (or (get creator-approved amendment) is-creator)
        })))
            (map-set contract-amendments contract-id updated-amendment)
            (if (and 
                (get party-a-approved updated-amendment)
                (get party-b-approved updated-amendment)
                (get creator-approved updated-amendment)
            )
                (begin
                    (try! (execute-contract-amendment contract-id))
                    (ok true)
                )
                (ok true)
            )
        )
    )
)

(define-private (execute-contract-amendment (contract-id uint))
    (let (
        (contract (unwrap! (map-get? legal-contracts contract-id) ERR-NOT-FOUND))
        (amendment (unwrap! (map-get? contract-amendments contract-id) ERR-NO-AMENDMENT))
        (current-value (get value contract))
        (new-value-opt (get new-value amendment))
    )
        (match new-value-opt
            new-val
            (if (> new-val current-value)
                (try! (stx-transfer? (- new-val current-value) (get party-a contract) (as-contract tx-sender)))
                (if (> current-value new-val)
                    (try! (stx-transfer? (- current-value new-val) (as-contract tx-sender) (get party-a contract)))
                    true
                )
            )
            true
        )
        (let (
            (updated-contract (merge contract {
                party-a: (default-to (get party-a contract) (get new-party-a amendment)),
                party-b: (default-to (get party-b contract) (get new-party-b amendment)),
                start-date: (default-to (get start-date contract) (get new-start-date amendment)),
                end-date: (default-to (get end-date contract) (get new-end-date amendment)),
                value: (default-to (get value contract) (get new-value amendment))
            }))
        )
            (map-set legal-contracts contract-id updated-contract)
            (map-set contract-amendments contract-id
                (merge amendment { status: "EXECUTED" })
            )
            (ok true)
        )
    )
)

(define-public (reject-contract-amendment (contract-id uint))
    (let (
        (contract (unwrap! (map-get? legal-contracts contract-id) ERR-NOT-FOUND))
        (amendment (unwrap! (map-get? contract-amendments contract-id) ERR-NO-AMENDMENT))
    )
        (asserts! (or 
            (is-eq tx-sender (get party-a contract)) 
            (is-eq tx-sender (get party-b contract))
            (is-eq tx-sender (get creator contract))
        ) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status amendment) "PENDING") ERR-AMENDMENT-EXECUTED)
        (map-set contract-amendments contract-id
            (merge amendment { status: "REJECTED" })
        )
        (ok true)
    )
)

(define-read-only (get-contract-amendment (contract-id uint))
    (ok (map-get? contract-amendments contract-id))
)

(define-map milestone-payment-schedules
    { contract-id: uint, milestone-id: uint, payment-stage: uint }
    {
        percentage: uint,
        released: bool,
        released-at: (optional uint),
        released-to: (optional principal),
        amount: uint
    }
)

(define-map milestone-payment-stage-counters
    { contract-id: uint, milestone-id: uint }
    uint
)

(define-private (store-payment-stage
    (stage-data { contract-id: uint, milestone-id: uint, milestone-value: uint, percentage: uint, stage-num: uint })
    (accumulated uint))
    (let (
        (amount (/ (* (get milestone-value stage-data) (get percentage stage-data)) u100))
    )
        (map-set milestone-payment-schedules
            { contract-id: (get contract-id stage-data), milestone-id: (get milestone-id stage-data), payment-stage: (get stage-num stage-data) }
            {
                percentage: (get percentage stage-data),
                released: false,
                released-at: none,
                released-to: none,
                amount: amount
            }
        )
        (+ accumulated u1)
    )
)

(define-public (configure-milestone-payment-schedule
    (contract-id uint)
    (milestone-id uint)
    (percentages (list 10 uint)))
    (let (
        (contract (unwrap! (map-get? legal-contracts contract-id) ERR-NOT-FOUND))
        (milestone (unwrap! (map-get? contract-milestones { contract-id: contract-id, milestone-id: milestone-id }) ERR-NOT-FOUND))
        (total-percentage (fold + percentages u0))
        (milestone-value (get value milestone))
        (stage-count (len percentages))
    )
        (asserts! (is-eq tx-sender (get creator contract)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq total-percentage u100) ERR-INVALID-PERCENTAGE)
        (asserts! (not (get completed milestone)) ERR-ALREADY-COMPLETED)
        (map-set milestone-payment-stage-counters
            { contract-id: contract-id, milestone-id: milestone-id }
            stage-count
        )
        (asserts! (> stage-count u0) ERR-INVALID-PERCENTAGE)
        (if (>= stage-count u1)
            (begin
                (map-set milestone-payment-schedules
                    { contract-id: contract-id, milestone-id: milestone-id, payment-stage: u1 }
                    {
                        percentage: (default-to u0 (element-at percentages u0)),
                        released: false,
                        released-at: none,
                        released-to: none,
                        amount: (/ (* milestone-value (default-to u0 (element-at percentages u0))) u100)
                    }
                )
                (if (>= stage-count u2)
                    (begin
                        (map-set milestone-payment-schedules
                            { contract-id: contract-id, milestone-id: milestone-id, payment-stage: u2 }
                            {
                                percentage: (default-to u0 (element-at percentages u1)),
                                released: false,
                                released-at: none,
                                released-to: none,
                                amount: (/ (* milestone-value (default-to u0 (element-at percentages u1))) u100)
                            }
                        )
                        (if (>= stage-count u3)
                            (begin
                                (map-set milestone-payment-schedules
                                    { contract-id: contract-id, milestone-id: milestone-id, payment-stage: u3 }
                                    {
                                        percentage: (default-to u0 (element-at percentages u2)),
                                        released: false,
                                        released-at: none,
                                        released-to: none,
                                        amount: (/ (* milestone-value (default-to u0 (element-at percentages u2))) u100)
                                    }
                                )
                                (if (>= stage-count u4)
                                    (begin
                                        (map-set milestone-payment-schedules
                                            { contract-id: contract-id, milestone-id: milestone-id, payment-stage: u4 }
                                            {
                                                percentage: (default-to u0 (element-at percentages u3)),
                                                released: false,
                                                released-at: none,
                                                released-to: none,
                                                amount: (/ (* milestone-value (default-to u0 (element-at percentages u3))) u100)
                                            }
                                        )
                                        (if (>= stage-count u5)
                                            (map-set milestone-payment-schedules
                                                { contract-id: contract-id, milestone-id: milestone-id, payment-stage: u5 }
                                                {
                                                    percentage: (default-to u0 (element-at percentages u4)),
                                                    released: false,
                                                    released-at: none,
                                                    released-to: none,
                                                    amount: (/ (* milestone-value (default-to u0 (element-at percentages u4))) u100)
                                                }
                                            )
                                            true
                                        )
                                    )
                                    true
                                )
                            )
                            true
                        )
                    )
                    true
                )
            )
            true
        )
        (ok true)
    )
)

(define-public (release-payment-stage
    (contract-id uint)
    (milestone-id uint)
    (payment-stage uint))
    (let (
        (contract (unwrap! (map-get? legal-contracts contract-id) ERR-NOT-FOUND))
        (milestone (unwrap! (map-get? contract-milestones { contract-id: contract-id, milestone-id: milestone-id }) ERR-NOT-FOUND))
        (payment (unwrap! (map-get? milestone-payment-schedules { contract-id: contract-id, milestone-id: milestone-id, payment-stage: payment-stage }) ERR-NOT-FOUND))
        (recipient (get party-b contract))
    )
        (asserts! (or (is-eq tx-sender (get creator contract)) (is-eq tx-sender (get party-a contract))) ERR-NOT-AUTHORIZED)
        (asserts! (not (get released payment)) ERR-PAYMENT-RELEASED)
        (try! (as-contract (stx-transfer? (get amount payment) tx-sender recipient)))
        (map-set milestone-payment-schedules
            { contract-id: contract-id, milestone-id: milestone-id, payment-stage: payment-stage }
            (merge payment { 
                released: true, 
                released-at: (some stacks-block-height),
                released-to: (some recipient)
            })
        )
        (ok true)
    )
)

(define-read-only (get-payment-stage
    (contract-id uint)
    (milestone-id uint)
    (payment-stage uint))
    (ok (map-get? milestone-payment-schedules { contract-id: contract-id, milestone-id: milestone-id, payment-stage: payment-stage }))
)

(define-read-only (get-milestone-payment-status
    (contract-id uint)
    (milestone-id uint))
    (let (
        (stage-count (default-to u0 (map-get? milestone-payment-stage-counters { contract-id: contract-id, milestone-id: milestone-id })))
    )
        (ok { total-stages: stage-count })
    )
)
