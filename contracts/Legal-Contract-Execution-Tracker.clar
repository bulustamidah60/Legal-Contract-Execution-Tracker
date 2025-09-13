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
