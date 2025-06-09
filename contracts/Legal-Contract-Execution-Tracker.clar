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

(define-public (add-milestone 
    (contract-id uint)
    (description (string-ascii 100))
    (deadline uint)
    (value uint))
    (let ((contract (unwrap! (map-get? legal-contracts contract-id) ERR-NOT-FOUND)))
        (asserts! (is-eq tx-sender (get creator contract)) ERR-NOT-AUTHORIZED)
        (map-set contract-milestones 
            { contract-id: contract-id, milestone-id: u1 }
            {
                description: description,
                deadline: deadline,
                completed: false,
                verified: false,
                value: value
            }
        )
        (ok true)
    )
)

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
