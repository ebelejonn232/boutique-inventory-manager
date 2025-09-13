;; title: inventory-management
;; version: 1.0.0
;; summary: Comprehensive inventory management system for boutiques
;; description: Provides stock tracking, purchase ordering, sales analytics, and trend forecasting for independent retailers

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-PRODUCT-NOT-FOUND (err u404))
(define-constant ERR-INSUFFICIENT-STOCK (err u405))
(define-constant ERR-INVALID-AMOUNT (err u400))
(define-constant ERR-ORDER-NOT-FOUND (err u406))
(define-constant ERR-ORDER-ALREADY-RECEIVED (err u407))
(define-constant ERR-INVALID-PRICE (err u408))

;; Data variables
(define-data-var contract-owner principal tx-sender)
(define-data-var next-product-id uint u1)
(define-data-var next-order-id uint u1)
(define-data-var total-revenue uint u0)

;; Product information map
(define-map products
  { product-id: uint }
  {
    name: (string-ascii 50),
    category: (string-ascii 30),
    price: uint,
    cost: uint,
    current-stock: uint,
    reorder-point: uint,
    total-sold: uint,
    created-at: uint,
    active: bool
  }
)

;; Purchase orders map
(define-map purchase-orders
  { order-id: uint }
  {
    supplier: (string-ascii 50),
    product-id: uint,
    quantity: uint,
    cost-per-unit: uint,
    total-cost: uint,
    status: (string-ascii 20),
    created-at: uint,
    received-at: (optional uint)
  }
)

;; Sales transactions map for analytics
(define-map sales-transactions
  { tx-id: uint }
  {
    product-id: uint,
    quantity: uint,
    unit-price: uint,
    total-amount: uint,
    timestamp: uint
  }
)

(define-data-var next-tx-id uint u1)

;; Store authorized managers
(define-map authorized-managers principal bool)

;; Public functions

;; Initialize contract with owner as first authorized manager
(define-public (initialize)
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (ok (map-set authorized-managers tx-sender true))
  )
)

;; Add authorized manager
(define-public (add-manager (manager principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (ok (map-set authorized-managers manager true))
  )
)

;; Remove authorized manager
(define-public (remove-manager (manager principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (ok (map-delete authorized-managers manager))
  )
)

;; Add new product to inventory
(define-public (add-product (name (string-ascii 50)) (category (string-ascii 30)) (price uint) (cost uint) (initial-stock uint) (reorder-point uint))
  (let 
    (
      (product-id (var-get next-product-id))
    )
    (asserts! (is-authorized tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (> price u0) ERR-INVALID-PRICE)
    (asserts! (> cost u0) ERR-INVALID-PRICE)
    
    (map-set products 
      { product-id: product-id }
      {
        name: name,
        category: category,
        price: price,
        cost: cost,
        current-stock: initial-stock,
        reorder-point: reorder-point,
        total-sold: u0,
        created-at: stacks-block-height,
        active: true
      }
    )
    (var-set next-product-id (+ product-id u1))
    (ok product-id)
  )
)

;; Update product information
(define-public (update-product (product-id uint) (name (string-ascii 50)) (category (string-ascii 30)) (price uint) (cost uint) (reorder-point uint))
  (begin
    (asserts! (is-authorized tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (> price u0) ERR-INVALID-PRICE)
    (asserts! (> cost u0) ERR-INVALID-PRICE)
    
    (match (map-get? products { product-id: product-id })
      product
      (begin
        (map-set products
          { product-id: product-id }
          (merge product {
            name: name,
            category: category,
            price: price,
            cost: cost,
            reorder-point: reorder-point
          })
        )
        (ok true)
      )
      ERR-PRODUCT-NOT-FOUND
    )
  )
)

;; Create purchase order
(define-public (create-purchase-order (supplier (string-ascii 50)) (product-id uint) (quantity uint) (cost-per-unit uint))
  (let 
    (
      (order-id (var-get next-order-id))
      (total-cost (* quantity cost-per-unit))
    )
    (asserts! (is-authorized tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (> quantity u0) ERR-INVALID-AMOUNT)
    (asserts! (> cost-per-unit u0) ERR-INVALID-PRICE)
    (asserts! (is-some (map-get? products { product-id: product-id })) ERR-PRODUCT-NOT-FOUND)
    
    (map-set purchase-orders
      { order-id: order-id }
      {
        supplier: supplier,
        product-id: product-id,
        quantity: quantity,
        cost-per-unit: cost-per-unit,
        total-cost: total-cost,
        status: "pending",
        created-at: stacks-block-height,
        received-at: none
      }
    )
    (var-set next-order-id (+ order-id u1))
    (ok order-id)
  )
)

;; Receive purchase order and update inventory
(define-public (receive-purchase-order (order-id uint))
  (begin
    (asserts! (is-authorized tx-sender) ERR-NOT-AUTHORIZED)
    
    (match (map-get? purchase-orders { order-id: order-id })
      order
      (begin
        (asserts! (is-eq (get status order) "pending") ERR-ORDER-ALREADY-RECEIVED)
        
        (match (map-get? products { product-id: (get product-id order) })
          product
          (begin
            ;; Update product stock
            (map-set products
              { product-id: (get product-id order) }
              (merge product {
                current-stock: (+ (get current-stock product) (get quantity order))
              })
            )
            
            ;; Update order status
            (map-set purchase-orders
              { order-id: order-id }
              (merge order {
                status: "received",
                received-at: (some stacks-block-height)
              })
            )
            (ok true)
          )
          ERR-PRODUCT-NOT-FOUND
        )
      )
      ERR-ORDER-NOT-FOUND
    )
  )
)

;; Record a sale
(define-public (record-sale (product-id uint) (quantity uint))
  (let 
    (
      (tx-id (var-get next-tx-id))
    )
    (asserts! (is-authorized tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (> quantity u0) ERR-INVALID-AMOUNT)
    
    (match (map-get? products { product-id: product-id })
      product
      (begin
        (asserts! (>= (get current-stock product) quantity) ERR-INSUFFICIENT-STOCK)
        (asserts! (get active product) ERR-PRODUCT-NOT-FOUND)
        
        (let 
          (
            (unit-price (get price product))
            (total-amount (* quantity unit-price))
            (new-stock (- (get current-stock product) quantity))
            (new-total-sold (+ (get total-sold product) quantity))
          )
          
          ;; Update product stock and sales count
          (map-set products
            { product-id: product-id }
            (merge product {
              current-stock: new-stock,
              total-sold: new-total-sold
            })
          )
          
          ;; Record transaction
          (map-set sales-transactions
            { tx-id: tx-id }
            {
              product-id: product-id,
              quantity: quantity,
              unit-price: unit-price,
              total-amount: total-amount,
              timestamp: stacks-block-height
            }
          )
          
          ;; Update total revenue
          (var-set total-revenue (+ (var-get total-revenue) total-amount))
          (var-set next-tx-id (+ tx-id u1))
          
          (ok {
            transaction-id: tx-id,
            total-amount: total-amount,
            remaining-stock: new-stock
          })
        )
      )
      ERR-PRODUCT-NOT-FOUND
    )
  )
)

;; Manually adjust stock (for returns, damage, etc.)
(define-public (adjust-stock (product-id uint) (new-quantity uint))
  (begin
    (asserts! (is-authorized tx-sender) ERR-NOT-AUTHORIZED)
    
    (match (map-get? products { product-id: product-id })
      product
      (begin
        (map-set products
          { product-id: product-id }
          (merge product {
            current-stock: new-quantity
          })
        )
        (ok true)
      )
      ERR-PRODUCT-NOT-FOUND
    )
  )
)

;; Deactivate product
(define-public (deactivate-product (product-id uint))
  (begin
    (asserts! (is-authorized tx-sender) ERR-NOT-AUTHORIZED)
    
    (match (map-get? products { product-id: product-id })
      product
      (begin
        (map-set products
          { product-id: product-id }
          (merge product { active: false })
        )
        (ok true)
      )
      ERR-PRODUCT-NOT-FOUND
    )
  )
)

;; Read-only functions

;; Get product information
(define-read-only (get-product (product-id uint))
  (map-get? products { product-id: product-id })
)

;; Get purchase order information
(define-read-only (get-purchase-order (order-id uint))
  (map-get? purchase-orders { order-id: order-id })
)

;; Get sales transaction information
(define-read-only (get-sales-transaction (tx-id uint))
  (map-get? sales-transactions { tx-id: tx-id })
)

;; Get total revenue
(define-read-only (get-total-revenue)
  (var-get total-revenue)
)

;; Check if user is authorized
(define-read-only (is-authorized (user principal))
  (default-to false (map-get? authorized-managers user))
)

;; Check if product needs reordering
(define-read-only (needs-reorder (product-id uint))
  (match (map-get? products { product-id: product-id })
    product
    (<= (get current-stock product) (get reorder-point product))
    false
  )
)

;; Get product profit margin
(define-read-only (get-profit-margin (product-id uint))
  (match (map-get? products { product-id: product-id })
    product
    (let 
      (
        (price (get price product))
        (cost (get cost product))
        (profit (- price cost))
      )
      (if (> price u0)
        (some (/ (* profit u100) price))
        none
      )
    )
    none
  )
)

;; Get current product count
(define-read-only (get-product-count)
  (- (var-get next-product-id) u1)
)

;; Get current order count
(define-read-only (get-order-count)
  (- (var-get next-order-id) u1)
)

;; Get total transactions count
(define-read-only (get-transaction-count)
  (- (var-get next-tx-id) u1)
)

;; Calculate total inventory value
(define-read-only (get-inventory-value (product-id uint))
  (match (map-get? products { product-id: product-id })
    product
    (some (* (get current-stock product) (get cost product)))
    none
  )
)

