#lang racket
(require redex)

(define-language E
  (τ num str)
  (e x
     number
     string
     (plus e e)
     (times e e)
     (cat e e)
     (len e)
     (let e x e))
  (x variable-not-otherwise-mentioned))

(define-extended-language E+Γ E
  [Γ · (x : τ Γ)])

(define-metafunction E+Γ
  [(different x_1 x_1) #f]
  [(different x_1 x_2) #t])

(define-judgment-form E+Γ
  #:mode (types I I O)
  #:contract (types Γ e τ)
  [-----------------
   (types (x : τ Γ) x τ)]
  [(types Γ x_1 τ_1)
   (side-condition (different x_1 x_2))
   ------------------------------------
   (types (x_2 : τ_2 Γ) x_1 τ_1)]
  [--------------------
   (types Γ string str)]
  [--------------------
   (types Γ number num)]
  [(types Γ e_1 num)
   (types Γ e_2 num)
   --------------------------
   (types Γ (plus e_1 e_2) num)]
  [(types Γ e_1 num)
   (types Γ e_2 num)
   ---------------------------
   (types Γ (times e_1 e_2) num)]
  [(types Γ e_1 str)
   (types Γ e_2 str)
   -------------------------
   (types Γ (cat e_1 e_2) str)]
  [(types Γ e str)
   ---------------------
   (types Γ (len e) num)]
  [(types Γ e_1 τ_1)
   (types (x : τ_1 Γ) e_2 τ_2)
   --------------------------
   (types Γ (let e_1 x e_2) τ_2)])

(module+ test
  (test-equal (judgment-holds (types · (plus 1 2) τ) τ) '(num))
  (test-equal (judgment-holds (types · (let 1 x (let 2 y (plus x y))) τ) τ) '(num)))

(module+ test
  (test-results))