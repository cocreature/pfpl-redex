#lang racket
(require redex)

(define-language E
  (τ ::= num str)
  (e ::=
     x
     N
     S
     (plus e e)
     (times e e)
     (cat e e)
     (len e)
     (let e x e))
  (N ::= number)
  (S ::= string)
  (x ::= variable-not-otherwise-mentioned)
  #:binding-forms
  (let e_1 x e_2 #:refers-to x))

(define-extended-language E+Γ E
  [Γ ::= · (x : τ Γ)])

(define-metafunction E+Γ
  [(different x_1 x_1) #f]
  [(different x_1 x_2) #t])

(define-judgment-form E+Γ
  #:mode (types I I I I O)
  #:contract (types Γ ⊢ e : τ)
  [-----------------
   (types (x : τ Γ) ⊢ x : τ)]

  [(types Γ ⊢ x_1 : τ_1)
   (side-condition (different x_1 x_2))
   ------------------------------------
   (types (x_2 : τ_2 Γ) ⊢ x_1 : τ_1)]

  [--------------------
   (types Γ ⊢ string : str)]

  [--------------------
   (types Γ ⊢ number : num)]

  [(types Γ ⊢ e_1 : num)
   (types Γ ⊢ e_2 : num)
   --------------------------
   (types Γ ⊢ (plus e_1 e_2) : num)]

  [(types Γ ⊢ e_1 : num)
   (types Γ ⊢ e_2 : num)
   ---------------------------
   (types Γ ⊢ (times e_1 e_2) : num)]

  [(types Γ ⊢ e_1 : str)
   (types Γ ⊢ e_2 : str)
   -------------------------
   (types Γ ⊢ (cat e_1 e_2) : str)]

  [(types Γ ⊢ e : str)
   ---------------------
   (types Γ ⊢ (len e) : num)]

  [(types Γ ⊢ e_1 : τ_1)
   (types (x : τ_1 Γ) ⊢ e_2 : τ_2)
   --------------------------
   (types Γ ⊢ (let e_1 x e_2) : τ_2)])

(define-extended-language E+Γ+C E+Γ
  (E ::=
     (plus E e)
     (plus V E)
     (cat E e)
     (cat V E)
     (let E x e)
     hole)
  (V ::=
     N S))

(module+ test
  (test-equal (judgment-holds (types · ⊢ (plus 1 2) : τ) τ) '(num))
  (test-equal (judgment-holds (types · ⊢ (let 1 x (let 2 y (plus x y))) : τ) τ) '(num)))

(define red
  (reduction-relation
   E+Γ+C
   (--> (plus N_1 N_2)
        ,(+ (term N_1) (term N_2)))
   (--> (cat S_1 S_2)
        ,(string-append (term S_1) (term S_2)))
   (--> (let V x e)
        (substitute e x V))))

(define -->r
  (context-closure red E+Γ+C E))

(module+ test
  (test-->> -->r (term (plus (plus 1 2) 3)) 6)
  (test-->> -->r (term (plus 1 (plus 2 3))) 6)
  (test-->> -->r (term (cat "abc" "def")) "abcdef")
  (test-->> -->r (term (let 1 x (plus 1 x))) 2)
  (test-->> -->r (term (let (plus 1 2) x (plus 3 x))) 6))

(define (uniquely-reduces? c)
  (= 1 (length (apply-reduction-relation -->r c))))

(define (unique-progress? c)
  (or (redex-match? E+Γ+C e c)
      (uniquely-reduces? c)))

(redex-check E+Γ+C e (unique-progress? (term e)) #:attempts 10000)

(module+ test
  (test-results))