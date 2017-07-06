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
  #:mode (⊢ I I I O)
  #:contract (⊢ Γ e : τ)
  [-----------------
   (⊢ (x : τ Γ) x : τ)]
  [(⊢ Γ x_1 : τ_1)
   (side-condition (different x_1 x_2))
   ------------------------------------
   (⊢ (x_2 : τ_2 Γ) x_1 : τ_1)]
  [--------------------
   (⊢ Γ string : str)]
  [--------------------
   (⊢ Γ number : num)]
  [(⊢ Γ e_1 : num)
   (⊢ Γ e_2 : num)
   --------------------------
   (⊢ Γ (plus e_1 e_2) : num)]
  [(⊢ Γ e_1 : num)
   (⊢ Γ e_2 : num)
   ---------------------------
   (⊢ Γ (times e_1 e_2) : num)]
  [(⊢ Γ e_1 : str)
   (⊢ Γ e_2 : str)
   -------------------------
   (⊢ Γ (cat e_1 e_2) : str)]
  [(⊢ Γ e : str)
   ---------------------
   (⊢ Γ (len e) : num)]
  [(⊢ Γ e_1 : τ_1)
   (⊢ (x : τ_1 Γ) e_2 : τ_2)
   --------------------------
   (⊢ Γ (let e_1 x e_2) : τ_2)])

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
  (test-equal (judgment-holds (⊢ · (plus 1 2) : τ) τ) '(num))
  (test-equal (judgment-holds (⊢ · (let 1 x (let 2 y (plus x y))) : τ) τ) '(num)))

(define red
  (reduction-relation
   E+Γ+C
   (--> (plus N_1 N_2)
        ,(+ (term N_1) (term N_2)))
   (--> (cat S_1 S_2)
        ,(string-append (term S_1) (term S_2)))
   (--> (let V x e)
        (substitute e x V))))

(module+ test
  (test-->> (context-closure red E+Γ+C E) (term (plus (plus 1 2) 3)) 6)
  (test-->> (context-closure red E+Γ+C E) (term (plus 1 (plus 2 3))) 6)
  (test-->> (context-closure red E+Γ+C E) (term (cat "abc" "def")) "abcdef")
  (test-->> (context-closure red E+Γ+C E) (term (let 1 x (plus 1 x))) 2)
  (test-->> (context-closure red E+Γ+C E) (term (let (plus 1 2) x (plus 3 x))) 6))

(module+ test
  (test-results))