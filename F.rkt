#lang racket

(require redex)

(define-language F
  (τ ::=
     t
     (τ_1 → τ_2)
     (∀ (t) τ))
  (e ::=
     x
     (lam (x τ) e)
     (ap e_1 e_2)
     (Lam (t) e)
     (Ap τ e))
  (Δ ::= • (t Δ))
  (Γ ::= • ((x : τ) Γ))
  (x t ::= variable-not-otherwise-mentioned)
  (type-assm ::=
             (hole Γ)
             ((x : τ) type-assm))
  #:binding-forms
  (∀ (t) τ #:refers-to t)
  (lam (x τ) e #:refers-to x)
  (Lam (t) e #:refers-to e))

(define-relation F
  lookup-type ⊂ Δ × t
  [(lookup-type (t Δ) t)]
  [(lookup-type (t_2 Δ) t)
   (lookup-type Δ t)])

(define-relation F
  type ⊂ Δ × τ
  [(type Δ t)
   (lookup-type Δ t)]
  [(type Δ (τ_1 → τ_2))
   (type Δ t_1)
   (type Δ t_2)]
  [(type Δ (∀ (t) τ))
   (type (t Δ) τ)])

(define-metafunction F
  lookup : Γ x -> τ or #f
  [(lookup ((x : τ) Γ) x)
   τ]
  [(lookup ((x_1 : τ) Γ) x)
   (lookup Γ x)]
  [(lookup • x)
   #f])

(module+ test
  (test-equal (term (lookup • x)) #f))

(define-judgment-form F
  #:mode (has-type I I I O)
  #:contract (has-type Δ Γ e τ)
  [(where τ (lookup Γ x))
   ----------------------
   (has-type Δ Γ x τ)]
  
  [(type Δ τ_1)
   (has-type Δ ((x : τ_1) Γ) e τ_2)
   ------------------------------------------
   (has-type Δ Γ (lam (x τ_1) e) (τ_1 → τ_2))]
  
  [(has-type Δ Γ e_1 (τ_1 → τ))
   (has-type Δ Γ e_1 τ_2)
   ----------------------------
   (has-type Δ Γ (ap e_1 e_2) τ)]
  
  [(has-type (t Δ) Γ e τ)
   ------------------------------------
   (has-type Δ Γ (Lam (t) e) (∀ (t) τ))]
  
  [(has-type Δ Γ e (∀ (t) τ_1))
   (type Δ τ)
   ---------------------------------------------
   (has-type Δ Γ (App τ e) (substitute τ_1 t τ))])

(module+ test
  (test-judgment-holds (lookup-type (t •) t))
  (test-equal (judgment-holds (lookup-type (t_2 •) t)) #f)
  (test-judgment-holds (lookup-type (t_2 (t •)) t)))

(module+ test
  (redex-check
   F (Δ Γ e τ)
   (let ([taus (map (λ (m) (bind-exp (findf (λ (bind) (equal? (bind-name bind) 'τ)) (match-bindings m))))
                    (or (redex-match F (in-hole type-assm (_ : τ)) (term Γ)) '()))])
   (implies (and (judgment-holds (has-type Δ Γ e τ))
                 (andmap (λ (t) (judgment-holds (type Δ t))) taus))
            (judgment-holds (type Δ τ))))))

(module+ test
  (test-results))