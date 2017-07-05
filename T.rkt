#lang racket
(require redex)

(define-language T
  (t ::= nat (arr t t))
  (x ::= variable-not-otherwise-mentioned)
  (e ::= x
         z
         (s e)
         (rec e_0 (x_1 x_2 e_1) e)
         (lam t x e)
         (ap e_1 e_2))
  (Γ ::= • (x : t Γ))
  (E ::= hole
         (s E)
         (ap E e)
         (ap v E)
         (rec e0 (x_1 x_2 e_1) E))
  (v ::= z
         (s v)
         (lam t x e))
  #:binding-forms

  (lam t x e #:refers-to x))

(define-metafunction T
  [(different x_1 x_1) #f]
  [(different x_1 x_2) #t])

(define-judgment-form T
  #:mode (types I I O)
  #:contract (types Γ e t)
  [---------------------
   (types (x : t Γ) x t)]
  [(types Γ x_1 t_1)
   (side-condition (different x_1 x_2))
   ------------------------------------
   (types (x_2 : t_2 Γ) x_1 t_1)]
  [---------------
   (types Γ z nat)]
  [(types Γ e nat)
   -------------------
   (types Γ (s e) nat)]
  [(types Γ e nat)
   (types Γ e_0 t)
   (types (x_1 : nat x_2 : t Γ) e_1 t)
   -----------------------------------
   (types Γ (rec e_0 (x_1 x_2 e_1) e) t)]
  [(types (x : t_1 Γ) e t_2)
   --------------------------------------
   (types Γ (lam t_1 x e) (arr t_1 t_2))]
  [(types Γ e_1 (arr t_2 t))
   (types Γ e_2 t_2)
   -------------------------
   (types Γ (ap e_1 e_2) t)])

(define red
  (reduction-relation T
   (--> (in-hole E (ap (lam t x e) v_2))
        (in-hole E (substitute e x v_2)))
   (--> (in-hole E (rec e_0 (x_1 x_2 e_1) z))
        (in-hole E e_0))
   (--> (in-hole E (rec e_0 (x_1 x_2 e_1) (s v)))
        (in-hole E (substitute (substitute e_1 x_1 v) x_2 (rec e_0 (x_1 x_2 e_1) v))))))

(module+ test
  (test-->> red (term (ap (lam nat x (rec z (u v (s (s v))) x)) (s z))) (term (s (s z))))
  (test-->> red (term (ap (lam nat x (rec z (u v (s (s v))) x)) (s (s z)))) (term (s (s (s (s z)))))))

(module+ test
  (test-results))