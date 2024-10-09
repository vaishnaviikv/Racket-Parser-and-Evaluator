#lang plai-typed

;; TODO: Implement the following two functions.
;;
;; parse should take an s-expression representing a program and return an
;; AST corresponding to the program.
;;
;; eval should take an expression e (i.e. an AST) and evaluate e,
;; returning the resulting value.
;;
;; See ps2-ast.rkt and README.md for more information.
;;
;; Shraddha Jain

(require "ps2-ast.rkt")

; Parser

; Helper functions for list parsing
(define (parse-list l)
  (cond
    [(s-exp-symbol? (first l))
     (parse-symbol-list l)]))

(define (parse-symbol-list l)
  (let [(op (s-exp->symbol (first l)))]
    (case op
      [(+) (plusC (parse (second l)) (parse (third l)))]
      [(*) (timesC (parse (second l)) (parse (third l)))]
      [(natrec) (parse-natrec l)]
      [(equal?) (parse-binary-op equal?C l)]
      [(if) (parse-if l)]
      [(list) (parse-list-constructor l)]
      [(cons) (parse-binary-op consC l)]
      [(first) (parse-unary-op firstC l)]
      [(rest) (parse-unary-op restC l)]
      [(listrec) (parse-listrec l)]
      [(let) (parse-let l)]
      [(let*) (parse-let* l)]
      [(unpack) (parse-unpack l)]
      [(id) (parse-id l)])))

; Common parsing patterns
(define (parse-binary-op constructor l)
  (constructor (parse (second l)) (parse (third l))))

(define (parse-unary-op constructor l)
  (constructor (parse (second l))))

(define (parse-natrec l)
  (let* [(n (parse (second l)))        
         (base-case (parse (third l))) 
         (vars-list (s-exp->list (fourth l))) 
         (x (s-exp->symbol (first vars-list))) 
         (y (s-exp->symbol (second vars-list))) 
         (rec-body (parse (third vars-list)))]  
    (natrecC n base-case x y rec-body))) 


(define (parse-if l)
  (let [(condition (parse (second l)))    
        (true-branch (parse (third l)))   
        (false-branch (parse (fourth l)))] 
    (ifC condition true-branch false-branch)))

(define (parse-list-constructor l)
  (listC (map parse (rest l)))) 

(define (parse-listrec l)
  (listrecC
   (parse (second l))                   
   (parse (third l))                    
   (s-exp->symbol (first (s-exp->list (fourth l))))  
   (s-exp->symbol (second (s-exp->list (fourth l)))) 
   (s-exp->symbol (third (s-exp->list (fourth l)))) 
   (parse (fourth (s-exp->list (fourth l))))))

(define (parse-bindings bindings)
  (map (lambda (b)
         (let [(binding (s-exp->list b))]
           (pair (s-exp->symbol (first binding))
                 (parse (second binding)))))
       bindings))

(define (parse-let l)
  (let* [(bindings (s-exp->list (second l)))
         (parsed-bindings (parse-bindings bindings))
         (body (parse (third l)))]
    (letC parsed-bindings body)))

(define (parse-let* l)
  (let* [(bindings (s-exp->list (second l)))
         (parsed-bindings (parse-bindings bindings))
         (body (parse (third l)))]
    (let*C parsed-bindings body)))

(define (parse-unpack l)
  (let* [(vars (map s-exp->symbol (s-exp->list (second l))))
         (e1 (parse (third l)))
         (e2 (parse (fourth l)))]
    (unpackC vars e1 e2)))

(define (parse-id l)
  (idC (s-exp->symbol (second l))))

; Main Parser
(define (parse (s : s-expression)) : Expr
  (cond
    [(s-exp-number? s) (valC (numV (s-exp->number s)))]
    [(s-exp-boolean? s) (valC (boolV (s-exp->boolean s)))]
    [(s-exp-symbol? s) (idC (s-exp->symbol s))]
    [(s-exp-list? s) (parse-list (s-exp->list s))]))

; Evaluator

; Types and Environment Setup
(define-type Binding 
  [bind (name : symbol) (val : Value)])

(define-type-alias Env (listof Binding))
(define empty-env empty)
(define extend-env cons)

; Environment Operations
(define (lookup x env)
  (cond
    [(cons? env)
     (let ([binding (first env)])
       (if (equal? (bind-name binding) x)
           (bind-val binding)
           (lookup x (rest env))))]
    [else (error 'lookup "No binding found")]))


(define (extend-env-unpack (env : Env) (vars : (listof symbol)) (vals : (listof Value))) : Env
  (if (empty? vars)
      env
      (extend-env (bind (first vars) (first vals))
                  (extend-env-unpack env (rest vars) (rest vals)))))

; Equality Helpers
(define (equal-value? (v1 : Value) (v2 : Value)) : boolean
  (type-case Value v1
    [numV (n1) 
     (type-case Value v2
       [numV (n2) (= n1 n2)]
       [else false])]
    [boolV (b1) 
     (type-case Value v2
       [boolV (b2) (eq? b1 b2)]
       [else false])]
    [listV (l1)
     (type-case Value v2
       [listV (l2) 
        (and (= (length l1) (length l2))
             (equal-list? l1 l2))]
       [else false])]))

(define (equal-list? (l1 : (listof Value)) (l2 : (listof Value))) : boolean
  (cond
    [(and (empty? l1) (empty? l2)) true]
    [(or (empty? l1) (empty? l2)) false]
    [else (and (equal-value? (first l1) (first l2))
               (equal-list? (rest l1) (rest l2)))]))

; List Operations
(define (safe-rest lst)
  (if (empty? lst)
      empty
      (rest lst)))

; Evaluation Helpers
(define (eval-numeric-op op env e1 e2 op-name)
  (let* ([v1 (eval-env env e1)]
         [v2 (eval-env env e2)])
    (cond
      [(and (numV? v1) (numV? v2))
       (numV (op (numV-n v1) (numV-n v2)))]
      [else (error 'eval-env "Operation requires numeric values")])))

(define (eval-list-op env e pred? error-msg proc)
  (let ([v (eval-env env e)])
    (type-case Value v
      [listV (vs)
       (cond
         [(pred? vs) (proc vs)]
         [else (error 'eval-env error-msg)])]
      [else (error 'eval-env "Operation can only be applied to a list")])))

; Main Evaluator
(define (eval-env (env : Env) (e : Expr)) : Value
  (type-case Expr e
    [valC (v) v] 
    
    [plusC (e1 e2) 
     (eval-numeric-op + env e1 e2 "Addition")]

    [timesC (e1 e2) 
     (eval-numeric-op * env e1 e2 "Multiplication")]

    [letC (bindings body)
          (let* ([evaluate-binding 
                  (lambda (b) 
                    (bind (fst b) (eval-env env (snd b))))]
                 [new-bindings (map evaluate-binding bindings)]
                 [new-env (foldr extend-env env new-bindings)])
            (eval-env new-env body))]

    [let*C (bindings body)
           (if (empty? bindings)
               (eval-env env body) 
               (let* ([first-binding (first bindings)]
                      [var (fst first-binding)]  
                      [expr (snd first-binding)]  
                      [val (eval-env env expr)] 
                      [new-env (extend-env (bind var val) env)])  
                 (eval-env new-env (let*C (rest bindings) body))))]
    
    [natrecC (e1 e2 x y e3)
     (let* ([n (numV-n (eval-env env e1))])
       (if (< n 0)
           (error 'eval-env "natrec expects a non-negative integer")
           (if (= n 0)
               (eval-env env e2)  
               (let* ([v (eval-env env (natrecC (valC (numV (- n 1))) e2 x y e3))] 
                      [new-env (extend-env (bind x (numV (- n 1)))  
                                          (extend-env (bind y v) env))])  
                 (eval-env new-env e3)))))]

    [equal?C (e1 e2)
     (let ([v1 (eval-env env e1)]
           [v2 (eval-env env e2)])
       (boolV (equal-value? v1 v2)))]

    [ifC (guard e1 e2)
     (let ([guard-value (eval-env env guard)])
       (type-case Value guard-value
         [boolV (b) (if b 
                        (eval-env env e1)
                        (eval-env env e2))]
         [else (error 'eval-env "If guard must evaluate to a boolean")]))]

    [listC (es)
     (listV (map (lambda (e) (eval-env env e)) es))]

    [consC (e1 e2)
     (let ([v1 (eval-env env e1)]
           [v2 (eval-env env e2)])
       (type-case Value v2
         [listV (vs) (listV (cons v1 vs))]
         [else (error 'eval-env "Second argument to cons must be a list")]))]

    [firstC (e)
     (eval-list-op env e 
                   (lambda (vs) (not (empty? vs)))
                   "Cannot take first of an empty list"
                   first)]

    [restC (e)
     (eval-list-op env e
                   (lambda (vs) (not (empty? vs)))
                   "Cannot take rest of an empty list"
                   (lambda (vs) (listV (rest vs))))]

    [unpackC (vars e1 e2)
     (let ([v1 (eval-env env e1)])
       (type-case Value v1
         [listV (vs)
          (if (= (length vars) (length vs))
              (eval-env (extend-env-unpack env vars vs) e2)
              (error 'eval-env "Mismatch in number of variables and list elements"))]
         [else (error 'eval-env "unpack requires a list value")]))]

    [listrecC (e1 e2 hd rest res e3)
              (let ([v1 (eval-env env e1)])
                (type-case Value v1
                  [listV (vs)
                         (letrec ([process-list
                                   (lambda (lst)
                                     (if (empty? lst)
                                         (eval-env env e2)
                                         (let* ([v-hd (first lst)]
                                                [v-rest (listV (safe-rest lst))]
                                                [v-rec (process-list (safe-rest lst))]
                                                [new-env (extend-env (bind hd v-hd)
                                                                     (extend-env (bind rest v-rest)
                                                                                 (extend-env (bind res v-rec) env)))])
                                           (eval-env new-env e3))))])
                           (process-list vs))]
                  [else 
                   (error 'eval-env "listrec requires a list value")]))]

    [idC (x) (lookup x env)]))


; Top-level Evaluator
(define (eval (e : Expr))
  (eval-env empty-env e))