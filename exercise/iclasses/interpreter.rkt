#lang racket

(require dcc019/util/env
         dcc019/util/memory
         dcc019/exercise/iclasses/ast)

(provide value-of-program)

(struct object (class_name fields))
(struct class (super_name name_fields methods))
(struct method (arguments body super_name fields))

(define program_class '())

; value-of :: Exp -> ExpVal
(define (value-of exp Δ)
  (match exp
    [(ast:int v) v]
    [(ast:bool v) v]
    [(ast:dif e1 e2) (- (value-of e1 Δ) (value-of e2 Δ))]
    [(ast:zero? e) (zero? (value-of e Δ))]
    [(ast:not e) (not (value-of e Δ))]
    [(ast:if e1 e2 e3) (if (value-of e1 Δ) (value-of e2 Δ) (value-of e3 Δ))]
    [(ast:var v) (apply-env Δ v)] ; esta implementação só funciona para variáveis imutáveis
    [(ast:let (ast:var x) e1 e2) (value-of e2 (extend-env x (value-of e1 Δ) Δ))]
    [(ast:send e (ast:var mth) args)
                (let* ([value_args (apply-value-of args Δ)]
                      [objeto (value-of e Δ)])
                      (apply_method (search_method (object-class_name objeto) mth) objeto value_args)
                )]
    [(ast:super (ast:var c) args) (display "super expression unimplemented")]
    [(ast:self) (display "self expression unimplemented")]
    [(ast:new (ast:var c) args) (display "new expression unimplemented")]
    [e (raise-user-error "unimplemented-construction: " e)]
    ))

; result-of :: Stmt -> Env -> State -> State
(define (result-of stmt Δ)
  (match stmt
    [(ast:assign (ast:var x) e) (display "assignment unimplemented")]
    [(ast:print e) (display (value-of e Δ))
                   #;(display "print unimplemented")]
    [(ast:return e) (value-of e Δ)]
    ; nó da AST do tipo blocos, que serão iterados para a interpretação;
    [(ast:block insts) (for ([l insts])
                             (result-of l Δ)
                            )]
    [(ast:if-stmt e s1 s2) (display "if statment unimplemented")]
    [(ast:while e s) (display "while unimplemented")]
    
    [(ast:local-decl (ast:var x) s) (result-of s (extend-env x (newref 'empty) Δ))]
    [(ast:send e (ast:var mth) args) (display "command send unimplemented")]
    [(ast:super (ast:var c) args) (display "command super unimplemented")]
    [e (raise-user-error "unimplemented-construction: " e)]
    ))

(define (apply_method method self args)
  (let* ([args-refs (map newref args)]
         [class_env (extend-env "self" self (extend-env "super" (method-super_name method) empty-env))]
         [method_env (bind-vars (method-fields method) (object-fields self) class_env)]
         [method_env_vars (bind-vars (method-arguments method) args-refs method_env)]
          )
    (result-of (method-body method) method_env_vars))
)

(define (apply-value-of exps Δ)
  (map (lambda (exp) (value-of exp Δ)) exps))

(define (search_method class_name method_name)
    (let ([methods (class-methods (search_class class_name))])
      (let ([found-method (assoc method_name methods)])
        (if found-method
            (cadr found-method)
            (raise-user-error "[ERROR] - Método não encontrado: " method_name)
        )
      )
    )
)

(define (search_class_in_program class_name)
  (if (null? program_class)
      #f
      (for/or ([class_ program_class])
        (if (equal? class_name (car class_))
            (cdr class_)
            #f))
  )
)

(define (search_class class_name)
  (let ([classe (search_class_in_program class_name)])
    (if classe
        classe
        (raise-user-error "[ERROR] - Classe não encontrada: " class_name)
    )
  )
)

(define (bind-vars vars values env)
  (for ([var vars] [val values])
    (set! env (extend-env var val env))
  )
  env
)

(define (value-of-program prog)
  (empty-store)
  (match prog
    [(ast:prog decls stmt)
     (begin
       ; you must collect all the classes declared and building its respectively environment
       ; execute the prog expression in the correct environment
       (result-of stmt init-env))]))

