#lang racket

(require dcc019/util/env
         dcc019/util/memory
         dcc019/exercise/iclasses/ast)

(provide value-of-program)

(struct object (class_name fields))
(struct class (class_super_name fields methods))
(struct method (arguments body super_name fields))

(define program_classes '())

; value-of :: Exp -> ExpVal
(define (value-of exp Δ)
  (printf "exp => ~a\n" exp)
  (match exp
    [(ast:int v) v]
    [(ast:bool v) v]
    [(ast:dif e1 e2)
     (let ([val1 (value-of e1 Δ)]
           [val2 (value-of e2 Δ)])
       (printf "Subtraindo: ~a - ~a\n" val1 val2)
       (- val1 val2))]
    [(ast:zero? e) (zero? (value-of e Δ))]
    [(ast:not e) (not (value-of e Δ))]
    [(ast:if e1 e2 e3) (if (value-of e1 Δ) (value-of e2 Δ) (value-of e3 Δ))]
    [(ast:var v)
     (let ([val (deref (apply-env Δ v))])
       (printf "Variável encontrada: ~a, valor: ~a\n" v val)
       val)]
    [(ast:assign (ast:var x) e)
     (let ([value (value-of e Δ)])
       (printf "Atribuindo ~a := ~a\n" x value)
       (setref! (apply-env Δ x) value)  ; Atualiza o valor da variável no ambiente
       value)]  ; Retorna o valor atribuído
    [(ast:let (ast:var x) e1 e2) 
     (value-of e2 (extend-env x (newref (value-of e1 Δ)) Δ))]
    [(ast:send e (ast:var method-dcl) args)
     (let* ([args_value (apply-value-of args Δ)]
            [obj (value-of e Δ)])
       (apply_method (search_method (object-class_name obj) method-dcl) obj args_value))]
    [(ast:super (ast:var c) args)
     (let* ([args_value (apply-value-of args Δ)]
            [obj (apply-env Δ "self")]
            [super_name (apply-env Δ "super")]
            [var (ast:var-name args)])
       (apply_method ((search_method super_name var) obj args_value)))]
    [(ast:self) (apply-env Δ "self")]
    [(ast:new (ast:var c) args)
     (let* ([args_value (apply-value-of args Δ)]
            [obj (new_object c args_value)])
       (apply_method (search_method c "initialize") obj args_value)
       obj)]
    [e (raise-user-error "unimplemented-construction: " e)]))

; result-of :: Stmt -> Env -> State -> State
(define (result-of stmt Δ)
  (printf "Processando statement: ~a\n" stmt)
  (match stmt
    [(ast:assign (ast:var x) e)
     (begin
       (printf "Atribuição: ~a := ~a\n" x (value-of e Δ))
       (setref! (apply-env Δ x) (value-of e Δ))
       42)]
    [(ast:print e)
     (begin
       (printf "Imprimindo expressão: ~a\n" e)
       (display (value-of e Δ))
       (newline)
       (value-of e Δ))]
    [(ast:return e)
     (begin
       (printf "Retornando: ~a\n" e)
       (value-of e Δ))]
    [(ast:block
