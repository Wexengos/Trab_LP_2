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
  ; (printf "exp => ~a\n" exp)   para não exibir o log
  (match exp
    [(ast:int v) v]
    [(ast:bool v) v]
    [(ast:dif e1 e2)
     (let ([val1 (value-of e1 Δ)]
           [val2 (value-of e2 Δ)])
       ; (printf "Subtraindo: ~a - ~a\n" val1 val2)  
       (- val1 val2))]
    [(ast:zero? e) (zero? (value-of e Δ))]
    [(ast:not e) (not (value-of e Δ))]
    [(ast:if e1 e2 e3) (if (value-of e1 Δ) (value-of e2 Δ) (value-of e3 Δ))]
    [(ast:var v)
     (let ([val (deref (apply-env Δ v))])
       ; (printf "Variável encontrada: ~a, valor: ~a\n" v val)  
       val)]
    [(ast:assign (ast:var x) e)
     (let ([value (value-of e Δ)])
       ; (printf "Atribuindo ~a := ~a\n" x value)  
       (setref! (apply-env Δ x) value)  ; Atualiza o valor da variável no ambiente
       value)]  ; Retorna o valor atribuído
    [(ast:let (ast:var x) e1 e2)
     (value-of e2 (extend-env x (newref (value-of e1 Δ)) Δ))]
    [(ast:send e (ast:var method-dcl) args)
     (let* ([args_value (apply-value-of args Δ)]
            [obj (value-of e Δ)])
       (apply_method (search_method (object-class_name obj) method-dcl) obj args_value))]
   [(ast:super (ast:var method) args)
 (let* ([args_value (apply-value-of args Δ)]    ; Avalia os argumentos
        [obj (apply-env Δ "self")]              ; Obtém o objeto atual (self)
        [super_name (apply-env Δ "super")]      ; Obtém o nome da superclasse
        [method (search_method super_name (ast:var-name method))]) ; Procura o método na superclasse
   (apply_method method obj args_value))]       ; Chama o método da superclasse com o objeto atual

    [(ast:self) (apply-env Δ "self")]
    [(ast:new (ast:var c) args)
     (let* ([args_value (apply-value-of args Δ)]
            [obj (new_object c args_value)])
       (apply_method (search_method c "initialize") obj args_value)
       obj)]
    [e (raise-user-error "unimplemented-construction: " e)]))

; result-of :: Stmt -> Env -> State -> State
(define (result-of stmt Δ)
  ; (printf "Processando statement: ~a\n" stmt)  
  (match stmt
    [(ast:assign (ast:var x) e)
     (begin
       ; (printf "Atribuição: ~a := ~a\n" x (value-of e Δ
              ; (printf "Atribuição: ~a := ~a\n" x (value-of e Δ))  
       (setref! (apply-env Δ x) (value-of e Δ))
       42)]
    [(ast:print e) 
     (display (value-of e Δ)) 
     (newline)]
    [(ast:return e)
     (begin
       ; (printf "Retornando: ~a\n" e)  
       (value-of e Δ))]
    [(ast:block stmts)
     (for ([l stmts])
       (begin
         ; (printf "Processando statement do bloco: ~a\n" l)  
         (result-of l Δ)
         ; (printf "Processado statement do bloco: ~a\n" l)  
         #f))]  ; Garante que o bloco retorne #f após completar
    [(ast:if-stmt e s1 s2) 
     (display "if statement unimplemented")]
    [(ast:while e s) 
     (display "while statement unimplemented")]
    [(ast:local-decl (ast:var x) s)
     (result-of s (extend-env x (newref 'empty) Δ))]
    [(ast:send e (ast:var method-dcl) args)
     (let* ([args_value (apply-value-of args Δ)]
            [obj (value-of e Δ)])
       (apply_method (search_method (object-class_name obj) method-dcl) obj args_value))]
    [(ast:super (ast:var method) args)
 (let* ([args_value (apply-value-of args Δ)]    ; Avalia os argumentos
        [obj (apply-env Δ "self")]              ; Obtém o objeto atual (self)
        [super_name (apply-env Δ "super")]      ; Obtém o nome da superclasse
        [method (search_method super_name method)]) ; Procura o método na superclasse (sem ast:var-name)
   (apply_method method obj args_value))]       ; Chama o método da superclasse com o objeto atual


    [e (raise-user-error "unimplemented-construction: " e)]))

(define (apply-value-of exps Δ)
  (map (lambda (exp) (value-of exp Δ)) exps))


(define (search_method class_name method_name)
    (let ([methods (class-methods (search_class class_name))])
      (let ([found-method (assoc method_name methods)])
        (if found-method
            (cadr found-method)
            (raise-user-error "error => método não implementado" method_name)))))

(define (apply_method method self args)
  ; (printf "Aplicando método: ~a com argumentos: ~a\n" (method-body method) args)  
  (let* ([args-refs (map newref args)]
         [class_env (extend-env "self" self (extend-env "super" (method-super_name method) empty-env))]
         [method_env (bind-vars (method-fields method) (object-fields self) class_env)]
         [method_env_vars (bind-vars (method-arguments method) args-refs method_env)])
    (result-of (method-body method) method_env_vars)))

(define (new_object class_name value_args)
  (let* ([name_fields (class-fields (search_class class_name))]
         [fields (map (lambda (f_name) (newref null)) name_fields)])
    ; (printf "Criando objeto da classe: ~a com campos: ~a\n" class_name fields)  
    (object class_name fields)))

(define (search_class_in_program class_name)
  (if (null? program_classes)
      #f
      (for/or ([class_ program_classes])
        (if (equal? class_name (car class_))
            (car (cdr class_))
            #f))))

(define (search_class class_name)
  (let ([class_ret (search_class_in_program class_name)])
    (if class_ret
        class_ret
        (raise-user-error "error => Classe não definida no escopo: " class_name))))

(define (bind-vars vars values env)
  (for ([var vars] [val values])
      ; (printf "bind vars: ~a\n" var)  
    (set! env (extend-env var val env)))
  env)

(define (add_class class-name classe)
  (if (check_class? class-name classe)
      (raise-user-error "error => Classe já definida no escopo: " class-name)
      (set! program_classes (append program_classes (list (list class-name classe))))))

(define (check_class? class-name classe)
  (let ([result_class (search_class_in_program class-name)])
    (and result_class
         (equal? (class-methods result_class) (class-methods classe))
         (equal? (class-fields result_class) (class-fields classe)))))

(define (get_class class_name)
  (if (null? program_classes)
      #f
      (for/or ([class_ program_classes])
        (if (equal? class_name (car class_))
            (cdr class_)
            #f))))

(define (define_methods methods class_super_name fields)
  (append 
   (map (lambda (method) (define_method method class_super_name fields)) methods)
   (class-methods (search_class class_super_name))))

(define (define_method method-decl super_name fields)
  (list (ast:var-name (ast:method-name method-decl))
        (method (map ast:var-name (ast:method-params method-decl))
                (ast:method-body method-decl)
                super_name
                fields)))

(define (append_fields current-fields super-fields)
  (let ([current-set (set current-fields)])
    (foldr (lambda (field acc)
             (if (set-member? current-set field)
                 acc
                 (cons field acc)))
           current-fields
           super-fields)))

(define (value-of-program prog)
  (empty-store)
  (match prog
    [(ast:prog decls stmt)
     (begin
       (add_class "object" (class #f '() '()))
       (for ([decl decls])
         (let* ([name (ast:var-name (ast:decl-name decl))]
                [name_super (or (ast:var-name (ast:decl-super decl)) "object")]
                ; (printf "Superclasse: ~a\n" name_super)  
                [fields (append_fields (class-fields (search_class name_super))
                                       (map ast:var-name (ast:decl-fields decl)))]
                [methods (define_methods (ast:decl-methods decl) name_super fields)]
                [cls (class name_super fields methods)])
           (add_class name cls)))
       ; (printf "Processamento completo do programa!\n")  
       (result-of stmt init-env))]))

;COLOCAR A EXPLICAÇÃO DO IMUTABILIDADE NO RELATA
;Explicação:

   ; z = 5, x = 4 (no escopo mais interno), e y = 2 foram usados para avaliar a expressão final.
    ;-(z, -(x, y)) é avaliado em duas partes: primeiro -(x, y) = 2, depois -(z, 2) = 3.

;Portanto, o resultado da expressão que será retornado pelo print é 3.