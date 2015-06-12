;;
;; Cyclone Scheme
;; Copyright (c) 2014, Justin Ethier
;; All rights reserved.
;;
;; This module performs Scheme-to-Scheme transformations, and also contains
;; various utility functions used by the compiler.
;;

(load (string-append (cyc:get-lib-dir) "scheme/cyclone.scm"))

;; Built-in macros
;; TODO: just a stub, real code would read (define-syntax) 
;;       from a lib file or such
(define *defined-macros* 
  (list 
    (cons 'and 
     (lambda (expr rename compare)
       (cond ((null? (cdr expr)))
             ((null? (cddr expr)) (cadr expr))
             (else (list (rename 'if) (cadr expr)
                         (cons (rename 'and) (cddr expr))
                         #f)))))
    (cons 'or
     (lambda (expr rename compare)
       (cond ((null? (cdr expr)) #f)
             ((null? (cddr expr)) (cadr expr))
             (else
              (list (rename 'let) (list (list (rename 'tmp) (cadr expr)))
                    (list (rename 'if) (rename 'tmp)
                          (rename 'tmp)
                          (cons (rename 'or) (cddr expr))))))))
    (cons 'let (lambda (exp rename compare) (let=>lambda exp)))
    (cons 'let*
      (lambda (expr rename compare)
        (if (null? (cdr expr)) (error "empty let*" expr))
        (if (null? (cddr expr)) (error "no let* body" expr))
        (if (null? (cadr expr))
            `(,(rename 'let) () ,@(cddr expr))
            (if (if (list? (cadr expr))
                    (every
                     (lambda (x)
                       (if (pair? x) (if (pair? (cdr x)) (null? (cddr x)) #f) #f))
                     (cadr expr))
                    #f)
                `(,(rename 'let) (,(caar (cdr expr)))
                  (,(rename 'let*) ,(cdar (cdr expr)) ,@(cddr expr)))
                (error "bad let* syntax")))))
    (cons 'begin (lambda (exp rename compare) (begin=>let exp)))
    (cons 'letrec (lambda (exp rename compare) (letrec=>lets+sets exp)))
    (cons 'cond
          (lambda (expr rename compare)
            (if (null? (cdr expr))
                (if #f #f)
                ((lambda (cl)
                   (if (compare (rename 'else) (car cl))
                       (if (pair? (cddr expr))
                           (error "non-final else in cond" expr)
                           (cons (rename 'begin) (cdr cl)))
                       (if (if (null? (cdr cl)) #t (compare (rename '=>) (cadr cl)))
                           (list (list (rename 'lambda) (list (rename 'tmp))
                                       (list (rename 'if) (rename 'tmp)
                                             (if (null? (cdr cl))
                                                 (rename 'tmp)
                                                 (list (car (cddr cl)) (rename 'tmp)))
                                             (cons (rename 'cond) (cddr expr))))
                                 (car cl))
                           (list (rename 'if)
                                 (car cl)
                                 (cons (rename 'begin) (cdr cl))
                                 (cons (rename 'cond) (cddr expr))))))
                 (cadr expr)))))
    (cons 'cond-expand
      ;; Based on the cond-expand macro from Chibi scheme
      (lambda (expr rename compare)
        (define (check x)
          (if (pair? x)
              (case (car x)
                ((and) (every check (cdr x)))
                ((or) (any check (cdr x)))
                ((not) (not (check (cadr x))))
                ;((library) (eval `(find-module ',(cadr x)) (%meta-env)))
                (else (error "cond-expand: bad feature" x)))
              (memq x *features*)))
        (let expand ((ls (cdr expr)))
          (cond ((null? ls))  ; (error "cond-expand: no expansions" expr)
                ((not (pair? (car ls))) (error "cond-expand: bad clause" (car ls)))
                ((eq? 'else (caar ls)) ;(identifier->symbol (caar ls)))
                 (if (pair? (cdr ls))
                     (error "cond-expand: else in non-final position")
                     `(,(rename 'begin) ,@(cdar ls))))
                ((check (caar ls)) `(,(rename 'begin) ,@(cdar ls)))
                (else (expand (cdr ls)))))))
    (cons 'quasiquote
      ;; Based on the quasiquote macro from Chibi scheme
      (lambda (expr rename compare)
        (define (qq x d)
          (cond
           ((pair? x)
            (cond
             ((compare (rename 'unquote) (car x))
              (if (<= d 0)
                  (cadr x)
                  (list (rename 'list) (list (rename 'quote) 'unquote)
                        (qq (cadr x) (- d 1)))))
             ((compare (rename 'unquote-splicing) (car x))
              (if (<= d 0)
                  (list (rename 'cons) (qq (car x) d) (qq (cdr x) d))
                  (list (rename 'list) (list (rename 'quote) 'unquote-splicing)
                        (qq (cadr x) (- d 1)))))
             ((compare (rename 'quasiquote) (car x))
              (list (rename 'list) (list (rename 'quote) 'quasiquote)
                    (qq (cadr x) (+ d 1))))
             ((and (<= d 0) (pair? (car x))
                   (compare (rename 'unquote-splicing) (caar x)))
              (if (null? (cdr x))
                  (cadr (car x))
                  (list (rename 'append) (cadr (car x)) (qq (cdr x) d))))
             (else
              (list (rename 'cons) (qq (car x) d) (qq (cdr x) d)))))
           ((vector? x) (list (rename 'list->vector) (qq (vector->list x) d)))
           ((if (symbol? x) #t (null? x)) (list (rename 'quote) x))
           (else x)))
        (qq (cadr expr) 0)))
  ))


(define (built-in-syms)
  '(call-with-values call/cc define))

;; Tuning
(define *do-code-gen* #t) ; Generate C code?

;; Trace
(define *trace-level* 2)
(define (trace level msg pp prefix)
    (if (>= *trace-level* level)
      (begin
        (display "/* ")
        (newline)
        (display prefix)
        (pp msg)
        (display " */")
        (newline))))
(define (trace:error msg) (trace 1 msg pretty-print ""))
(define (trace:warn msg)  (trace 2 msg pretty-print ""))
(define (trace:info msg)  (trace 3 msg pretty-print ""))
(define (trace:debug msg) (trace 4 msg display "DEBUG: "))

(define (cyc:error msg)
  (error msg)
  (exit 1))

;; File Utilities

;; Get the basename of a file, without the extension.
;; EG: "file.scm" ==> "file"
(define (basename filename)
  (let ((pos (list-index #\. (reverse (string->list filename)))))
   (if (= pos -1)
       filename
       (substring filename 0 (- (string-length filename) pos 1)))))

;; Find the first occurence of e within the given list.
;; Returns -1 if e is not found.
(define list-index
  (lambda (e lst)
    (if (null? lst)
      -1
      (if (eq? (car lst) e)
        0
        (if (= (list-index e (cdr lst)) -1) 
          -1
          (+ 1 (list-index e (cdr lst))))))))


;; Utilities.

(cond-expand
  (cyclone
    ; member : symbol sorted-set[symbol] -> boolean
    (define (member sym S)
      (if (not (pair? S))
          #f
          (if (eq? sym (car S))
              #t
              (member sym (cdr S)))))

    ; void : -> void
    (define (void) (if #f #t)))
  (else #f))

; tagged-list? : symbol value -> boolean
(define (tagged-list? tag l)
  (and (pair? l)
       (eq? tag (car l))))

; char->natural : char -> natural
(define (char->natural c)
  (let ((i (char->integer c)))
    (if (< i 0)
        (* -2 i)
        (+ (* 2 i) 1))))

; integer->char-list : integer -> string
(define (integer->char-list n)
  (string->list (number->string n)))

; gensym-count : integer
(define gensym-count 0)

; gensym : symbol -> symbol
(define gensym (lambda params
                 (if (null? params)
                     (begin
                       (set! gensym-count (+ gensym-count 1))
                       (string->symbol (string-append
                                        "$"
                                        (number->string gensym-count))))
                     (begin
                       (set! gensym-count (+ gensym-count 1))
                       (string->symbol (string-append 
                                        (if (symbol? (car params))
                                            (symbol->string (car params))
                                            (car params))
                                        "$"
                                        (number->string gensym-count)))))))

; symbol<? : symbol symobl -> boolean
(define (symbol<? sym1 sym2)
  (string<? (symbol->string sym1)
            (symbol->string sym2)))

; insert : symbol sorted-set[symbol] -> sorted-set[symbol]
(define (insert sym S)
  (if (not (pair? S))
      (list sym)
      (cond
        ((eq? sym (car S))       S)
        ((symbol<? sym (car S))  (cons sym S))
        (else (cons (car S) (insert sym (cdr S)))))))

; remove : symbol sorted-set[symbol] -> sorted-set[symbol]
(define (remove sym S)
  (if (not (pair? S))
      '()
      (if (eq? (car S) sym)
          (cdr S)
          (cons (car S) (remove sym (cdr S))))))
          
; union : sorted-set[symbol] sorted-set[symbol] -> sorted-set[symbol]
(define (union set1 set2)
  ; NOTE: This should be implemented as merge for efficiency.
  (if (not (pair? set1))
      set2
      (insert (car set1) (union (cdr set1) set2))))

; difference : sorted-set[symbol] sorted-set[symbol] -> sorted-set[symbol]
(define (difference set1 set2)
  ; NOTE: This can be similarly optimized.
  (if (not (pair? set2))
      set1
      (difference (remove (car set2) set1) (cdr set2))))

; reduce : (A A -> A) list[A] A -> A
(define (reduce f lst init)
  (if (not (pair? lst))
      init
      (reduce f (cdr lst) (f (car lst) init))))

; azip : list[A] list[B] -> alist[A,B]
(define (azip list1 list2)
  (if (and (pair? list1) (pair? list2))
      (cons (list (car list1) (car list2))
            (azip (cdr list1) (cdr list2)))
      '()))

; assq-remove-key : alist[A,B] A -> alist[A,B]
(define (assq-remove-key env key)
  (if (not (pair? env))
      '()
      (if (eq? (car (car env)) key)
          (assq-remove-key (cdr env) key)
          (cons (car env) (assq-remove-key (cdr env) key)))))

; assq-remove-keys : alist[A,B] list[A] -> alist[A,B]
(define (assq-remove-keys env keys)
  (if (not (pair? keys))
      env
      (assq-remove-keys (assq-remove-key env (car keys)) (cdr keys))))

;; Simplified version of filter from SRFI 1
(define (filter pred lis)
  (let recur ((lis lis))
   (if (null? lis)
    lis
    (let ((head (car lis))
          (tail (cdr lis)))
      (if (pred head)
          (let ((new-tail (recur tail)))
        (if (eq? tail new-tail) lis
            (cons head new-tail)))
          (recur tail))))))


;; Data type predicates and accessors.

; const? : exp -> boolean
(define (const? exp)
  (or (integer? exp)
      (real? exp)
      (string? exp)
      (vector? exp)
      (char? exp)
      (boolean? exp)))

; ref? : exp -> boolean
(define (ref? exp)
  (symbol? exp))

; quote? : exp -> boolean
(define (quote? exp)
  (tagged-list? 'quote exp))

; let? : exp -> boolean
(define (let? exp)
  (tagged-list? 'let exp))

; let->bindings : let-exp -> alist[symbol,exp]
(define (let->bindings exp)
  (cadr exp))

; let->exp : let-exp -> exp
(define (let->exp exp)
  (cddr exp))

; let->bound-vars : let-exp -> list[symbol]
(define (let->bound-vars exp)
  (map car (cadr exp)))

; let->args : let-exp -> list[exp]
(define (let->args exp)
  (map cadr (cadr exp)))

; letrec? : exp -> boolean
(define (letrec? exp)
  (tagged-list? 'letrec exp))

; letrec->bindings : letrec-exp -> alist[symbol,exp]
(define (letrec->bindings exp)
  (cadr exp))

; letrec->exp : letrec-exp -> exp
(define (letrec->exp exp)
  (cddr exp))

; letrec->exp : letrec-exp -> list[symbol]
(define (letrec->bound-vars exp)
  (map car (cadr exp)))

; letrec->exp : letrec-exp -> list[exp]
(define (letrec->args exp)
  (map cadr (cadr exp)))

; lambda? : exp -> boolean
(define (lambda? exp)
  (tagged-list? 'lambda exp))

(define (lambda-varargs? exp)
  (and (lambda? exp)
       (or (symbol? (lambda->formals exp))
           (and (pair? (lambda->formals exp))
                (not (list? (lambda->formals exp)))))))

; lambda->formals : lambda-exp -> list[symbol]
(define (lambda->formals exp)
  (cadr exp))

(define (lambda-varargs? exp)
  (let ((type (lambda-formals-type exp)))
    (or (equal? type 'args:varargs)
        (equal? type 'args:fixed-with-varargs))))

(define (lambda-varargs-var exp)
  (if (lambda-varargs? exp)
    (if (equal? (lambda-formals-type exp) 'args:varargs)
        (lambda-formals exp) ; take symbol directly
        (car (reverse (lambda-formals->list exp)))) ; Last arg is varargs
    #f))

(define (lambda-formals-type exp)
 (let ((args (lambda->formals exp)))
   (cond
     ((symbol? args) 'args:varargs)
     ((list? args)   'args:fixed)
     ((pair? args)   'args:fixed-with-varargs)
     (else
       (error `(Unexpected formals list in lambda-formals-type: ,args))))))

(define (lambda-formals->list exp)
  (if (lambda-varargs? exp)
      (let ((args (lambda->formals exp)))
        (if (symbol? args)
            (list args)
            (pair->list args)))
      (lambda->formals exp)))

;; Repack a list of args (symbols) into lambda formals, by type
;; assumes args is a proper list
(define (list->lambda-formals args type)
  (cond 
    ((eq? type 'args:fixed) args)
    ((eq? type 'args:fixed-with-varargs) (list->pair args))
    ((eq? type 'args:varargs) 
     (if (> (length args) 1)
         (error `(Too many args for varargs ,args))
         (car args)))
    (else (error `(Unexpected type ,type)))))

;; Create a proper copy of an improper list
;; EG: (1 2 . 3) ==> (1 2 3)
(define (pair->list p)
  (let loop ((lst p))
    (if (not (pair? lst))
        (cons lst '())
        (cons (car lst) (loop (cdr lst))))))

;; Create an improper copy of a proper list
(define (list->pair l)
  (let loop ((lst l))
    (cond
    ((not (pair? lst)) 
     lst)
    ((null? (cdr lst))
     (car lst))
    (else
     (cons (car lst) (loop (cdr lst)))))))

; lambda->exp : lambda-exp -> exp
(define (lambda->exp exp)
  (cddr exp)) ;; JAE - changed from caddr, so we can handle multiple expressions

; if? : exp -> boolean
(define (if? exp)
  (tagged-list? 'if exp))

; if->condition : if-exp -> exp
(define (if->condition exp)
  (cadr exp))

; if->then : if-exp -> exp
(define (if->then exp)
  (caddr exp))

;; if-else? : if-exp -> bool
;; Determines whether an if expression has an else clause
(define (if-else? exp)
  (and (tagged-list? 'if exp)
       (> (length exp) 3)))

; if->else : if-exp -> exp
(define (if->else exp)
  (cadddr exp))

; app? : exp -> boolean
(define (app? exp)
  (pair? exp))

; app->fun : app-exp -> exp
(define (app->fun exp)
  (car exp))

; app->args : app-exp -> list[exp]
(define (app->args exp)
  (cdr exp))
  
; prim? : exp -> boolean
(define (prim? exp)
  (member exp *primitives*))

(define *primitives* '(
     Cyc-global-vars
     Cyc-get-cvar
     Cyc-set-cvar!
     Cyc-cvar? ;; Cyclone-specific
     Cyc-has-cycle?
     Cyc-stdout
     +
     -
     *
     /
     =
     >
     <
     >=
     <=
     apply
     %halt
     exit
     system
     Cyc-default-exception-handler
     Cyc-current-exception-handler
     cons
     cell-get
     set-global!
     set-cell!
     cell
     eq?
     eqv?
     equal?
     assoc
     assq
     assv
     memq
     memv
     member
     length
     set-car!
     set-cdr!
     car
     cdr
     caar cadr cdar cddr
     caaar caadr cadar caddr cdaar cdadr cddar cdddr
     caaaar caaadr caadar caaddr cadaar cadadr
     caddar cadddr cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
     char->integer
     integer->char
     string->number
     string-append
     string-cmp
     string->list
     list->string
     string->symbol
     symbol->string
     number->string
     make-vector
     list->vector
     vector-length
     vector-ref
     vector-set!
     boolean?
     char?
     eof-object?
     null?
     number?
     real?
     integer?
     pair?
     procedure?
     vector?
     string?
     symbol?
     current-input-port
     open-input-file
     close-input-port
     read-char
     peek-char
     Cyc-write
     display))

;; Constant Folding
;; Is a primitive being applied in such a way that it can be
;; evaluated at compile time?
(define (precompute-prim-app? ast)
  (and 
    (pair? ast)
    (prim? (car ast))
    ;; Does not make sense to precompute these
    (not (member (car ast)
                '(Cyc-global-vars
                  Cyc-get-cvar
                  Cyc-set-cvar!
                  Cyc-cvar?
                  Cyc-stdout
                  apply
                  %halt
                  exit
                  system
                  Cyc-default-exception-handler
                  Cyc-current-exception-handler
                  cell-get
                  set-global!
                  set-cell!
                  cell
                  set-car!
                  set-cdr!
                  string->symbol ;; Could be mistaken for an identifier
                  string->list ;; Mistaken for function call (maybe OK if it was quoted, though). same for above?
                  make-vector
                  ;; I/O must be done at runtime for side effects:
                  current-input-port
                  open-input-file
                  close-input-port
                  read-char
                  peek-char
                  write
                  display)))
    (call/cc
      (lambda (return)
        (for-each
          (lambda (expr)
            (if (or (vector? expr)
                    (not (const? expr)))
              (return #f)))
          (cdr ast))
        #t))))

(define (prim-call? exp)
  (and (list? exp) (prim? (car exp))))

; begin? : exp -> boolean
(define (begin? exp) 
  (tagged-list? 'begin exp))

; begin->exps : begin-exp -> list[exp]
(define (begin->exps exp)
  (cdr exp))

; define : exp -> boolean
(define (define? exp)
  (tagged-list? 'define exp))

(define (define-lambda? exp)
  (let ((var (cadr exp)))
    (or
      ;; Standard function
      (and (list? var) 
           (> (length var) 0)
           (symbol? (car var)))
      ;; Varargs function
      (and (pair? var)
           (symbol? (car var))))))

(define (define->lambda exp)
  (cond
    ((define-lambda? exp)
     (let ((var (caadr exp))
           (args (cdadr exp))
           (body (cddr exp)))
       `(define ,var (lambda ,args ,@body))))
    (else exp)))

; define->var : define-exp -> var
(define (define->var exp)
  (cond
    ((define-lambda? exp)
     (caadr exp))
    (else 
     (cadr exp))))

; define->exp : define-exp -> exp
(define (define->exp exp)
  (cddr exp))

; set! : exp -> boolean
(define (set!? exp)
  (tagged-list? 'set! exp))

; set!->var : set!-exp -> var
(define (set!->var exp)
  (cadr exp))

; set!->exp : set!-exp -> exp
(define (set!->exp exp)
  (caddr exp))

; closure? : exp -> boolean
(define (closure? exp) 
  (tagged-list? 'closure exp))

; closure->lam : closure-exp -> exp
(define (closure->lam exp) 
  (cadr exp))

; closure->env : closure-exp -> exp
(define (closure->env exp) 
  (caddr exp))

(define (closure->fv exp) 
  (cddr exp))

; env-make? : exp -> boolean
(define (env-make? exp) 
  (tagged-list? 'env-make exp))

; env-make->id : env-make-exp -> env-id
(define (env-make->id exp)
  (cadr exp))

; env-make->fields : env-make-exp -> list[symbol]
(define (env-make->fields exp)
  (map car (cddr exp)))
  
; env-make->values : env-make-exp -> list[exp]
(define (env-make->values exp)
  (map cadr (cddr exp)))

; env-get? : exp -> boolen
(define (env-get? exp)
  (tagged-list? 'env-get exp))

; env-get->id : env-get-exp -> env-id
(define (env-get->id exp)
  (cadr exp))
  
; env-get->field : env-get-exp -> symbol
(define (env-get->field exp)
  (caddr exp))

; env-get->env : env-get-exp -> exp
(define (env-get->env exp)
  (cadddr exp)) 

; set-cell!? : set-cell!-exp -> boolean
(define (set-cell!? exp)
  (tagged-list? 'set-cell! exp))

; set-cell!->cell : set-cell!-exp -> exp
(define (set-cell!->cell exp)
  (cadr exp))

; set-cell!->value : set-cell!-exp -> exp
(define (set-cell!->value exp)
  (caddr exp))

; cell? : exp -> boolean
(define (cell? exp)
  (tagged-list? 'cell exp))

; cell->value : cell-exp -> exp
(define (cell->value exp)
  (cadr exp))

; cell-get? : exp -> boolean
(define (cell-get? exp)
  (tagged-list? 'cell-get exp))

; cell-get->cell : cell-exp -> exp
(define (cell-get->cell exp)
  (cadr exp))



;; Syntax manipulation.

;; ; substitute-var : alist[var,exp] ref-exp -> exp
;; (define (substitute-var env var)
;;   (let ((sub (assq var env)))
;;     (if sub
;;         (cadr sub)
;;         var)))
;; 
;; ; substitute : alist[var,exp] exp -> exp
;; (define (substitute env exp)
;;   
;;   (define (substitute-with env)
;;     (lambda (exp)
;;       (substitute env exp)))
;; 
;;   (cond
;;     ; Core forms:    
;;     ((null? env)        exp)
;;     ((const? exp)       exp)
;;     ((prim? exp)        exp)
;;     ((ref? exp)         (substitute-var env exp))
;;     ((lambda? exp)      `(lambda ,(lambda->formals exp)
;;                            ,@(map (lambda (body-exp) 
;;                                     ;; TODO: could be more efficient
;;                                     (substitute 
;;                                         (assq-remove-keys env (lambda->formals exp)) 
;;                                         body-exp))
;;                                  (lambda->exp exp))))
;;     ((set!? exp)        `(set! ,(substitute-var env (set!->var exp))
;;                                ,(substitute env (set!->exp exp))))
;;     ((if? exp)          `(if ,(substitute env (if->condition exp))
;;                              ,(substitute env (if->then exp))
;;                              ,(substitute env (if->else exp))))
;;     
;;     ; Sugar:
;;     ((let? exp)         `(let ,(azip (let->bound-vars exp)
;;                                      (map (substitute-with env) (let->args exp)))
;;                            ,(substitute (assq-remove-keys env (let->bound-vars exp))
;;                                         (car (let->exp exp)))))
;;     ((letrec? exp)      (let ((new-env (assq-remove-keys env (letrec->bound-vars exp))))
;;                           `(letrec ,(azip (letrec->bound-vars exp) 
;;                                           (map (substitute-with new-env) 
;;                                                (letrec->args exp)))
;;                              ,(substitute new-env (car (letrec->exp exp))))))
;;     ((begin? exp)       (cons 'begin (map (substitute-with env) (begin->exps exp))))
;; 
;;     ; IR (1):
;;     ((cell? exp)        `(cell ,(substitute env (cell->value exp))))
;;     ((cell-get? exp)    `(cell-get ,(substitute env (cell-get->cell exp))))
;;     ((set-cell!? exp)   `(set-cell! ,(substitute env (set-cell!->cell exp))
;;                                     ,(substitute env (set-cell!->value exp))))
;;     
;;     ; IR (2):
;;     ((closure? exp)     `(closure ,(substitute env (closure->lam exp))
;;                                   ,(substitute env (closure->env exp))))
;;     ((env-make? exp)    `(env-make ,(env-make->id exp) 
;;                                    ,@(azip (env-make->fields exp)
;;                                            (map (substitute-with env)
;;                                                 (env-make->values exp)))))
;;     ((env-get? exp)     `(env-get ,(env-get->id exp)
;;                                   ,(env-get->field exp)
;;                                   ,(substitute env (env-get->env exp))))
;;     
;;     ; Application:
;;     ((app? exp)         (map (substitute-with env) exp))
;;     (else               (error "unhandled expression type in substitution: " exp))))
;; 

;; Macro expansion
(define (macro? exp) (assoc (car exp) *defined-macros*))
(define (macro-expand exp)
  (let ((macro (assoc (car exp) *defined-macros*)))
    ;; assumes ER macro
    (if macro
      ((cdr macro) 
        exp 
        (lambda (sym) ;; TODO: not good enough, need to actually rename, and keep same results if
          sym)        ;; the same symbol is renamed more than once
        (lambda (sym-a sym-b) ;; TODO: the compare function from exrename.
          (eq? sym-a sym-b))) ;; this may need to be more sophisticated
      exp))) ;; TODO: error instead??

; expand : exp -> exp
(define (expand exp)
  (cond
    ((const? exp)      exp)
    ((prim? exp)       exp)
    ((ref? exp)        exp)
    ((quote? exp)      exp)
    ((lambda? exp)     `(lambda ,(lambda->formals exp)
                          ,@(map expand (lambda->exp exp))))
    ((define? exp)     (if (define-lambda? exp)
                           (expand (define->lambda exp))
                          `(define ,(expand (define->var exp))
                                ,@(expand (define->exp exp)))))
    ((set!? exp)       `(set! ,(expand (set!->var exp))
                              ,(expand (set!->exp exp))))
    ((if? exp)         `(if ,(expand (if->condition exp))
                            ,(expand (if->then exp))
                            ,(if (if-else? exp)
                                 (expand (if->else exp))
                                 ;; Insert default value for missing else clause
                                 ;; FUTURE: append the empty (unprinted) value
                                 ;; instead of #f
                                 #f)))
    ((app? exp)
     (cond
;; TODO: could check for a define-syntax here and load into memory
;; if found. would then want to continue expanding. may need to 
;; return some value such as #t or nil as a placeholder, since the
;; define-syntax form would not be carried forward in the compiled code
;;   ((define-syntax? exp) ...)
     ((macro? exp)
       (expand ;; Could expand into another macro
         (macro-expand exp)))
     (else
       (map expand exp))))
    (else
      (error "unknown exp: " exp))))

; TODO: eventually, merge below functions with above *defined-macros* defs and 
;;      replace both with a lib of (define-syntax) constructs

; let=>lambda : let-exp -> app-exp
(define (let=>lambda exp)
  (if (let? exp)
      (let ((vars (map car (let->bindings exp)))
            (args (map cadr (let->bindings exp))))
        `((lambda (,@vars) ,@(let->exp exp)) ,@args))
      exp))

; letrec=>lets+sets : letrec-exp -> exp
(define (letrec=>lets+sets exp)
  (if (letrec? exp)
      (let* ((bindings  (letrec->bindings exp))
             (namings   (map (lambda (b) (list (car b) #f)) bindings))
             (names     (letrec->bound-vars exp))
             (sets      (map (lambda (binding) 
                               (cons 'set! binding))
                             bindings))
             (args      (letrec->args exp)))
        `(let ,namings
           (begin ,@(append sets (letrec->exp exp)))))))
;; NOTE: chibi uses the following macro. turns vars into defines?
;;(define-syntax letrec
;;  (er-macro-transformer
;;   (lambda (expr rename compare)
;;     ((lambda (defs)
;;        `((,(rename 'lambda) () ,@defs ,@(cddr expr))))
;;      (map (lambda (x) (cons (rename 'define) x)) (cadr expr))))))
;;

; begin=>let : begin-exp -> let-exp
(define (begin=>let exp)
  (define (singlet? l)
    (and (list? l)
         (= (length l) 1)))
  
  (define (dummy-bind exps)
    (cond
      ((singlet? exps)  (car exps))
      
      ; JAE - should be fine until CPS phase
      ((pair? exps)
       `((lambda ()
         ,@exps)))))
      ;((pair? exps)     `(let (($_ ,(car exps)))
      ;                    ,(dummy-bind (cdr exps))))))
  (dummy-bind (begin->exps exp)))


;; Top-level analysis

; Separate top-level defines (globals) from other expressions
;
; This function extracts out non-define statements, and adds them to 
; a "main" after the defines.
;
(define (isolate-globals exp program? lib-name)
  (let loop ((top-lvl exp)
             (globals '())
             (exprs '()))
    (cond 
      ((null? top-lvl)
       (append
         (reverse globals)
         (expand 
           (cond 
             (program?
               ;; This is the main program, keep top level.
               ;; Use 0 here (and below) to ensure a meaningful top-level
               `((begin 0 ,@(reverse exprs)))
             )
             (else
               ;; This is a library, keep inits in their own function
               `((define ,(lib:name->symbol lib-name)
                  (lambda () 0 ,@(reverse exprs))))))
           )))
      (else
       (cond
         ((define? (car top-lvl))
          (cond
            ;; Global is redefined, convert it to a (set!) at top-level
            ((has-global? globals (define->var (car top-lvl)))
             (loop (cdr top-lvl)
                   globals
                   (cons
                    `(set! ,(define->var (car top-lvl))
                           ,@(define->exp (car top-lvl)))
                     exprs)))
            ;; Form cannot be properly converted to CPS later on, so split it up
            ;; into two parts - use the define to initialize it to false (CPS is fine),
            ;; and place the expression into a top-level (set!), which can be
            ;; handled by the existing CPS conversion.
            ((or 
               ;; TODO: the following line may not be good enough, a global assigned to another
               ;; global may still be init'd to nil if the order is incorrect in the "top level"
               ;; initialization code.
               (symbol? (car (define->exp (car top-lvl)))) ;; TODO: put these at the end of top-lvl???
               (and (list? (car (define->exp (car top-lvl))))
                    (not (lambda? (car (define->exp (car top-lvl)))))))
             (loop (cdr top-lvl)
                   (cons
                    `(define ,(define->var (car top-lvl)) #f)
                    globals)
                   (cons
                    `(set! ,(define->var (car top-lvl))
                           ,@(define->exp (car top-lvl)))
                    exprs)))
            ;; First time we've seen this define, add it and keep going
            (else
             (loop (cdr top-lvl)
                   (cons (car top-lvl) globals)
                   exprs))))
         (else
           (loop (cdr top-lvl)
                 globals
                 (cons (car top-lvl) exprs))))))))

; Has global already been found?
;
; NOTE:
; Linear search may get expensive (n^2), but with a modest set of
; define statements hopefully it will be acceptable. If not, will need
; to use a faster data structure (EG: map or hashtable)
(define (has-global? exp var)
  (call/cc
    (lambda (return)
      (for-each 
        (lambda (e)
          (if (and (define? e)
                   (equal? (define->var e) var))
            (return #t)))
       exp)
      #f)))

; Compute list of global variables based on expression in top-level form
; EG: (def, def, expr, ...)
(define (global-vars exp)
  (let ((globals '()))
    (for-each
      (lambda (e)
        (if (define? e)
            (set! globals (cons (define->var e) globals))))
      exp)
    globals))

;; Remove global variables that are not used by the rest of the program.
;; Many improvements can be made, including:
;;
;; TODO: remove unused locals
(define (filter-unused-variables asts lib-exports)
  (define (do-filter code)
    (let ((all-fv (apply      ;; More efficient way to do this?
                    append    ;; Could use delete-duplicates
                    (map 
                      (lambda (ast)
                        (if (define? ast)
                            (let ((var (define->var ast)))
                              ;; Do not keep global that refers to itself
                              (filter
                                (lambda (v)
                                  (not (equal? v var)))
                                (free-vars (define->exp ast))))
                            (free-vars ast)))
                      code))))
      (filter
        (lambda (ast)
          (or (not (define? ast))
              (member (define->var ast) all-fv)
              (member (define->var ast) lib-exports)))
        code)))
  ;; Keep filtering until no more vars are removed
  (define (loop code)
    (let ((new-code (do-filter code)))
      (if (> (length code) (length new-code))
          (loop new-code)
          new-code)))
  (loop asts))

;; Syntactic analysis.

; free-vars : exp -> sorted-set[var]
(define (free-vars ast . opts)
  (define bound-only? 
    (and (not (null? opts))
         (car opts)))

  (define (search exp)
    (cond
      ; Core forms:
      ((const? exp)    '())
      ((prim? exp)     '())    
      ((quote? exp)    '())    
      ((ref? exp)      (if bound-only? '() (list exp)))
      ((lambda? exp)   
        (difference (reduce union (map search (lambda->exp exp)) '())
                    (lambda-formals->list exp)))
      ((if? exp)       (union (search (if->condition exp))
                              (union (search (if->then exp))
                                     (search (if->else exp)))))
      ((define? exp)     (union (list (define->var exp))
                              (search (define->exp exp))))
      ((set!? exp)     (union (list (set!->var exp)) 
                              (search (set!->exp exp))))
      ; Application:
      ((app? exp)       (reduce union (map search exp) '()))
      (else             (error "unknown expression: " exp))))
  (search ast))





;; Mutable variable analysis and elimination.

;; Mutables variables analysis and elimination happens
;; on a desugared Intermediate Language (1).

;; Mutable variable analysis turns mutable variables 
;; into heap-allocated cells:

;; For any mutable variable mvar:

;; (lambda (... mvar ...) body) 
;;           =>
;; (lambda (... $v ...) 
;;  (let ((mvar (cell $v)))
;;   body))

;; (set! mvar value) => (set-cell! mvar value)

;; mvar => (cell-get mvar)

; mutable-variables : list[symbol]
(define mutable-variables '())

(define (clear-mutables)
  (set! mutable-variables '()))

; mark-mutable : symbol -> void
(define (mark-mutable symbol)
  (set! mutable-variables (cons symbol mutable-variables)))

; is-mutable? : symbol -> boolean
(define (is-mutable? symbol)
  (define (is-in? S)
    (if (not (pair? S))
        #f
        (if (eq? (car S) symbol)
            #t
            (is-in? (cdr S)))))
  (is-in? mutable-variables))

; analyze-mutable-variables : exp -> void
(define (analyze-mutable-variables exp)
  (cond 
    ; Core forms:
    ((const? exp)    (void))
    ((prim? exp)     (void))
    ((ref? exp)      (void))
    ((quote? exp)    (void))
    ((lambda? exp)   (begin
                        (map analyze-mutable-variables (lambda->exp exp))
                        (void)))
    ((set!? exp)     (begin (mark-mutable (set!->var exp))
                            (analyze-mutable-variables (set!->exp exp))))
    ((if? exp)       (begin
                       (analyze-mutable-variables (if->condition exp))
                       (analyze-mutable-variables (if->then exp))
                       (analyze-mutable-variables (if->else exp))))
    
    ; Sugar:
    ((let? exp)      (begin
                       (map analyze-mutable-variables (map cadr (let->bindings exp)))
                       (map analyze-mutable-variables (let->exp exp))
                       (void)))
    ((letrec? exp)   (begin
                       (map analyze-mutable-variables (map cadr (letrec->bindings exp)))
                       (map analyze-mutable-variables (letrec->exp exp))
                       (void)))
    ((begin? exp)    (begin
                       (map analyze-mutable-variables (begin->exps exp))
                       (void)))
    
    ; Application:
    ((app? exp)      (begin 
                       (map analyze-mutable-variables exp)
                       (void)))
    (else            (error "unknown expression type: " exp))))


; wrap-mutables : exp -> exp
(define (wrap-mutables exp globals)
  
  (define (wrap-mutable-formals formals body-exp)
    (if (not (pair? formals))
        body-exp
        (if (is-mutable? (car formals))
            `((lambda (,(car formals))
                ,(wrap-mutable-formals (cdr formals) body-exp))
              (cell ,(car formals)))
            (wrap-mutable-formals (cdr formals) body-exp))))
  
  (cond
    ; Core forms:
    ((const? exp)    exp)
    ((ref? exp)      (if (and (not (member exp globals))
                              (is-mutable? exp))
                         `(cell-get ,exp)
                         exp))
    ((prim? exp)     exp)
    ((quote? exp)    exp)
    ((lambda? exp)   `(lambda ,(lambda->formals exp)
                        ,(wrap-mutable-formals (lambda-formals->list exp)
                                               (wrap-mutables (car (lambda->exp exp)) globals)))) ;; Assume single expr in lambda body, since after CPS phase
    ((set!? exp)     `(,(if (member (set!->var exp) globals)
                            'set-global!
                            'set-cell!) 
                        ,(set!->var exp) 
                        ,(wrap-mutables (set!->exp exp) globals)))
    ((if? exp)       `(if ,(wrap-mutables (if->condition exp) globals)
                          ,(wrap-mutables (if->then exp) globals)
                          ,(wrap-mutables (if->else exp) globals)))
    
    ; Application:
    ((app? exp)      (map (lambda (e) (wrap-mutables e globals)) exp))
    (else            (error "unknown expression type: " exp))))

;; Alpha conversion
;; (aka alpha renaming)
;;
;; This phase is intended to rename identifiers to preserve lexical scoping
;;
;; TODO: does not properly handle renaming builtin functions, would probably need to
;; pass that renaming information downstream
(define (alpha-convert ast globals return-unbound)
  ;; Initialize top-level variables
  (define (initialize-top-level-vars ast fv)
    (if (> (length fv) 0)
       ;; Free variables found, set initial values
       `((lambda ,fv ,ast)
          ,@(map (lambda (_) #f) fv))
        ast))

  ;; Find any defined variables in the given code block
  (define (find-defined-vars ast)
    (filter
      (lambda (expr)
        (not (null? expr)))
      (map
        (lambda (expr)
          (if (define? expr)
            (define->var expr)
           '()))
        ast)))

  ;; Take a list of identifiers and generate a list of 
  ;; renamed pairs, EG: (var . renamed-var)
  (define (make-a-lookup vars)
    (map 
      (lambda (a) (cons a (gensym a))) 
      vars))

  ;; Wrap any defined variables in a lambda, so they can be initialized
  (define (initialize-defined-vars ast vars)
    (if (> (length vars) 0)
      `(((lambda ,vars ,@ast)
         ,@(map (lambda (_) #f) vars)))
       ast))

  ;; Perform actual alpha conversion
  (define (convert ast renamed)
;(write `(DEBUG convert ,ast))
;(write (newline))
    (cond
      ((const? ast) ast)
      ((quote? ast) ast)
      ((ref? ast)
       (let ((renamed (assoc ast renamed)))
         (cond
          (renamed 
            (cdr renamed))
          (else ast))))
      ((define? ast)
       ;; Only internal defines at this point, of form: (define ident value)
       `(set! ,@(map (lambda (a) (convert a renamed)) (cdr ast))))
      ((set!? ast)
         ;; Without define, we have no way of knowing if this was a
         ;; define or a set prior to this phase. But no big deal, since
         ;; the set will still work in either case, so no need to check
         `(set! ,@(map (lambda (a) (convert a renamed)) (cdr ast))))
      ((if? ast)
       ;; Add a failsafe here in case macro expansion added more
       ;; incomplete if expressions.
       ;; FUTURE: append the empty (unprinted) value instead of #f
       (if (if-else? ast)
          `(if ,@(map (lambda (a) (convert a renamed)) (cdr ast)))
          (convert (append ast '(#f)) renamed)))
      ((prim-call? ast)
       (let ((converted
               (cons (car ast) 
                     (map (lambda (a) (convert a renamed))
                          (cdr ast)))))
         (if (precompute-prim-app? converted)
           (eval converted) ;; OK, evaluate at compile time
           converted)))
      ((lambda? ast)
       (let* ((args (lambda-formals->list ast))
              (ltype (lambda-formals-type ast))
              (a-lookup (map (lambda (a) (cons a (gensym a))) args))
              (body (lambda->exp ast))
              (define-vars (find-defined-vars body))
              (defines-a-lookup (make-a-lookup define-vars))
             )
         `(lambda 
            ,(list->lambda-formals
                (map (lambda (p) (cdr p)) a-lookup)  
                ltype)
            ,@(initialize-defined-vars 
                (convert 
                    body 
                   (append a-lookup defines-a-lookup renamed))
                (map (lambda (p) (cdr p)) defines-a-lookup)))))
      ((app? ast)
       (map (lambda (a) (convert a renamed)) ast))
      (else
        (error "unhandled expression: " ast))))

  (let* ((fv (difference (free-vars ast) globals))
         ;; Only find set! and lambda vars
         (bound-vars (union globals (free-vars ast #t)))
         ;; vars never bound in prog, but could be built-in
         (unbound-vars (difference fv bound-vars))
         ;; vars we know nothing about - error!
         (unknown-vars (difference unbound-vars (built-in-syms)))
         )
    (cond
     ((> (length unknown-vars) 0)
      (let ((unbound-to-return (list)))
        (if (member 'eval unknown-vars) 
            (set! unbound-to-return (cons 'eval unbound-to-return)))
        (if (or (member 'read unknown-vars) 
                (member 'read-all unknown-vars))
            (set! unbound-to-return (cons 'read unbound-to-return)))
        (if (and (> (length unbound-to-return) 0) 
                 (= (length unknown-vars) (length unbound-to-return)))
            (return-unbound unbound-to-return)
            ;; TODO: should not report above (eval read) as errors
            (error "Unbound variable(s)" unknown-vars))))
     ((define? ast)
      ;; Deconstruct define so underlying code can assume internal defines
      (let ((body (car ;; Only one member by now
                    (define->exp ast))))
;(write `(DEBUG body ,body))
        (cond
          ((lambda? body)
           (let* ((args (lambda-formals->list body))
                  (ltype (lambda-formals-type body))
                  (a-lookup (map (lambda (a) (cons a (gensym a))) args))
                  (define-vars (find-defined-vars (lambda->exp body)))
                  (defines-a-lookup (make-a-lookup define-vars))
                 )
            ;; Any internal defines need to be initialized within the lambda,
            ;; so the lambda formals are preserved. So we need to deconstruct
            ;; the defined lambda and then reconstruct it, with #f placeholders
            ;; for any internal definitions.
            ;;
            ;; Also, initialize-top-level-vars cannot be used directly due to 
            ;; the required splicing.
            `(define 
               ,(define->var ast)
               (lambda
                 ,(list->lambda-formals
                    (map (lambda (p) (cdr p)) a-lookup)  
                    ltype)
                 ,@(convert (let ((fv* (union
                                         define-vars
                                         (difference fv (built-in-syms))))
                                 (ast* (lambda->exp body)))
                             (if (> (length fv*) 0)
                               `(((lambda ,fv* ,@ast*)
                                  ,@(map (lambda (_) #f) fv*)))
                                ast*))
                            (append a-lookup defines-a-lookup))))))
          (else
            `(define 
               ,(define->var ast)
               ,@(convert (initialize-top-level-vars 
                            (define->exp ast)
                            (difference fv (built-in-syms)))
                          (list)))))))
     (else
      (convert (initialize-top-level-vars 
                 ast 
                 (difference fv (built-in-syms)))
               (list))))))

;; CPS conversion 
;;
;; This is a port of code from the 90-minute Scheme->C Compiler by Marc Feeley
;;
;; Convert intermediate code to continuation-passing style, to allow for
;; first-class continuations and call/cc
;;

(define (cps-convert ast)

  (define (cps ast cont-ast)
    (cond
          ((const? ast)
           (list cont-ast ast))

          ((ref? ast)
           (list cont-ast ast))

          ((quote? ast)
           (list cont-ast ast))

          ((set!? ast)
           (cps-list (cddr ast) ;; expr passed to set
                     (lambda (val)
                       (list cont-ast 
                         `(set! ,(cadr ast) ,@val))))) ;; cadr => variable

          ((if? ast)
           (let ((xform
                  (lambda (cont-ast)
                    (cps-list (list (cadr ast))
                              (lambda (test)
                                 (list 'if
                                       (car test)
                                       (cps (caddr ast)
                                            cont-ast)
                                       (cps (cadddr ast)
                                            cont-ast)))))))
             (if (ref? cont-ast) ; prevent combinatorial explosion
                 (xform cont-ast)
                 (let ((k (gensym 'k)))
                    (list (list 'lambda
                           (list k)
                           (xform k))
                          cont-ast)))))

          ((prim-call? ast)
           (cps-list (cdr ast) ; args to primitive function
                     (lambda (args)
                        (list cont-ast
                            `(,(car ast) ; op
                              ,@args)))))

          ((lambda? ast)
           (let ((k (gensym 'k))
                 (ltype (lambda-formals-type ast)))
             (list cont-ast
                   `(lambda
                      ,(list->lambda-formals
                         (cons k (cadr ast)) ; lam params
                         (if (equal? ltype 'args:varargs)
                             'args:fixed-with-varargs ;; OK? promote due to k
                             ltype))
                      ,(cps-seq (cddr ast) k)))))

;
; TODO: begin is expanded already by desugar code... better to do it here?
;          ((seq? ast)
;           (cps-seq (ast-subx ast) cont-ast))

          ((app? ast)
           (let ((fn (app->fun ast)))
             (cond
              ((lambda? fn)
                 (cps-list (app->args ast)
                           (lambda (vals)
                             (cons (list
                                     'lambda
                                     (lambda->formals fn)
                                     (cps-seq (cddr fn) ;(ast-subx fn)
                                                    cont-ast))
                                    vals))))
              (else
                 (cps-list ast ;(ast-subx ast)
                           (lambda (args)
                              (cons (car args)
                                    (cons cont-ast
                                          (cdr args)))))))))

          (else
           (error "unknown ast" ast))))

  (define (cps-list asts inner)
    (define (body x)
      (cps-list (cdr asts)
                (lambda (new-asts)
                  (inner (cons x new-asts)))))

    (cond ((null? asts)
           (inner '()))
          ((or (const? (car asts))
               (ref? (car asts)))
           (body (car asts)))
          (else
           (let ((r (gensym 'r))) ;(new-var 'r)))
             (cps (car asts)
                  `(lambda (,r) ,(body r)))))))

  (define (cps-seq asts cont-ast)
    (cond ((null? asts)
           (list cont-ast #f))
          ((null? (cdr asts))
           (cps (car asts) cont-ast))
          (else
           (let ((r (gensym 'r)))
             (cps (car asts)
                  `(lambda
                     (,r)
                    ,(cps-seq (cdr asts) cont-ast)))))))

  ;; Remove dummy symbol inserted into define forms converted to CPS
  (define (remove-unused ast)
    (list (car ast) (cadr ast) (cadddr ast)))

  (let* ((global-def? (define? ast)) ;; No internal defines by this phase
         (ast-cps
          (if global-def?
            (remove-unused
              `(define ,(define->var ast)
                ,@(let ((k (gensym 'k))
                        (r (gensym 'r)))
                   (cps (car (define->exp ast)) 'unused))))
            (cps ast '%halt))))
    ast-cps))


;; Closure-conversion.
;;
;; Closure conversion eliminates all of the free variables from every
;; lambda term.
;;
;; The code below is based on a fusion of a port of the 90-min-scc code by 
;; Marc Feeley and the closure conversion code in Matt Might's scheme->c 
;; compiler.

(define (pos-in-list x lst)
  (let loop ((lst lst) (i 0))
    (cond ((not (pair? lst)) #f)
          ((eq? (car lst) x) i)
          (else 
            (loop (cdr lst) (+ i 1))))))

(define (closure-convert exp globals)
 (define (convert exp self-var free-var-lst)
  (define (cc exp)
   (cond
    ((const? exp)        exp)
    ((quote? exp)        exp)
    ((ref? exp)
      (let ((i (pos-in-list exp free-var-lst)))
        (if i
            `(%closure-ref
              ,self-var
              ,(+ i 1))
            exp)))
    ((or
        (tagged-list? '%closure-ref exp)
        (tagged-list? '%closure exp)
        (prim-call? exp))
        `(,(car exp)
          ,@(map cc (cdr exp)))) ;; TODO: need to splice?
    ((set!? exp)  `(set! ,(set!->var exp)
                         ,(cc (set!->exp exp))))
    ((lambda? exp)
     (let* ((new-self-var (gensym 'self))
            (body  (lambda->exp exp))
            (new-free-vars 
              (difference 
                (difference (free-vars body) (lambda-formals->list exp))
                globals)))
       `(%closure
          (lambda
            ,(list->lambda-formals
               (cons new-self-var (lambda-formals->list exp))
               (lambda-formals-type exp))
            ,(convert (car body) new-self-var new-free-vars)) ;; TODO: should this be a map??? was a list in 90-min-scc.
          ,@(map (lambda (v) ;; TODO: splice here?
                    (cc v))
            new-free-vars))))
    ((if? exp)  `(if ,@(map cc (cdr exp))))
    ((cell? exp)       `(cell ,(cc (cell->value exp))))
    ((cell-get? exp)   `(cell-get ,(cc (cell-get->cell exp))))
    ((set-cell!? exp)  `(set-cell! ,(cc (set-cell!->cell exp))
                                   ,(cc (set-cell!->value exp))))
    ((app? exp)
     (let ((fn (car exp))
           (args (map cc (cdr exp))))
       (if (lambda? fn)
           (let* ((body  (lambda->exp fn))
                  (new-free-vars 
                    (difference
                      (difference (free-vars body) (lambda-formals->list fn))
                      globals))
                  (new-free-vars? (> (length new-free-vars) 0)))
               (if new-free-vars?
                 ; Free vars, create a closure for them
                 (let* ((new-self-var (gensym 'self)))
                   `((%closure 
                        (lambda
                          ,(list->lambda-formals
                             (cons new-self-var (lambda-formals->list fn))
                             (lambda-formals-type fn))
                          ,(convert (car body) new-self-var new-free-vars))
                        ,@(map (lambda (v) (cc v))
                               new-free-vars))
                     ,@args))
                 ; No free vars, just create simple lambda
                 `((lambda ,(lambda->formals fn)
                           ,@(map cc body))
                   ,@args)))
           (let ((f (cc fn)))
            `((%closure-ref ,f 0)
              ,f
              ,@args)))))
    (else                
      (error "unhandled exp: " exp))))
  (cc exp))

 `(lambda ()
    ,(convert exp #f '())))

; Suitable definitions for the cell functions:
;(define (cell value) (lambda (get? new-value) 
;                       (if get? value (set! value new-value))))
;(define (set-cell! c v) (c #f v))
;(define (cell-get c) (c #t #t))
