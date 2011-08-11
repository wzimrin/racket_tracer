#lang racket

(require "buttons.rkt")

(require [except-in lang/htdp-advanced
                    #%app define lambda require #%module-begin let local
                    let* letrec image? λ and or if
                    check-expect check-within check-error check-member-of check-range
                    when begin]
         [prefix-in asl:
                    [only-in lang/htdp-advanced and or if when begin]]
         [for-meta 1
                   [only-in racket/list
                            first last cons? take make-list]]
         test-engine/racket-tests
         syntax-color/scheme-lexer
         ;racket/pretty
         [only-in net/sendurl
                  send-url/contents]
         [only-in planet/resolver
                  resolve-planet-path]
         [only-in web-server/templates
                  include-template]
         2htdp/image
         [except-in 2htdp/universe big-bang]
         [prefix-in universe:
                    [only-in 2htdp/universe
                             big-bang]]
         [only-in racket/gui
                  message-box]
         ;syntax/toplevel
         [for-syntax racket/port
                     racket/list]
         ;images in source to browser
         net/base64
         file/convertible
         mzlib/pconvert ;print-convert?
         (planet dherman/json:3:0))

(provide require only-in except-in prefix-in combine-in
         provide all-defined-out all-from-out 
         rename-out except-out prefix-out struct-out combine-out protect-out
         [rename-out (app-recorder #%app)
                     (check-expect-recorder check-expect)
                     (check-within-recorder check-within)
                     (check-error-recorder check-error)
                     (check-range-recorder check-range)
                     (check-member-of-recorder check-member-of)
                     (custom-define define)
                     (custom-lambda lambda)
                     (custom-lambda λ)
                     (big-bang-recorder big-bang)
                     (custom-module-begin #%module-begin)
                     
                     (asl:and and)
                     (asl:or or)
                     (asl:if if)
                     (asl:when when)
                     (asl:begin begin)
                     
                     (cs019:local local)
                     (cs019:let* let*)
                     (cs019:letrec letrec)
                     (cs019:let let)]
         [all-from-out lang/htdp-advanced
                       2htdp/image
                       2htdp/universe]
         trace
         trace-all
         trace-failed-checks
         trace-explicit)

;----------------------------------------------------------------
;                     cs019 lang definitions
;----------------------------------------------------------------

(define-syntax (top-level body-exprs)
  (syntax-case body-exprs ()
    [(_ bodies ...)
     #'(#%module-begin bodies ... (run-tests) (display-results))]))

(define-for-syntax (check-bodies bodies e name)
  (let ([body-stxs (syntax-e bodies)])
    (cond 
      [(empty? body-stxs)
       (raise-syntax-error
        name 
        "expected an expression for the body, but nothing's there"
        e)]
      [(cons? (cdr body-stxs))
       (raise-syntax-error
        name
        (format "expected only one expression for the body, but found ~s"
                (length body-stxs))
        e)]
      [else (void)])))

(define-syntax-rule (no-begin-redef name orig-name)
  (define-syntax (name e)
    (syntax-case e ()
      [(_ header . bodies)
       (begin
         (check-bodies #'bodies e 'orig-name)
         (syntax/loc e
           (orig-name header . bodies)))])))

(no-begin-redef cs019:let let)
(no-begin-redef cs019:let* let*)
(no-begin-redef cs019:letrec letrec)
(no-begin-redef cs019:local local)

;----------------------------------------------------------------
;                     tracer definitions
;----------------------------------------------------------------

;the actual struct that stores our data
(struct node (name prefix func result actual kids idx span src-idx src-span used? title has-title) #:mutable #:transparent)

;creates a node with no result or children
;takes a name, function object, a list of the arguments, a call index and span, and a definition index and span
(define (create-node name func actual idx span src-idx src-span)
  (node name "" func 'no-result actual empty idx span src-idx src-span #f #f #f))

;adds a kid k to node n
(define (add-kid n k)
  (set-node-kids! n (cons k (node-kids n))))

;a wrapper for exceptions - it lets us distinguish between an exception that was created and returned by user code
;and an exception that we caught and used as the return value
; not all exceptions that are handled by the exception handler are exn -- if the user uses
; (raise v), v: any, not specifically an exn
(struct exn-wrapper (exn))
(define (exn-wrapper-message w)
  (let ([unwrapped (exn-wrapper-exn w)])
    (if (exn? unwrapped)
        (exn-message unwrapped)
        (format "~s" unwrapped))))

;the parent node for all failing check expects
(define top-ce-node (create-node 'top-ce-node #f empty 0 0 0 0))

;the parent node for all big-bangs
(define top-big-bang-node
  (create-node 'top-big-bang-node #f empty 0 0 0 0))

;the parent node for all normal traces
(define top-node (create-node 'top-node #f empty 0 0 0 0))

;the current definition we are in
(define current-call (make-parameter #f))

;defines a macro for a (trace-*) form
;when one of these is the first expression in the file, module-begin finds it, takes
;the appropriate action, and removes it.  If we ever see this elsewhere, that should be
;an error
(define-syntax-rule (trace-macro name)
  (define-syntax (name e)
    (raise-syntax-error 'name  "must be the first expression in the file" e)))

;trace-all makes every top level expression and failed check-expect traced
(trace-macro trace-all)
;traces failed check-expects and any top level expression wrapped in (trace )
(trace-macro trace-explicit)
;trace-failed-checks traces every failed check-expect, but ignores normal top level expressions
(trace-macro trace-failed-checks)

(define-for-syntax trace? (box #f))
(define-for-syntax trace-macro-on? (box #f))

;if (trace-explicit) is set, causes the wrapped expression to be traced
;otherwise, simply evaluates the wrapped expressions
(define-syntax (trace e)
  (syntax-case e ()
    [(_ . bodies)
     (if (unbox trace-macro-on?)
         #'(parameterize ([current-call top-node])
             . bodies)
         #'(begin . bodies))]))

;the actual fun that was applied with the last use of #%app
(define current-fun (make-parameter #f))
;the node that was created with the last use of #%app
(define current-app-call (make-parameter empty))

;a hash table of all correct and failing check expects
(define ce-hash (make-hash))

;get information on a check-*, given a list of function and arguments
(define (ce-info n)
  (let* ([key (list (node-func n) (node-actual n))]
         [l (hash-ref ce-hash key (list #f #f #f))])
    (values (first l) (second l) (third l))))

;takes a hashtable, a key (list function arguments), the index and span of a check expect, and whether the check expect succeeded
;adds the the check-expect to the hash
(define (add-to-ce-hash key idx span success)
  (hash-set! ce-hash key (list idx span success))) 

(define (lambda-body-fun args body name fun idx span)
  (if (current-call)
      (let* ([app-call? (eq? fun (current-fun))]
             [n (if app-call?
                    (current-app-call)
                    (create-node name fun args
                                 0 0
                                 idx
                                 span))])
        (cond
          [app-call?
           (begin (set-node-src-idx! n idx)
                  (set-node-src-span! n span))]
          [(node? (current-app-call))
           (add-kid (current-app-call) n)]
          [#t (add-kid (current-call) n)])
        (when (node? (current-app-call))
          (set-node-used?! (current-app-call) #t))
        (parameterize ([current-call n])
          (let ([result (with-handlers ([identity exn-wrapper])
                          (body))])
            (set-node-result! n result)
            (if (exn-wrapper? result)
                (error "Error")
                result))))
      (body)))

;generates the interior of an annotated function definition
;takes a syntax object of a list of arguments, a syntax object for the body,
;a syntax object that is the display name of the function, the original syntax object
;of the function definition, and a syntax object that can be used to refer to the function
(define-for-syntax (lambda-body args bodies name orig fun)
  #`(lambda-body-fun #,args
                     (lambda () #,@bodies)
                     '#,name #,fun
                     #,(syntax-position orig) #,(syntax-span orig)))

;traces a lambda, need temp to know which function is currently being applied (the actual lambda)
;not our lambda body. at runtime only have access to procedure, so knowing e doesn't help
(define-syntax (custom-lambda e)
 (syntax-case e ()
    [(_ args . bodies)
     (begin
       (check-bodies #'bodies e 'lambda)
       (if (unbox trace?)
           (quasisyntax/loc e
             (letrec ([temp (lambda args
                              #,(lambda-body (syntax/loc e
                                               (list . args))
                                             #'bodies
                                             #'lambda e #'temp))])
               (procedure-rename temp 'lambda)))
           (syntax/loc e
             (lambda args . bodies))))]))

;traces a define
(define-syntax (custom-define e)
  (syntax-case e ()
    [(_ header . tail)
     (check-bodies #'tail e 'define)
     (if (unbox trace?)
      (syntax-case e ()
        [(_ (fun-expr arg-expr ...) . bodies)
         (quasisyntax/loc e
           (define (fun-expr arg-expr ...)
             #,(lambda-body (syntax/loc e
                              (list arg-expr ...))
                            #'bodies
                            #'fun-expr e #'fun-expr)))]
        [(_ name value)
         (syntax-case #'value (custom-lambda)
           [(custom-lambda (arg-expr ...) . bodies)
            (begin
              (check-bodies #'bodies e 'lambda)
              (syntax/loc e
                (custom-define (name arg-expr ...) . bodies)))]
           [_ #'(define name value)])])
      (syntax/loc e
        (define header . tail)))]))

;gets the leftmost element out of a nested list
(define (function-sym datum)
  (if (cons? datum)
      (function-sym (first datum))
      datum))

(define (custom-apply fun args name idx span)
  (if (current-call)
      (let* ([n (create-node (function-sym name) fun args
                             idx span 0 0)]
             [result (with-handlers ([identity exn-wrapper])
                       (parameterize ([current-fun fun]
                                      [current-app-call n])
                         (apply fun args)))])
        (when (or (node-used? n)
                  (exn-wrapper? result))
          (set-node-result! n result)
          (add-kid (current-call) n))
        (if (exn-wrapper? result)
            (error "Error")
            result))
      (apply fun args)))

;takes a syntax object that will be bound at runtime to a the evaluated form of the function,
;a syntax object that will be bound at runtime to an evaluated list of the arguments to the function
;the original syntax object of the application, and the syntax of the function
;it traces the application of the function to its arguments
(define-syntax (apply-recorder e)
  (syntax-case e ()
    [(_ fun args orig fun-expr)
     (with-syntax ([idx (syntax-position #'orig)]
                   [span (syntax-span #'orig)])
       (syntax/loc #'orig
         (custom-apply fun args 'fun-expr idx span)))]))

;records all function calls we care about - redefinition of #%app
(define-syntax (app-recorder e)
  (syntax-case e ()
    [(_ fun-expr arg-expr ...)
     (if (unbox trace?)
         (quasisyntax/loc e
           (apply-recorder fun-expr (list arg-expr ...) #,e fun-expr))
         (syntax/loc e
           (#%plain-app fun-expr arg-expr ...)))]))

;helper function - takes a list of names and how long the list should be, and
;returns a list of names of the correct length, dropping names off the back of the list
;or repeating the last name as necessary
(define-for-syntax (fix-names count names)
  (let ([name-count (length names)])
    (if (>= name-count count)
        (take names count)
        (append (take names (sub1 name-count))
                (make-list (- count name-count -1)
                           (last names))))))

;records a big-bang call - it wraps each passed in function in its own node
(define-syntax (big-bang-recorder e)
  (syntax-case e ()
    [(_ world . handlers)
     (if (unbox trace?)
         (quasisyntax/loc e
           (begin 
             (define current-big-bang-node
               (create-node 'big-bang #f empty
                            #,(syntax-position e)
                            #,(syntax-span e)
                            0 0))
             (add-kid top-big-bang-node current-big-bang-node)
             (universe:big-bang
              world
              #,@(map (lambda (handler)
                        (syntax-case handler ()
                          [(n f . o)
                           (quasisyntax/loc handler
                             [n (let ([f-value f])
                                    (lambda args
                                      (let* ([node
                                              (create-node 'n #f args
                                                           #,(syntax-position handler)
                                                           #,(syntax-span handler)
                                                           0 0)]
                                             [result
                                              (parameterize ([current-call node])
                                                (with-handlers ([identity exn-wrapper])
                                                  (apply f-value args)))])
                                        (set-node-result! node result)
                                        #,@(if (member (syntax->datum #'n)
                                                       '(to-draw on-draw))
                                               #'((set-node-title! node result)
                                                  (set-node-has-title! node #t))
                                               #'())
                                        (add-kid current-big-bang-node node)
                                        (if (exn-wrapper? result)
                                            (error "Error")
                                            result))))
                                . o])]))
                      (syntax-e #'handlers)))))
         (syntax/loc e 
           (universe:big-bang world . handlers)))]))

;a macro that, when called, defines a macro to replace a check-* form
;takes the name that the new macro should be called, the check-* form it is supposed to replace
;a function to determine if the check-* passed (it will receive the arguments in order)
;and a list of the names for the child nodes of the check-*
(define-syntax-rule (generalized-check-expect-recorder name original-name
                                                       passed? node-names-stx)
  (define-syntax (name e)
    (with-syntax ([idx (syntax-position e)]
                  [span (syntax-span e)]
                  [actual 'actual]
                  [expected 'expected])
      (syntax-case e ()
        [(_ actual-stx . expected-stxs)
         (if (unbox trace?)
             (let* ([datum (syntax-e #'actual-stx)]
                    [func-stx (if (pair? datum)
                                  (car datum)
                                  datum)]
                    [args-stx (when (pair? datum)
                                (cdr datum))]
                    [expected-datums (syntax-e #'expected-stxs)]
                    [node-names 'node-names-stx])
               (quasisyntax/loc e
                 (begin 
                   (define parent-node;the top node for the check-expect
                     (create-node '#,func-stx #f empty idx span 0 0))
                   (set-node-prefix! parent-node
                                     (format "~s" 'original-name))
                   (original-name
                    ;the actual value that the check expect expects is evaluated last, so put cleanup code here
                    (let* ([actual-node (create-node '#,(first node-names);the node for the actual value
                                                     #f
                                                     empty
                                                     #,(syntax-position #'actual-stx)
                                                     #,(syntax-span #'actual-stx)
                                                     0
                                                     0)])
                      (let-values ([(func args);we must evaluate the func and the args within a parameterize, but we need
                                    ;to evaluate them and store them separately so we can add the call to ce-hash
                                    ;we also need to check if there the actual is an application - if not, don't do anything here
                                    #,(if (pair? datum)
                                          #`(parameterize ([current-call actual-node])
                                              (with-handlers ([identity
                                                               ;on error, we can just store a wrapped exception in func
                                                               (lambda (exn)
                                                                 (values
                                                                  (exn-wrapper exn)
                                                                  #f))])
                                                (values #,func-stx (list . #,args-stx))))
                                          #'(values #f #f))])
                        ;calculate the result for the actual node
                        ;if we calculated a func and args above, apply them with apply-recorder
                        ;otherwise, just evaluate actual-stx
                        (set-node-result! actual-node
                                          (parameterize ([current-call actual-node])
                                            (with-handlers ([identity exn-wrapper])
                                              #,(if (pair? datum)
                                                    #`(if (exn-wrapper? func)
                                                          func
                                                          (apply-recorder
                                                           func args 
                                                           actual-stx #,func-stx))
                                                    func-stx))))
                        ;decide if the check-* passed, using the provided function
                        (let ([ce-correct? (apply passed?
                                                  (cons (node-result actual-node)
                                                        (reverse
                                                         (map node-result
                                                              (node-kids parent-node)))))])
                          ;add the actual node to the end of the parent-nodes kids (ie, where it would have gone if it had been evaluated first)
                          (set-node-kids! parent-node (append (node-kids parent-node) 
                                                              (list actual-node)))
                          ;add the check to ce-hash
                          #,(when (pair? datum)
                              #`(add-to-ce-hash (list func args)
                                                idx
                                                span
                                                ce-correct?))
                          ;if we failed the check, add the ce to the top node
                          (when (not ce-correct?)
                            (set-node-result! parent-node #f)
                            (add-kid top-ce-node parent-node))))
                      ;if actual-node threw an exception, re-throw it here
                      ;if not, return the result to the actual check-*
                      (if (exn-wrapper? (node-result actual-node))
                          (error "Error")
                          (node-result actual-node)))
                    ;create the code for the expected values
                    ;there can be an arbitrary number of values
                    ;we use a map instead of ... because we need to map over the expected values
                    ;and the correct name at the same time
                    #,@(map (lambda (expected-stx name)
                              #`(let* ([expected-node;create a node for this expected value
                                        (create-node '#,name
                                                     #f
                                                     empty
                                                     #,(syntax-position
                                                        expected-stx)
                                                     #,(syntax-span expected-stx)
                                                     0
                                                     0)]
                                       ;calculate its result (no need to worry about function/arguments, since we don't use them individually)
                                       [result (parameterize ([current-call expected-node])
                                                 (with-handlers ([identity exn-wrapper])
                                                   #,expected-stx))])
                                  (add-kid parent-node expected-node)
                                  (set-node-result! expected-node result)
                                  (if (exn-wrapper? result)
                                      (begin;on error, the underlying check-* will abort, but so we need to finish up node creation/addition here
                                        (set-node-result! parent-node #f)
                                        (add-kid top-ce-node parent-node)
                                        (error "Error"))
                                      result)))
                            expected-datums
                            (fix-names (length expected-datums)
                                       (cdr node-names)))))))
             (syntax/loc e
               (original-name actual-stx . expected-stxs)))]))))

;redefinition of check-expect
(generalized-check-expect-recorder
 check-expect-recorder
 check-expect
 (lambda (actual expected)
   (equal? actual expected))
 (test expected))

;redefinition of check-within
(generalized-check-expect-recorder
 check-within-recorder
 check-within
 (lambda (actual value delta)
   (and (number? actual)
        (number? value)
        (number? delta)
        (<= (abs (- actual value))
            delta)))
 (test expected delta))

;redefinition of check-error
(generalized-check-expect-recorder
 check-error-recorder
 check-error
 (lambda (actual [message #f])
   (and (exn-wrapper? actual)
        (or (not message)
            (equal? (exn-wrapper-message actual) message))))
 (test msg))

;redefinition of check-member-of
(generalized-check-expect-recorder
 check-member-of-recorder
 check-member-of
 (lambda (actual . possibilities)
   (member actual possibilities))
 (test expected))

;redefinition of check-range
(generalized-check-expect-recorder
 check-range-recorder
 check-range
 (lambda (actual low high)
   (and (number? actual)
        (number? low)
        (number? high)
        (< actual high)
        (> actual low)))
 (test min max))

;returns the base64 encoding of the image as a png byte string
(define (get-base64 img)
  (base64-encode (convert img 'png-bytes)))

;returns the data-uri encoding of an image
(define (uri-string img)
  (string-append "data:image/png;charset=utf-8;base64,"
                 (bytes->string/utf-8 (get-base64 img))))

;converts an image to a jsexpr
(define (json-image img)
  (hasheq 'type "image"
          'src (uri-string img)))

;print the result string readably
(define (format-nicely x depth width literal)
  (cond
    [(image? x)
     (json-image x)]
    [(exn-wrapper? x)
     (hasheq 'type "error"
             'message (exn-wrapper-message x))]
    [#t
     (let* ([p (open-output-string "out")])
       ;set columns and depth
       (parameterize ([pretty-print-columns width]
                      [pretty-print-depth depth]
                      [constructor-style-printing #t])
         (if (and (procedure? x) (object-name x))
             (display (object-name x) p)            
             (pretty-write (print-convert x) p)))
       ;return what was printed
       (hasheq 'type "value"
               'value (get-output-string p)))]))

;converts a single node to a jsexpr
(define (node->json t)
  ;calls format-nicely on the elements of the list and formats that into a 
  ;javascript list
  (local [(define (format-list lst depth literal)
            (map (lambda (x)
                   (format-nicely x depth 40 literal))
                 lst))]
    (let-values ([(ce-idx ce-span ce-correct?) (ce-info t)])
      (hasheq 'name
              (format "~a" (node-name t))
              'actuals
              (format-list (node-actual t) #f #t)
              'actualsShort
              (format-list (node-actual t) 2 #t)
              'result
              (format-nicely (node-result t) #f 40 #t)
              'resultShort
              (format-nicely (node-result t) 2 40 #t)
              'idx
              (node-idx t)
              'span
              (node-span t)
              'srcIdx
              (node-src-idx t)
              'srcSpan
              (node-src-span t)
              'children
              (map node->json (reverse (node-kids t)))
              'ceIdx
              ce-idx
              'ceSpan
              ce-span
              'ceCorrect
              ce-correct?
              'prefix
              (node-prefix t)
              'title
              (if (node-has-title t)
                  (format-nicely (node-title t) #f 40 #t)
                  (hasheq 'type "none"))))))

;takes a port and a list of characters/specials
;returns a list of (list syntax-type content)
(define (lex-port p actual)
  (let*-values ([(str type junk start end) (scheme-lexer p)]
                [(span) (and start end
                             (if (equal? str "λ")
                                 1
                                 (- end start)))])
    (if (eq? type 'eof)
        empty
        (cons (list type (take actual span))
              (lex-port p (drop actual span))))))

;takes a list of characters/specials
;returns a jsexpr with the source and color information
(define (colors src)
  (apply append
         (map (lambda (lst)
                (let ([type (format "~a" (first lst))])
                  (if (andmap (lambda (x)
                                (and (char? x)
                                     (not (equal? x #\λ))))
                              (second lst))
                      (list (hasheq 'type "string"
                                    'color type
                                    'text (list->string (second lst))))
                      (map (lambda (val)
                             (cond 
                               [(image? val)
                                (hasheq 'type "image"
                                         'color type
                                         'src (uri-string val))]
                               [(equal? val #\λ)
                                (hasheq 'type "html"
                                        'color type
                                        'html "&lambda;")]
                               [else
                                (hasheq 'type "string"
                                        'color type
                                        'text (format "~a" val))]))
                           (second lst)))))
              (lex-port (let-values ([(in out) (make-pipe-with-specials)])
                          (for ([x src])
                            (if (char? x)
                                (display x out)
                                (write-special x out)))
                          (close-output-port out)
                          in)
                        src))))

;converts a node tree to json
(define (tree->json call)
  (jsexpr->json (node->json call)))

;converts a list of characters/specials to json
(define (code->json src)
  (jsexpr->json (colors src)))

;creates a html page
(define (page name o errored src)
  (let* ([title (string-append name " Trace")]
        [CSSPort (open-input-file (resolve-planet-path 
                                         '(planet tracer/tracer/tracer.css)))]
        [tracerCSS (port->string CSSPort)]
        [sideImageSrc (format "~s" (uri-string normal-side-arrow))]
        [downImageSrc (format "~s" (uri-string normal-down-arrow))]
        [correctCEImageSrc (format "~s" (uri-string normal-correct-checkbox))]
        [failedCEImageSrc (format "~s" (uri-string normal-failed-checkbox))]
        [correctCEImageSelSrc (format "~s" (uri-string highlight-correct-checkbox))]
        [failedCEImageSelSrc (format "~s" (uri-string highlight-failed-checkbox))]
        [toDefImageSrc (format "~s" (uri-string normal-src-button))]
        [toDefImageSelSrc (format "~s" (uri-string highlight-src-button))]
        [jQueryPort (open-input-file (resolve-planet-path
                                         '(planet tracer/tracer/jquery.js)))]
         
        [jQuery (port->string jQueryPort)]
        [tracerJSPort (open-input-file (resolve-planet-path
                                         '(planet tracer/tracer/tracer.js)))]
        [tracerJS (port->string tracerJSPort)]
        [theTrace (tree->json top-node)]
        [ceTrace (tree->json top-ce-node)]
        [bigBangTrace (tree->json top-big-bang-node)]
        [code (code->json src)]
        [offset o]
        [errored (jsexpr->json errored)]
        [template (include-template "index.html")])
    (close-input-port CSSPort)
    (close-input-port jQueryPort)
    (close-input-port tracerJSPort)
    template))

;adds after-body to the end, and deals with the extra information provided by the reader
(define-syntax (custom-module-begin stx)
  (syntax-case stx ()
    [(_ name source offset . bodies)
     (let* ([bodies-list (syntax-e #'bodies)]
            [first-expression (and (cons? bodies-list)
                                   (syntax->datum (first bodies-list)))])
       (if (member first-expression 
                  '((trace-all)
                    (trace-failed-checks)
                    (trace-explicit))) 
           (begin
             (set-box! trace? #t)
             (when (equal? first-expression '(trace-explicit))
               (set-box! trace-macro-on? #t))
             #`(#%plain-module-begin
                (current-call
                 #,(cond
                     [(equal? first-expression '(trace-all))
                      #'top-node]
                     [(member first-expression
                              '((trace-failed-checks)
                                (trace-explicit)))
                      #f]))
                ;Code to run after users program has run
                ;If nothing to trace, message to user
                ;If code to trace, generates and displays page
                (define (final errored)
                  (display-results)
                  ;If empty trace generate error message
                  (if (and (empty? (node-kids top-node))
                           (empty? (node-kids top-ce-node))
                           (empty? (node-kids top-big-bang-node)))
                      #,(cond 
                          [(member first-expression
                                   '((trace-all)
                                     (trace-explicit)))
                           #'(message-box "Error" 
                                          "There is nothing to trace in this file. Did you define any functions in this file? Are they called from this file?" 
                                          #f 
                                          '(ok stop))]
                          [(equal? first-expression '(trace-failed-checks))
                           #'(message-box "Congratulations!"
                                          "All your check-expects passed."
                                          #f
                                          '(ok))])
                      (send-url/contents (page name offset errored source))))
                ;Set exception handler to allow tracing of functions that error out
                (uncaught-exception-handler (lambda (x)
                                              (displayln (exn-message x))
                                              (final #t)
                                              ((error-escape-handler))))
                #,@(datum->syntax #f (cdr bodies-list))
                (run-tests)
                (final #f)))
           #'(top-level . bodies)))]))
