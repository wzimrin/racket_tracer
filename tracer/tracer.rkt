#lang racket

(require "buttons.rkt")

(require [except-in lang/htdp-intermediate-lambda
                    #%app define lambda require #%module-begin let local
                    let* letrec image? λ and or if
                    check-expect check-within check-error check-member-of check-range]
         [prefix-in isl:
                    [only-in lang/htdp-intermediate-lambda
                             define lambda let require local image? and or if]]
         [for-meta 1
                   [only-in racket/list
                            first last cons? take make-list]]
         test-engine/racket-tests
         syntax-color/scheme-lexer
         racket/pretty
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
         syntax/toplevel
         [for-syntax racket/port]
         net/base64
         file/convertible
         mzlib/pconvert
         (planet dherman/json:3:0))

(provide local let* letrec require only-in except-in prefix-in let
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
                     
                     (isl:and and)
                     (isl:or or)
                     (isl:if if)]
         [all-from-out lang/htdp-intermediate-lambda
                       2htdp/image
                       2htdp/universe]
         #%module-begin
         trace)

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
(struct exn-wrapper (exn))
(define (exn-wrapper-message w)
  (let ([unwrapped (exn-wrapper-exn w)])
    (if (exn? unwrapped)
        (exn-message unwrapped)
        (format "~s" unwrapped))))

;the parent node for all failing check expects
(define top-ce-node (create-node 'CE-top-level #f empty 0 0 0 0))

;the parent node for all big-bangs
(define top-big-bang-node
  (create-node 'top-big-bang-node #f empty 0 0 0 0))

;the parent node for all normal traces
(define top-node (create-node 'top-level #f empty 0 0 0 0))

;the current definition we are in
(define current-call (make-parameter top-node))

;macro to determine whether tracing is on or off
;(trace #t) turns tracing on for all executions below the call
;(trace #f) turns tracing off similarly
;(trace #t body ...) turns tracing on for the body expressions but does not affect tracing elsewhere
;(trace #f body ...) turns tracing off similarly
;the trace macro only affects runtime behavior; it does not affect the code generated
(define-syntax (trace e)
  (syntax-case e ()
    [(_ on? . bodies)
     (with-syntax ([new-current #'(if on? top-node #f)])
       (if (cons? (syntax-e #'bodies))
           #'(parameterize ([current-call new-current])
               . bodies)
           #'(current-call new-current)))]))

;the position (ala syntax-position) of the last use of #%app
(define current-idx (make-parameter 0))
;the span of the last use of #%app
(define current-span (make-parameter 0))
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

;generates the interior of an annotated function definition
;takes a syntax object of a list of arguments, a syntax object for the body,
;a syntax object that is the display name of the function, the original syntax object
;of the function definition, and a syntax object that can be used to refer to the function
(define-for-syntax (lambda-body args body name orig fun)
  #`(let ([body-thunk (lambda () #,body)])
      (if (current-call)
          (let* ([app-call? (eq? #,fun (current-fun))]
                 [n (if app-call?
                        (current-app-call)
                        (create-node '#,name #,fun #,args
                                     0 0
                                     #,(syntax-position orig)
                                     #,(syntax-span orig)))])
            (cond
              [app-call?
               (begin (set-node-src-idx! n #,(syntax-position orig))
                      (set-node-src-span! n #,(syntax-span orig)))]
              [(node? (current-app-call))
               (add-kid (current-app-call) n)]
              [#t (add-kid (current-call) n)])
            (when (node? (current-app-call))
              (set-node-used?! (current-app-call) #t))
            (parameterize ([current-call n])
              (let ([result (with-handlers ([identity exn-wrapper])
                              (body-thunk))])
                (set-node-result! n result)
                (if (exn-wrapper? result)
                    (error "Error")
                    result))))
          (body-thunk))))

;traces a lambda
(define-syntax (custom-lambda-name e)
  (syntax-case e ()
    [(_ name args body)
     (let ([sym (gensym)])
       #`(letrec ([#,sym (lambda args
                           #,(lambda-body #'(list . args) #'body #'name e sym))])
           (procedure-rename #,sym 'name)))]))

(define-syntax-rule (custom-lambda args body)
  (custom-lambda-name lambda args body))

;traces a define
(define-syntax (custom-define e)
  (syntax-case e (lambda)
    [(_ (fun-expr arg-expr ...) body)
     #`(define (fun-expr arg-expr ...)
         #,(lambda-body #'(list arg-expr ...) #'body #'fun-expr e #'fun-expr))]
    [(_ fun-expr (lambda (arg-expr ...) body))
     #'(custom-define (fun-expr arg-expr ...) body)]
    [(_ id val)
     #'(define id val)]))

;gets the leftmost element out of a nested list
(define (function-sym datum)
  (if (cons? datum)
      (function-sym (first datum))
      datum))

;takes a syntax object that will be bound at runtime to a the evaluated form of the function,
;a syntax object that will be bound at runtime to an evaluated list of the arguments to the function
;the original syntax object of the application, and the syntax of the function
;it traces the application of the function to its arguments
(define-syntax (apply-recorder e)
  (syntax-case e ()
    [(_ fun args e fun-expr)
     (with-syntax ([idx (syntax-position #'e)]
                   [span (syntax-span #'e)])
       #'(if (current-call)
             (let* ([n (create-node (function-sym 'fun-expr) fun args
                                    idx span 0 0)]
                    [result (with-handlers ([identity exn-wrapper])
                              (parameterize ([current-idx idx]
                                             [current-span span]
                                             [current-fun fun]
                                             [current-app-call n])
                                (apply fun args)))])
               (when (or (node-used? n)
                         (exn-wrapper? result))
                 (set-node-result! n result)
                 (add-kid (current-call) n))
               (if (exn-wrapper? result)
                   (error "Error")
                   result))
             (apply fun args)))]))

;records all function calls we care about - redefinition of #%app
(define-syntax (app-recorder e)
  (syntax-case e ()
    [(_ fun-expr arg-expr ...)
     #`(let ([fun fun-expr]
             [args (list arg-expr ...)])    
         (apply-recorder fun args #,e fun-expr))]))

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
    [(_ world (name fun other ...) ...)
     #`(begin 
         (define current-big-bang-node
           (create-node 'big-bang #f empty
                        #,(syntax-position e)
                        #,(syntax-span e)
                        0 0))
         (add-kid top-big-bang-node current-big-bang-node)
         (universe:big-bang
          world
          #,@(map (lambda (f n o)
                    #`[#,n (let ([f-value #,f])
                             (lambda args
                               (let* ([node (create-node '#,n #f args
                                                         #,(syntax-position f) #,(syntax-span f) 0 0)]
                                      [result (parameterize ([current-call node])
                                                (with-handlers ([identity exn-wrapper])
                                                  (apply f-value args)))])
                                 (set-node-result! node result)
                                 #,@(if (equal? (syntax->datum n) 'to-draw)
                                        #'((set-node-title! node result)
                                           (set-node-has-title! node #t))
                                        #'())
                                 (add-kid current-big-bang-node node)
                                 (if (exn-wrapper? result)
                                     (error "Error")
                                     result))))
                           #,@o])
                  
                  (syntax-e #'(fun ...))
                  (syntax-e #'(name ...))
                  (syntax-e #'((other ...) ...)))))]))

;a macro that, when called, defines a macro to replace a check-* form
;takes the name that the new macro should be called, the check-* form it is supposed to replace
;a function to determine if the check-* passed (it will recieve the arguments in order)
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
         (let* ([datum (syntax-e #'actual-stx)]
                [func-stx (if (pair? datum)
                              (car datum)
                              datum)]
                [args-stx (when (pair? datum)
                            (cdr datum))]
                [expected-datums (syntax-e #'expected-stxs)]
                [node-names 'node-names-stx])
           #`(begin 
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
                                   (cdr node-names))))))]))))

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

;Code to run after users program has run
;If nothing to trace, message to user
;If code to trace, generates and displays page
(define (after-body name offset errored src)
  (display-results)
  ;If empty trace generate error message
  (if (and (empty? (node-kids top-node))
           (empty? (node-kids top-ce-node))
           (empty? (node-kids top-big-bang-node)))
      (message-box "Error" 
                   "There is nothing to trace in this file. Did you define any functions in this file? Are they called from this file?" 
                   #f 
                   '(ok stop))
      (send-url/contents (page name offset errored src))))

;adds after-body to the end, and deals with the extra information provided by the reader
(define-syntax (#%module-begin stx)
  (syntax-case stx ()
    [(_ name source offset body ...)
     #`(#%plain-module-begin
        ;Set exception handler to allow tracing of functions that error out
        (uncaught-exception-handler (lambda (x)
                                      (displayln (exn-message x))
                                      (after-body name offset #t source)
                                      ((error-escape-handler))))
        body ...
        (run-tests)
        (after-body name offset #f source))]))