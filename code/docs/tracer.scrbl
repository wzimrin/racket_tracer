#lang scribble/manual
@(require scribble/eval)
@(require (for-label racket))

@title{Tracer}

The Tracer creates a graphical representation of a program as the evaluation tree. It is a tool for debugging code, as well as a teaching aid. By looking at the output at each level, it is possible to quickly narrow down what function is generating incorrect input. Currently, the tracer lang uses the semantics of intermediate-student-with-lambda.  

@section{Getting Started}

To use the Tracer, set your language to #lang planet tracer/tracer. When you run your code, your preferred browser (as selected in Dr Racket under Edit, Preferences, Browser) will open up with the trace. 

The tab bar along the top displays the top level function calls. The body of the page shows the trace for a single top level function call. 

Each box in the tracer corresponds to a call of a user-defined function.  A call is a child of another call if the child call appeared within the body of the parent function.

Consider the code below:

@racketblock[(define (alpha a) (+ a a))
             (define (beta b) (* b 2))
             (define (gamma c) (+ (foo c) (bar c)))
             (define (delta d) (+ (foo (bar d))))
             (gamma 3)
             (delta 3)]

The trace of this code would generate two top level calls - gamma and delta. Both of this calls would have foo and bar as children at the same level. 

Clicking on the function name will highlight the corresponding call in the code browser on the right of the screen.  If an argument is too large, it will be shortened and clicking on the shortened argument will expand (or collapse) it.

Suggested browsers: Firefox, Chrome, or Safari.  Other browsers may work, but we don't test them.
                             




