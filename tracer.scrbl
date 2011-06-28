#lang scribble/manual
@(require scribble/eval)
@(require (for-label racket))

@title{Tracer}

The Tracer creates a graphical representation of a program as the evaluation tree. It is a tool for debugging code, as well as a teaching aid. By looking at the output at each level, it is possible to quickly narrow down what function is generating incorrect input. Currently, the tracer lang uses the semantics of intermediate-student-with-lambda.  

@section{Getting Started}

To use the Tracer, set your language to #lang planet tracer/tracer. When you run your code, your preferred browser (as selected in Dr Racket under Edit, Preferences, Browser) will open up with the trace. 

The tab bar along the top displays the top level function calls. The body of the page shows the trace for a single top level function call. Consider the code below:

@racketblock[(define (alpha a) (+ a a))
             (define (beta b) (* b 2))
             (define (gamma c) (+ (alpha c) (beta c)))
             (define (delta d) (+ (alpha (beta d))))
             (gamma 3)
             (delta 3)]

The trace of this code would generate two top level calls - gamma and delta. Both of this calls would have alpha and beta as children at the same level. 


                             




