#lang scribble/manual
@(require scribble/eval)
@(require (for-label racket))

@title{Tracer}

@section{Getting Started}

To use the Tracer, set your language to #lang planet tracer/tracer. When you run your code, your preferred browser (as selected in Dr Racket under Edit, Preferences, Browser) will open up with the trace. 

The tab bar along the top displays the top level function calls. The body of the page shows the trace for a single top level function call. 

@racketblock[(define (foo a) (+ a a))
         (define (bar b) (* b 2))
         (foo (bar 3))]




