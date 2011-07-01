#lang scribble/manual
@(require scribble/eval)
@(require (for-label racket))

@title{Tracer}

The Tracer provides an interface for exploring the execution of a program. It is a tool for debugging code, as well as a teaching aid. By looking at the output at each level, it is possible to quickly narrow down what function is generating incorrect input. Currently, the tracer lang uses the semantics of intermediate-student-with-lambda.  

@section{Getting Started}

To use the Tracer, set your language to @litchar{#lang planet tracer/tracer}. When you run your code, your preferred browser (as selected in Dr Racket under Edit, Preferences, Browser) will open up with the trace. 

@section{Understanding The Display}

@subsection{The Tab Bar}
The tab bar along the top displays the top level function calls. The body of the page shows the trace for a single top level function call. 

@subsection{The Trace}

Each box in the tracer corresponds to a call of a user-defined function.  A call is a child of another call if the child call appeared within the body of the parent function.

Consider the code below:

@codeblock|{
#lang planet tracer/tracer
(define (alpha a) (+ a a))
(define (beta b) (* b 2))
(define (gamma c) (+ (alpha c) (beta c)))
(define (delta d) (+ (alpha (beta d))))
(gamma 3)
(delta 3)}|

The trace of this code would generate two top level calls - @racket[gamma] and @racket[delta]. Both of this calls would have alpha and beta as children at the same level. 

Clicking on the function name will highlight the corresponding call to that function in the code browser on the right of the screen. Clicking on the expand or collapse button will display or hide the child calls for that function. To view the definition of a function, click on the jump to definition button located to the right of the expand/collapse button. If an argument is displayed in a shortened form (includes ...), clicking on it will expand it. Clicking again will collapse it back to it's original size. 

Once the trace is too large to display on the screen, you can move the trace by clicking and dragging, or by using the scroll bars. 

@subsection{The Code}

The right side of the display shows the source code of the file that was traced. To expand click anywhere along the expand bar on the top, or double click anywhere on the code itself. To collapse, the same applies. Clicking on a function name or jump to definition will automatically open the code pane. Clicking again on the same function will hide the code pane. 

@section{Compatibility}
Suggested browsers: Firefox, Chrome, or Safari.  Other browsers may work, but we don't test them. @litchar{#lang planet tracer/tracer} currently supports images generated in the code: @codeblock{(define r (circle 25 "solid" "red"))} However, we don't currently support images embedded in the code. 

                             




