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

The trace of this code would generate two top level calls: @racket[gamma] and @racket[delta]. Both of this calls would have alpha and beta as children at the same level. 

Once the trace is too large to display on the screen, you can move the trace by clicking and dragging, or by using the scroll bars. 

@subsection{A Call}

The top portion of a call shows its function name and the arguments passed to it.  Clicking on the function name will highlight the source of the function call in the code.  Underneath the call, there are up to three buttons.  The magnifying glass button highlights the function definition.  The up or down arrowhead button shows or hides the children of the call.  The checkbox button relates to check expects, and is discussed in @(secref "check-expect").

@subsection{The Code}

The right side of the display shows the source code of the file that was traced. Click anywhere along the bar above the code or on the code itself to expand or shrink the code. Clicking on a function name or the magnifying glass automatically expands the code pane. Clicking again on the same function name will shrink the code pane. 

@(subsection "Check Expects" #:tag "check-expect")

If a call in the trace corresponds to a check-expect (the function and the arguments passed to the function are both the same), the call is colored.  If the check-expect passed, the call is green, while if the check-expect failed, the call is red.  The checkbox button appears as well, which will highlight the check-expect that corresponds to the call.

If there are any failed check-expects, a new check-expect tab appears in the tab bar.  Clicking on this tab shows a menu on the left with the failed check-expects.  Clicking on a check-expect brings up the trace of the check-expect, letting you look at how the actual value and the expected value were computed.

@section{Compatibility}
Suggested browsers: Firefox, Chrome, or Safari.  Other browsers may work, but we don't test them. @litchar{#lang planet tracer/tracer} currently supports images generated in the code or embedded in the code.

                             




