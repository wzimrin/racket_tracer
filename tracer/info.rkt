#lang setup/infotab
(define name "Tracer")
(define blurb '("Traces the execution of a functional program. To use set lang to #lang planet tracer/tracer, do not use require."))
(define repositories '("4.x"))
(define required-core-version "5.1.1")
(define primary-file "tracer.rkt")
(define categories '(devtools))
(define scribblings '(("docs/tracer.scrbl")))
(define release-notes '((p "Support for CS0190 contracts and signatures added. Some GUI bugs fixed. Changes in default printing settings.")))
(define compile-omit-paths (list "docs"))

