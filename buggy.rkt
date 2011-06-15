#lang s-exp "tracer.rkt"

(define (split str)
  (local [(define (iter remaining current-word found-words)
            (cond
              [(empty? remaining)
               (reverse
                (if (empty? current-word)
                    found-words
                    (cons (list->string (reverse current-word)) found-words)))]
              [(cons? remaining)
               (if (char-whitespace? (first remaining))
                   (if (empty? current-word)
                       (iter (rest remaining) current-word found-words)
                             (iter (rest remaining)
                                   empty
                                   (cons (list->string (reverse current-word))
                                         found-words)))
                   (iter (rest remaining)
                         (cons (first remaining) current-word)
                         found-words))]))]
    (iter (string->list str) empty empty)))

(define (add word store)
  (cond
    [(empty? store)
     (list (list word 1))]
    [(equal? word (first (first store)))
     (cons (list word (add1 (second (first store))))
           (add word (rest store)))]
    [#t
     (cons (first store)
           (add word (rest store)))]))

(define (count-words string)
  (local [(define (iter words store)
            (if (empty? words)
                store
                (iter (rest words) (add (first words) store))))]
    (iter (split string) empty)))

(count-words "hello goodbye ku ku kuchu   diamonds ku goodbye ")
(count-words "one two three three four five  one one one")