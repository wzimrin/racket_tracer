#lang planet tracer/tracer

;takes a string and splits it into words, separated by one or more spaces
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

#|
takes an object (generally a string) and an a-list (a list of lists, with the first element 
of the sublist being a key and the second element of the sublist being its value) (basically, 
a low tech map), and increments the value corresponding to the object
|#
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

;counts how many times a word appears in a string
(define (count-words string)
  (local [(define (iter words store)
            (if (empty? words)
                store
                (iter (rest words) (add (first words) store))))]
    (iter (split string) empty)))

(count-words "hello goodbye ku ku kuchu   diamonds ku goodbye ")
(count-words "one two three three four five  one one one")