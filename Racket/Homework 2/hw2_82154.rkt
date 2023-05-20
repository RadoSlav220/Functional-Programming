#lang racket

;deletes first occurance of an element in a list
(define (delete-first-occ lst x)
  (cond [(empty? lst)           null]
        [(equal? (first lst) x) (rest lst)]
        [else (cons (first lst) (delete-first-occ (rest lst) x))]))

;-----------------------------------------------------------------------------------------;
;task 1 - version 1
(define (itinerary1 flights)
  (λ (start)
    ;finds the best route, giving priority to the lexicographic order, not to the possibility of finding a route
    (define (find-best-route curAirport bestNext curRoute moreToGo toTry)
      (cond [(empty? moreToGo)  (reverse curRoute)]
            [(empty? toTry)     (if (empty? bestNext)
                                    null
                                    (find-best-route (cdr bestNext)
                                                     null
                                                     (cons bestNext curRoute)
                                                     (delete-first-occ moreToGo bestNext)
                                                     (delete-first-occ moreToGo bestNext)))]
            [(and (equal? curAirport (car (first toTry)))
                  (or (empty? bestNext) (string>? (cdr bestNext) (cdr (first toTry)))))
             (find-best-route curAirport (first toTry) curRoute moreToGo (rest toTry))]

            [else (find-best-route curAirport bestNext curRoute moreToGo (rest toTry))]))

    (define route (find-best-route start null null flights flights))

    (define (convert-to-final-answer lst)
      (if (empty? lst)
          lst
          (cons (cdr (first lst)) (convert-to-final-answer (rest lst)))))

    (if (empty? route)
        "No such itinerary"
        (cons start (convert-to-final-answer route)))))


;task 1 - version 2
(define (itinerary2 flights)
  (λ (start)
    
    ;finds the flights which can be executed from curAirport
    (define (possible-flights curAirport allflights)
      (filter (λ (x) (equal? (car x) curAirport)) allflights))
  
    ;checks if route1 is better than route2
    (define (isbetter? route1 route2)
      (cond [(empty? route1)                                      #f]
            [(empty? route2)                                      #t]
            [(string<? (cdr (first route1)) (cdr (first route2))) #t]
            [(string>? (cdr (first route1)) (cdr (first route2))) #f]
            [else                                                 (isbetter? (rest route1) (rest route2))]))

    ;finds best route, giving priority in the following way:
    ;1) finding a possible route
    ;2) finding the smallest lexicographic route
    (define (find-best curDest curRoute moreToGo possib)
      (cond [(empty? moreToGo)      (reverse curRoute)]
            [(empty? possib)        null]
            [(let ([res1 (find-best
                          (cdr (first possib))
                          (cons (first possib) curRoute)
                          (delete-first-occ moreToGo (first possib))
                          (possible-flights (cdr (first possib)) (delete-first-occ moreToGo (first possib))))]
                   [res2 (find-best curDest curRoute moreToGo (rest possib))])
               (if (isbetter? res1 res2)
                   res1
                   res2))]))

    (define bestOption (find-best start null flights (possible-flights start flights)))

    (define (convert-to-final-answer lst)
      (if (empty? lst)
          lst
          (cons (cdr (first lst)) (convert-to-final-answer (rest lst)))))
    
    (if (empty? bestOption)
        "No such itinerary!"
        (cons start (convert-to-final-answer bestOption)))))

;;tests
(define lst1 '(("SFO" . "HKO") ("YYZ" . "SFO") ("YUL" . "YYZ") ("HKO" . "ORD")))
(define lst2 '( ("B" . "C") ("A" . "C") ("C" . "A") ("A" . "B")))
(define lst3 '(("SFO" . "COM") ("COM" . "YYZ")))
(define lst4 '(("A" . "C") ("C" . "A") ("A" . "B")))
(define lst5 '(("G" . "E") ("E" . "F") ("A" . "G") ("D" . "E") ("B" . "C") ("E" . "C") ("A" . "B") ("C" . "D") ("F" . "A") ("C" . "A")))

((itinerary1 lst1) "YUL")
((itinerary1 lst2) "A")
((itinerary1 lst3) "COM")
((itinerary1 lst4) "A")
((itinerary1 lst5) "A")

;-----------------------------------------------------------------------------------------;
;task 2
(define (pad xs)
  (λ (x)
    ;creates a row consisted only of the value of num
    (define (create-mono-row total num)
      (if (= total 0)
          null
          (cons num (create-mono-row (- total 1) num))))

    (define mono-row (create-mono-row (+ 2 (length (first xs))) x))
    
    (define (modify-row row)
      (cons x (append row (list x))))
    
(append (cons mono-row (map modify-row xs)) (list mono-row))))
          
;;tests
((pad '( (1 2 3)
         (4 5 6)
         (7 8 9) )
) 0)

((pad '( (1 2 3 2 1 2 4)
         (4 5 6 0 3 4 2)
         (7 8 9 8 2 3 2)
         (7 8 9 8 2 3 2)
         (7 8 9 8 2 3 2))
) 9)           