#lang racket

;task 1
(define (sum-counts-iter x d)

  ;finds the count of the d-s in the number num (num > 0)
  (define (count-single-num num d)
    (cond [(= 0 num)                0] ;end of recursion
          [(= d (remainder num 10)) (+ 1 (count-single-num (quotient num 10) d))]
          [else                     (count-single-num (quotient num 10) d)]))

  (define (sum-digits num)
    (if (< num 10)
        num
        (+ (remainder num 10) (sum-digits (quotient num 10)))))
  
  (define (countD-s curNum curCount)
    (if (> curNum x)
        curCount
        (countD-s (+ 1 curNum) (+ curCount (count-single-num curNum d)))))

  (sum-digits (countD-s 1 0)))
  
        
(sum-counts-iter 1 1) ; -> 1
(sum-counts-iter 5123 1) ; -> 19
(sum-counts-iter 1234 8) ; -> 10
(sum-counts-iter 5555 5) ; -> 10
(sum-counts-iter 65432 6) ; -> 11
(sum-counts-iter 70000 1) ; -> 11
(sum-counts-iter 123321 1) ; -> 29

;task 2
(define (add-ones n)

  ;we construct the result using the method:                  10^0*x_0   +  10^1*x_1  + ... + 10^k*x_k
  ;x_0...x_k are the digits of the result in reverse order: /last digit/                   /first digit/
  ;if we have original digit 9 corresponding to degree m, we add 10^m*0 + 10^(m+1)*1 = 10^(m+1)
  (define (helper num curDeg)
    (cond [(= num 0)                0] ;end of recursion
          [(= 9 (remainder num 10)) (+ (expt 10 (+ curDeg 1)) (helper (quotient num 10) (+ curDeg 2)))]
          [else                     (+ (* (+ (remainder num 10) 1) (expt 10 curDeg)) (helper (quotient num 10) (+ curDeg 1)))]))

  (if (= n 0)
      1
      (helper n 0)))


(add-ones 0)   ; -> 1
(add-ones 123) ; -> 234
(add-ones 193) ; -> 2104
(add-ones 998) ; -> 10109
(add-ones 9999) ; -> 10101010