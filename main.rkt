#lang racket/base

(provide pareto-frontier-3)
(require racket/list
         racket/match)

(define (pareto-frontier-3 xs first-key second-key third-key #:inf [inf +inf.0])
  (define (elem< a b)
    (cond
      [(= (third-key a) (third-key b))
       (cond
         [(= (first-key a) (first-key b))
          (< (second-key a) (second-key b))]
         [else (< (first-key a) (first-key b))])]
      [else (< (third-key a) (third-key b))]))

  (define (dominated-first-second? a b)
    (and (>= (first-key a) (first-key b))
         (>= (second-key a) (second-key b))))

  ;; precondition: xs are sorted by third-key, and in a way that if a precedes b
  ;; and a is not equal to b, then b can never dominate a
  (define (naive-3 xs)
    (for/fold ([frontier '()] #:result (sort frontier < #:key first-key))
              ([current (in-list xs)])
      (cond
        [(ormap (λ (front) (dominated-first-second? current front)) frontier) frontier]
        [else (cons current frontier)])))

  (let loop ([xs (sort xs elem<)] [len (length xs)])
    (match xs
      [(list) '()]
      [(list x) (list x)]
      [_ #:when (< len 16) (naive-3 xs)]
      [_
       (define pos (quotient len 2))
       (define-values (front back) (split-at xs pos))
       (define front* (loop front pos))
       (define back* (loop back (- len pos)))
       (let loop ([front front*] [back back*] [acc '()] [lowest inf])
         (match* (front back)
           [((cons f front*) (cons b back*))
            (cond
              [(< (first-key b) (first-key f))
               (cond
                 [(<= lowest (second-key b)) (loop front back* acc lowest)]
                 [else (loop front back* (cons b acc) lowest)])]
              [else (loop front* back (cons f acc) (min lowest (second-key f)))])]
           [('() (cons b back*))
            (cond
              [(<= lowest (second-key b)) (loop front back* acc lowest)]
              [else (loop front back* (cons b acc) lowest)])]
           [(_ '()) (append (reverse acc) front)]))])))

(module+ test
  (require rackunit
           racket/set
           racket/list)
  (check-equal? (list->set (pareto-frontier-3 (list (list 1 5 6)
                                                    (list 1 5 7)
                                                    (list 1 6 5)
                                                    (list 2 4 7))
                                              first
                                              second
                                              third))
                (set (list 1 5 6)
                     (list 1 6 5)
                     (list 2 4 7)))

  (check-equal? (length (pareto-frontier-3 (list (list 1 5 6)
                                                 (list 1 5 7)
                                                 (list 1 6 5)
                                                 (list 1 6 5)
                                                 (list 2 4 7))
                                           first
                                           second
                                           third))
                3)

  (check-equal? (list->set (pareto-frontier-3 (list (list 1 9 9)
                                                    (list 2 1 1))
                                              first
                                              second
                                              third))
                (set (list 1 9 9)
                     (list 2 1 1))))
