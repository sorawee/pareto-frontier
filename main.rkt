#lang racket/base

(provide pareto-3)
(require racket/sequence
         racket/match
         rebellion/base/comparator
         rebellion/base/option
         rebellion/collection/sorted-set)

(define (pareto-3 xs first-key second-key third-key)
  (define xs* (sort xs > #:key first-key))
  (define second-comparator (comparator-map real<=> second-key))
  (define third-comparator (comparator-map real<=> third-key))
  (define the-comparator (comparator-chain second-comparator third-comparator))
  (define *set* (make-mutable-sorted-set #:comparator the-comparator))
  (define *ans* '())

  (define (dominated? x y)
    (and (<= (second-key x) (second-key y))
         (<= (third-key x) (third-key y))))

  (define (proceed! x)
    (let loop ()
      (match (sorted-set-element-at-most *set* x)
        [(present candidate)
         (when (dominated? candidate x)
           (sorted-set-remove! *set* candidate)
           (loop))]
        [(== absent) (void)]))

    (set! *ans* (cons x *ans*))
    (sorted-set-add! *set* x))

  (for ([x (in-list xs*)])
    ;; is x dominated by anyone in *set*?
    (match (sorted-set-element-at-least *set* x)
      [(present candidate)
       (unless (dominated? x candidate)
         ;; x is not dominated by anything in *set*
         (proceed! x))]
      [(== absent) (proceed! x)]))

  *ans*)

(module+ test
  (require rackunit
           racket/set
           racket/list)
  (check-equal? (list->set (pareto-3 (list (list 1 5 6)
                                           (list 1 5 7)
                                           (list 1 6 5)
                                           (list 2 4 7))
                                     (compose1 - first)
                                     (compose1 - second)
                                     (compose1 - third)))
                (set (list 1 5 6)
                     (list 1 6 5)
                     (list 2 4 7)))

  (check-equal? (length (pareto-3 (list (list 1 5 6)
                                        (list 1 5 7)
                                        (list 1 6 5)
                                        (list 1 6 5)
                                        (list 2 4 7))
                                  (compose1 - first)
                                  (compose1 - second)
                                  (compose1 - third)))
                3)

  (check-equal? (list->set (pareto-3 (list (list 1 9 9)
                                           (list 2 1 1))
                                     (compose1 - first)
                                     (compose1 - second)
                                     (compose1 - third)))
                (set (list 1 9 9)
                     (list 2 1 1))))
