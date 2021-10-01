#lang info
(define collection "pareto-frontier")
(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/pareto-frontier.scrbl" ())))
(define pkg-desc "Efficient Pareto frontier calculation")
(define version "0.0")
(define pkg-authors '(sorawee))
(define license '(Apache-2.0 OR MIT))
