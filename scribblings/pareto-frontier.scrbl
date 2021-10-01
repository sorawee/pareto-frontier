#lang scribble/manual
@require[scribble/example
         @for-label[pareto-frontier
                    racket/base
                    racket/list
                    racket/contract]]

@(define evaluator (make-base-eval))
@(evaluator '(require pareto-frontier
                      racket/list))

@title{pareto-frontier}
@author[@author+email["Sorawee Porncharoenwase" "sorawee.pwase@gmail.com"]]

@defmodule[pareto-frontier]

This library provides functions to compute the Pareto frontier set.

@defproc[(pareto-frontier-3 [xs list?]
                            [first-key (-> any/c any/c)]
                            [second-key (-> any/c any/c)]
                            [third-key (-> any/c any/c)])
         list?]{
  Computes the three dimensional Pareto frontier of @racket[xs]
  according to three objectives: @racket[first-key], @racket[second-key], and @racket[third-key].
  In our formulation, if @racket[a] is less then @racket[b], then @racket[a] dominates @racket[b].

  @examples[#:eval evaluator
    (pareto-frontier-3
     (list (list 1 9 3) (list 2 9 4) (list 10 8 10))
     first second third)
  ]
}
