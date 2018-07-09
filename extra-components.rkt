#lang racket
(provide make-still make-still-screen)
(require 2htdp/image)
(require 2htdp/universe)

(define (make-still image seconds)
  (define (to-draw state) image)
  (define on-tick add1)
  (define (seconds->ticks seconds) (* seconds 28))
  (define initial-state 0)
  (define (stop-when state) (> state (seconds->ticks seconds)))
  (define (output state) #f)
  (lambda (dispatch)
    (case dispatch
      [(to-draw) to-draw]
      [(on-tick) on-tick]
      [(stop-when) stop-when]
      [(output) output]
      [(initial-state) initial-state])))

(define (make-still-screen message font-size)
  (define (to-draw state)
    (text message font-size 'black))
  (define (on-key state key) #t)
  (define stop-when (λ (state) (and (boolean? state) state)))
  (define (output state) (void))
  (define initial-state #f)
  (lambda (dispatch)
    (case dispatch
      [(to-draw) to-draw]
      [(on-key) on-key]
      [(stop-when) stop-when]
      [(output) output]
      [(initial-state) initial-state])))
