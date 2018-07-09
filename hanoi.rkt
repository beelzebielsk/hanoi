#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require "component-state.rkt")
(require "menu.rkt")
(require "towers.rkt")
(require "compose-components.rkt")


(define-struct control-state [components-left component])
(define tower-heights (build-list 4 (curry + 3)))
(displayln tower-heights)
(define menu-items (map number->string tower-heights))

(define (make-still-screen message font-size)
  (define (to-draw state)
    (text message font-size 'black))
  (define (on-key state key) #f)
  (define stop-when boolean?)
  (define (output state) #f)
  (define initial-state #f)
  (lambda (dispatch)
    (case dispatch
      [(to-draw) to-draw]
      [(on-key) on-key]
      [(stop-when) stop-when]
      [(output) output]
      [(initial-state) initial-state])))

(define (finish-screen ignored)
  (make-still-screen "Thanks for Playing!" 40))


(define make-game 
  (compose-components 'hanoi
    (compose-components 'first make-menu-game)
    (compose-components 'rest make-towers-game finish-screen)))

(define game (make-game (list "Choose Tower Size:"
                              menu-items
                              tower-heights)))

(big-bang
  (game 'initial-state)
  [to-draw (game 'to-draw)]
  [on-key (game 'on-key) ]
  [stop-when (game 'stop-when)])
