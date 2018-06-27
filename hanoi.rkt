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

(define (make-final-state-adaptor game-creator adaptor)
  (lambda (initializer)
    (let [(game (game-creator initializer))]
      (lambda (dispatch)
        (case dispatch
          [(final-state) 
           (lambda (state) (adaptor ((game 'final-state) state)))]
          [else (game dispatch)])))))

(define game 
  (compose-components (list make-menu-game make-towers-game)
                      (list "Choose Tower Size:"
                            tower-heights
                            menu-items)
                      'hanoi))
(big-bang
  ((game 'initial-state->state) #f)
  [to-draw (game 'to-draw)]
  [on-key (game 'on-key) ]
  [stop-when (game 'stop-when)])
