#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require "component-state.rkt")
(require "menu.rkt")
(require "towers.rkt")


(define-struct control-state [components-left component])
(define tower-heights (build-list 4 (curry + 3)))
(displayln tower-heights)
(define menu-items (map number->string tower-heights))
(define initial-state->menu-game (curry apply make-menu-game))

; initializer is initial value to kick off the 1st component.
(define (compose-components components initializer name)
  (define screen-width 1000)
  (define screen-height 500)
  (define base-scene (empty-scene screen-width screen-height))
  (define (get-current-game state)
    (control-state-component (state-private state)))
  (define (to-draw state)
    (place-image/align 
      (((get-current-game state) 'to-draw) (state-public state))
      (/ screen-width 2) (/ screen-height 2)
      'middle 'middle
      base-scene))
  (define (transition state)
    (let* [(games-left (control-state-components-left (state-private state)))
           (current-game (get-current-game state))]
      (if (null? games-left)
        #f
        (let* [(current-game-state (state-public state))
               (final-state 
                 ((current-game 'final-state) current-game-state))
               (rest-games (cdr games-left))
               (new-game ((car games-left) final-state))]
          (make-state
            (make-control-state rest-games new-game)
            ((new-game 'initial-state->state) #f))))))
  (define (on-key state key)
    (let* [(current-game-state (state-public state))
           (current-game (get-current-game state))
           (next-game-state ((current-game 'on-key) current-game-state key))]
      (if ((current-game 'stop-when) next-game-state)
        (transition (set-state-public state next-game-state))
        (set-state-public state next-game-state))))
  (define stop-when boolean?)
  (lambda (dispatch)
    (case dispatch
      [(name) name]
      [(to-draw) to-draw]
      [(on-key) on-key]
      [(stop-when) stop-when]
      [(initial-state->state) 
       (lambda (state)
         (let [(first-game ((car components) initializer))]
           (make-state
             (make-control-state 
               (cdr components)
               first-game)
             ((first-game 'initial-state->state) #f))))])))


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
