#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require "component-state.rkt")
(require "menu.rkt")
(require "towers.rkt")


(define-struct control-state [component running])
(define tower-heights (build-list 4 (curry + 3)))
(displayln tower-heights)
(define menu-items (map number->string tower-heights))
(define (make-hanoi-game)
  (define initial-state
    (let [(menu (make-menu-game "Select Tower Size:" 
                    tower-heights
                    menu-items))]
    (make-state 
      (make-control-state menu #t)
      ((menu 'initial-state->state) #f))))
  (define base-scene (empty-scene 1000 500))
  (define (get-current-game state)
    (control-state-component (state-private state)))
  (define (to-draw state)
    (place-image/align 
      (((get-current-game state) 'to-draw) (state-public state))
      500 250
      'middle 'middle
      base-scene))
  (define (on-key state key)
    (let* [(transition 
             (lambda (state)
               (case ((get-current-game state) 'name)
                 [(menu)
                  (let* [(current-game-state (state-public state))
                         (final-state 
                           (((get-current-game state) 'final-state) current-game-state))
                         (towers-game (make-towers-game final-state))]
                    (make-state
                      (make-control-state towers-game #t)
                      ((towers-game 'initial-state->state) #f)))]
                 [(towers) 
                  (make-state
                    (make-control-state #f #f)
                    #f)])))
           (make-hanoi-state 
             (lambda (public-state-of-game)
               (set-state-public
                 state
                 public-state-of-game)))
           (current-game (get-current-game state))
           (next-game-state ((current-game 'on-key) (state-public state) key))]
      (if ((current-game 'stop-when) next-game-state)
        (transition (set-state-public state next-game-state))
        (set-state-public state next-game-state))))
  (define stop-when (compose1 (curry eq? #f) control-state-running state-private))
  (lambda (dispatch)
    (case dispatch
      [(name) 'hanoi]
      [(to-draw) to-draw]
      [(on-key) on-key]
      [(stop-when) stop-when]
      [(initial-state->state) (lambda (state) initial-state)])))

(define game (make-hanoi-game))
(big-bang
  ((game 'initial-state->state) #f)
  [to-draw (game 'to-draw)]
  [on-key (game 'on-key) ]
  [stop-when (game 'stop-when)])
