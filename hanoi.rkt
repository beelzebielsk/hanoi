#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require "component-state.rkt")
(require "menu.rkt")
(require "towers.rkt")


(define-struct control-state [component running initial-tower-info])
(define tower-heights (build-list 4 (curry + 3)))
(displayln tower-heights)
(define menu-items (map number->string tower-heights))
(define (make-hanoi-game)
  (define menu-game 
    (make-menu-game "Select Tower Size:" 
                    tower-heights
                    menu-items))
  (define (initial-state->state current-game-state)
    (make-state (make-control-state 'menu #t #f)
                ((menu-game 'initial-state->state) #f)))
  (define (get-current-component state)
    (control-state-component (state-private state)))
  (define (to-draw state)
    (case (get-current-component state)
      [(menu) ((menu-game 'to-draw) (state-public state))]
      [(towers)
       (let* [(height (control-state-initial-tower-info (state-private state)))
              (towers-game (make-towers-game height))]
         ((towers-game 'to-draw) (state-public state)))]
      [else empty-image]))
  (define (on-key state key)
    (let* [(menu-to-tower 
             (lambda (state)
               (let [(tower-height ((menu-game 'final-state) 
                                    (state-public state)))]
                 (displayln (format "Chosen tower height: ~a" tower-height))
                 (make-state
                   (make-control-state 'towers #t tower-height)
                   (((make-towers-game tower-height) 'initial-state->state) #f)))))
           (tower-to-finish
             (lambda (state)
               (make-state
                 (make-control-state 'towers #f #f)
                 #f)))
           (make-hanoi-state 
             (lambda (public-state-of-game)
               (set-state-public
                 state
                 public-state-of-game)))]
    (case (get-current-component state)
      [(menu) 
       (let [(menu-state ((menu-game 'on-key) (state-public state) key))]
         (if ((menu-game 'stop-when) menu-state)
           (menu-to-tower (make-hanoi-state menu-state))
           (make-hanoi-state menu-state)))]
      [(towers)
       (let* [(height (control-state-initial-tower-info (state-private state)))
              (towers-game (make-towers-game height))
              (towers-state ((towers-game 'on-key) (state-public state) key))]
         (if ((towers-game 'stop-when) towers-state)
           (tower-to-finish (make-hanoi-state towers-state))
           (make-hanoi-state towers-state)))]
      [else (error (format "Current game is not recognized: '~a'" 
                           (get-current-component state)))])))
  (define stop-when (compose1 (curry eq? #f) control-state-running state-private))
  (lambda (dispatch)
    (case dispatch
      [(to-draw) to-draw]
      [(on-key) on-key]
      [(stop-when) stop-when]
      [(initial-state->state) initial-state->state])))

(define game (make-hanoi-game))
(big-bang
  ((game 'initial-state->state) #f)
  [to-draw (game 'to-draw)]
  [on-key (game 'on-key) ]
  [stop-when (game 'stop-when)])
