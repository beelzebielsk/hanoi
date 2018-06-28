#lang racket
(provide compose-components make-final-state-adaptor)

(require 2htdp/image)
(require 2htdp/universe)
(require "component-state.rkt")

(module+ test
  (require "menu.rkt")
  (require "towers.rkt")
  (define tower-heights (build-list 4 (curry + 3)))
  (define menu-items (map number->string tower-heights)))

(define-struct control-state [components-left component])

; A component is a function that takes a single argument: some sort of
; initializer. Each component is free to describe what the form of
; this initializer is, and this should be public knowledge; it's part
; of that component's interface.
;
; So, a component is a procedure. The contract of a procedure, which
; the procedure must adhere to is:
; The information that it takes in as an initializer, and the
; information that it "returns" as it's final state. Everything else
; should be considered private to the component.
;
; The result of (component initializer) is a closure which exposes, at
; a minimum, the following names with the following values:
; - to-draw : procedure?. Same specification as the to-draw procedure
;   for big-bang: state? -> image?
; - on-key : procedure?. Same specification as the on-key procedure
;   for big-bang: state? string -> state?
; - stop-when: procedure?. Same specification as the stop-when
;   procedure for big-bang: state? -> boolean?
; - final-state: procedure?. This has nothing to do with big-bang.
;   The contract of this procedure is state? -> any/c.
; - initial-state: state?. This procedure steps up the initial state
;   of the component, for when the component is 1st run. This way, a
;   user does not have to know anything about the structure of the
;   state of a component, while the component is running. This should
;   be considered private to the component, subject to change at any
;   time.
;
; The result of compose-components is, itself, a component. The
; initializer of compose-components is the initializer of the 1st
; composed component, and the final state of compose-components is
; the final state of the last composed component.
;
; Examples:
; - (compose-components 'test comp1): The initializer and final state
;   of this component is the initializer and final state of comp1.
; - (compose-components 'test comp1 comp2 comp3): The initializer and
;   final state of this component is the initlaizer of comp1 and the
;   final state of comp3.
(define (compose-components name . components)
  (lambda (initializer)
    (define screen-width 1000)
    (define screen-height 500)
    ; base-scene is a quick hack to deal with big-bang not it's output
    ; images. All images of big-bang are placed within a scene that is
    ; the same size as the first picture big-bang shows.
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
      (let* [(games-left 
               (control-state-components-left (state-private state)))
             (current-game (get-current-game state))
             (current-game-state (state-public state))
             (final-state 
               ((current-game 'final-state) current-game-state))]
        (if (null? games-left)
          (make-state #f final-state)
          (let* [(rest-games (cdr games-left))
                 (new-game ((car games-left) final-state))]
            (make-state
              (make-control-state rest-games new-game)
              (new-game 'initial-state))))))
    (define (on-key state key)
      (let* [(current-game-state (state-public state))
             (current-game (get-current-game state))
             (next-game-state ((current-game 'on-key) current-game-state key))]
        (if ((current-game 'stop-when) next-game-state)
          (transition (set-state-public state next-game-state))
          (set-state-public state next-game-state))))
    (define stop-when (compose1 boolean? state-private))
    (define final-state state-public)
    (lambda (dispatch)
      (case dispatch
        [(name) name]
        [(to-draw) to-draw]
        [(on-key) on-key]
        [(final-state) final-state]
        [(stop-when) stop-when]
        [(initial-state) 
         (let [(first-game ((car components) initializer))]
           (make-state
             (make-control-state 
               (cdr components)
               first-game)
             (first-game 'initial-state)))]))))

; component? procedure? -> component?
; This takes in a component and a function and returns a new component.
; The final state of this component is the result of running the final
; state of the original component through the adaptor function.
(define (make-final-state-adaptor component adaptor)
  (lambda (initializer)
    (let [(game (component initializer))]
      (lambda (dispatch)
        (case dispatch
          [(final-state) 
           (lambda (state) (adaptor ((game 'final-state) state)))]
          [else (game dispatch)])))))


; An example of compose components.
(module+ test
  (define game 
    ((compose-components 'hanoi make-menu-game make-towers-game) 
     (list "Choose Tower Size:"
           menu-items
           tower-heights)))
  (big-bang
    (game 'initial-state)
    [to-draw (game 'to-draw)]
    [on-key (game 'on-key) ]
    [stop-when (game 'stop-when)]))


; An example of make-final-state-adaptor
(module+ test
  (define (range start len)
    (if (zero? len)
      null
      (cons start (range (add1 start) (sub1 len)))))
  (define menu-creator
    (make-final-state-adaptor 
      make-menu-game
      (lambda (final-state)
        (list (format "Choose Tower Size from ~a items:" final-state)
              (map number->string (range 3 final-state))
              (range 3 final-state)))))


  (define game1
    ((compose-components 
       'hanoi 
       menu-creator make-menu-game make-towers-game) 
     (list "Choose how many options for next menu:"
           (map number->string (range 1 5))
           (range 1 5))))
  (big-bang
    (game1 'initial-state)
    [to-draw (game1 'to-draw)]
    [on-key (game1 'on-key) ]
    [stop-when (game1 'stop-when)]))
