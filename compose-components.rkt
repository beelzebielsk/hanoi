#lang racket
(provide compose-components make-output-adaptor)

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
; So, a component is a procedure. The contract of a component, which
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
; - output: procedure?. This has nothing to do with big-bang.
;   The contract of this procedure is state? -> any/c. Specifically,
;   this transforms the final state of the component (which likely has
;   details and structure that are private to the component) into a
;   value meant for consumption by other components.
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
(define (compose-components . components)
  (lambda (initializer)
    (define screen-width 1000)
    (define screen-height 500)
    ; base-scene is a quick hack to deal with big-bang not resizing
    ; it's output images. All images of big-bang are placed within a
    ; scene that is the same size as the first picture big-bang shows.
    (define base-scene (empty-scene screen-width screen-height))
    (define (get-current-game state)
      (control-state-component (state-private state)))
    (define (to-draw state)
      (place-image/align 
        (((get-current-game state) 'to-draw) (state-public state))
        (/ screen-width 2) (/ screen-height 2)
        'middle 'middle
        base-scene))
    ; Changes state from one game to the next. Used when the current
    ; component's state would cause the stop-when? function of the
    ; current component to be true.
    ; - If there are no more components left, then the next state should
    ; cause the stop-when? function of the composed component to be
    ; true.
    ; - Otherwise, the composed component should initialize the next
    ;   component, and start using the to-draw, on-key, etc. functions
    ;   of the next component.
    (define (transition state)
      (let* [(games-left 
               (control-state-components-left (state-private state)))
             (current-game (get-current-game state))
             (current-game-state (state-public state))
             (output
               ((current-game 'output) current-game-state))]
        (if (null? games-left)
          (make-state #f output)
          (let* [(rest-games (cdr games-left))
                 (new-game ((car games-left) output))]
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
    ; The on-tick function of the composition behaves as the identity
    ; function if the current component does not expose an on-tick
    ; function, and if the current component does expose an on-tick
    ; function, then that function is used.
    (define on-tick
      (lambda (state)
        (let [(current-game (get-current-game state))]
          (if (void? (current-game 'on-tick))
            state
            (let* [(current-game-state (state-public state))
                   (next-game-state 
                     ((current-game 'on-tick) current-game-state))]
              (if ((current-game 'stop-when) next-game-state)
                (transition (set-state-public state next-game-state))
                (set-state-public state next-game-state)))))))

    (define stop-when (compose1 boolean? state-private))
    (define output state-public)
    (lambda (dispatch)
      (case dispatch
        [(to-draw) to-draw]
        [(on-key) on-key]
        [(on-tick) on-tick]
        [(output) output]
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
(define (make-output-adaptor component adaptor)
  (lambda (initializer)
    (let [(game (component initializer))]
      (lambda (dispatch)
        (case dispatch
          [(output) 
           (lambda (state) (adaptor ((game 'output) state)))]
          [else (game dispatch)])))))


; An example of compose components.
(module+ test
  (define game 
    ((compose-components make-menu-game make-towers-game) 
     (list "Choose Tower Size:"
           menu-items
           tower-heights)))
  (big-bang
    (game 'initial-state)
    [to-draw (game 'to-draw)]
    [on-key (game 'on-key) ]
    [stop-when (game 'stop-when)]))


; An example of make-output-adaptor
(module+ test
  (define (range start len)
    (if (zero? len)
      null
      (cons start (range (add1 start) (sub1 len)))))
  (define menu-creator
    (make-output-adaptor 
      make-menu-game
      (lambda (final-state)
        (list (format "Choose Tower Size from ~a items:" final-state)
              (map number->string (range 3 final-state))
              (range 3 final-state)))))


  (define game1
    ((compose-components 
       menu-creator make-menu-game make-towers-game) 
     (list "Choose how many options for next menu:"
           (map number->string (range 1 5))
           (range 1 5))))
  (big-bang
    (game1 'initial-state)
    [to-draw (game1 'to-draw)]
    [on-key (game1 'on-key) ]
    [stop-when (game1 'stop-when)]))

; An example of on-tick behavior
(module+ test
(define (seconds->ticks seconds) (* seconds 28))
(define (ticks->seconds ticks) (/ ticks 28))
(define (draw-box x-pos y-pos) 
  (let [(box (rectangle 30 10 'solid 'black))
        (scene (rectangle 400 300 'solid 'white))]
    (place-image box x-pos y-pos scene)))
(define (draw ticks) (draw-box ticks ticks))
(define (silly-box initializer)
  (lambda (dispatch)
    (case dispatch
      [(to-draw) draw]
      [(on-tick) add1]
      [(initial-state) 0]
      [(output) identity]
      [(stop-when) (lambda (state) (>= (ticks->seconds state) 1))])))

  (define game3
    ((compose-components 
       (make-output-adaptor 
         silly-box 
         (lambda (output)
           (list "Choose Tower Size:"
                 menu-items
                 tower-heights)))
       make-menu-game make-towers-game) (void)))
  (big-bang
    (game3 'initial-state)
    [to-draw (game 'to-draw)]
    [on-key (game 'on-key)]
    [on-tick (game 'on-tick)]
    [stop-when (game 'stop-when)]))
