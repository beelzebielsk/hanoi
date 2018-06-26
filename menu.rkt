#lang racket
; list[pair[string, any/c]] -> bang-thing
; The menu takes in a list of key/value pairs. The key is displayed in
; the menu, and selecting a menu item means causing the value for that
; key to be the final state of the menu.

(provide
  ;menu? menu-title menu-vals menu-items
  ;menu-next menu-prev menu-ref
  ;draw-menu
  make-menu-game
  )
(require 2htdp/image)
(require 2htdp/universe)
(require "component-state.rkt")

(define rem 30)
; number -> integer
(define make-font-size round) 

(define (draw-menu-item item)
  (text item rem 'black))

(module+ test
  (draw-menu-item "item"))

(define cursor
  (beside (rectangle (make-font-size (* 3/2 rem)) 
                     rem 'solid 'black)
          (rotate -90
                  (triangle rem 'solid 'black))))

(module+ test 
  cursor
  (beside cursor (draw-menu-item "item")))


(define (draw-menu-items title items selected-item)
  (define (draw-iter items-left menu-so-far distance-to-selected)
    (if (null? items-left)
      menu-so-far
      (if (zero? distance-to-selected)
        (draw-iter (cdr items-left)
                   (above menu-so-far
                          (beside cursor 
                                  (draw-menu-item (car items-left))))
                   (sub1 distance-to-selected))
        (draw-iter (cdr items-left)
                   (above menu-so-far
                          (draw-menu-item (car items-left)))
                   (sub1 distance-to-selected)))))
  (let [(initial-menu (text title (make-font-size (* 3/2 rem)) 'black))]
    (draw-iter items initial-menu selected-item)))

(define-struct menu [title vals items])
; Takes in index of currently selected item, returns index of next
; item.
(define (menu-next menu index)
  (modulo (add1 index) (length (menu-items menu))))
(define (menu-prev menu index)
  (modulo (sub1 index) (length (menu-items menu))))
(define (menu-ref menu index)
  (list-ref (menu-vals menu) index))

(define (draw-menu menu selected-item)
  (draw-menu-items (menu-title menu) 
                   (menu-items menu)
                   selected-item))

(define (make-menu-game title vals items)
  (define game-menu (make-menu title vals items))
  (define (initial-state->state initial)
    (make-state #t 0))
  (define (to-draw state)
    (draw-menu game-menu (state-public state)))
  (define (on-key state key)
    (let [(return "\r") (menu-entry (state-public state))]
      (case key
        ; High parts of a menu are earlier in list
        ; Low parts of a menu are later in list
        [("down" "j") 
         (set-state-public state (menu-next game-menu menu-entry))]
        [("up" "k")
         (set-state-public state (menu-prev game-menu menu-entry))]
        [("q" return "\n" "\r\n" "\n\r") 
         (set-state-private state #f)]
        [else state])))
  (define (final-state state)
    (let [(index (state-public state))]
      (menu-ref game-menu index)))
  (define stop-when (compose1 (curry eq? #f) state-private))
  (lambda (dispatch)
    (case dispatch
      [(to-draw) to-draw]
      [(on-key) on-key]
      [(stop-when) stop-when]
      [(final-state) final-state]
      [(initial-state->state) initial-state->state])))
(module+ test
  (define vals (build-list 5 identity))
  (define items (map number->string vals))
  (define game (make-menu-game "Select Tower Size:" vals items))

  (big-bang
    ((game 'initial-state->state) #f)
    [to-draw (game 'to-draw)]
    [on-key (game 'on-key) ]
    [stop-when (game 'stop-when)]))
  
