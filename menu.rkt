#lang racket
; list[pair[string, any/c]] -> bang-thing
; The menu takes in a list of key/value pairs. The key is displayed in
; the menu, and selecting a menu item means causing the value for that
; key to be the final state of the menu.

(require 2htdp/image)
(require 2htdp/universe)

(define rem 30)
; number -> integer
(define make-font-size round) 

(define (draw-menu-item item)
  (text item rem 'black))

(define cursor
  (beside (rectangle (make-font-size (* 3/2 rem)) 
                     rem 'solid 'black)
          (rotate -90
                  (triangle rem 'solid 'black))))

(define (draw-menu title items selected-item)
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

;(define items (list "one" "two" "three"))
(define vals (build-list 5 identity))
(define items (map number->string vals))

(define-struct menu [vals items])
; Takes in index of currently selected item, returns index of next
; item.
(define (menu-next menu index)
  (modulo (add1 index) (length (menu-items menu))))
(define (menu-prev menu index)
  (modulo (sub1 index) (length (menu-items menu))))
(define (menu-value menu index)
  (second (list-ref (menu-vals menu) index)))
(define tower-menu (make-menu vals items))

(big-bang
  0
  [to-draw 
    (lambda (state)
      (draw-menu "Select Tower Size:" 
                 (menu-items tower-menu)
                 state))]
  [on-key
    (lambda (state key)
      (let [(return "\r")]
        (case key
          ; High parts of a menu are earlier in list
          ; Low parts of a menu are later in list
          [("down" "j") (menu-next tower-menu state)]
          [("up" "k") (menu-prev tower-menu state)]
          [("q" return) #f]
          [else state])))]
  [stop-when boolean?])
  
