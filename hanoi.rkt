#lang racket

(provide (rename-out [make-game make-hanoi-game]))
(require 2htdp/image)
(require 2htdp/universe)
(require "component-state.rkt")
(require "menu.rkt")
(require "towers.rkt")
(require "compose-components.rkt")
(require "extra-components.rkt")

(define (finish-screen ignored)
  (make-still-screen "Thanks for Playing!" 40))

(define (make-start-screen num-options)
  (displayln num-options)
  (let* [(min-tower-height 3)
         (tower-heights 
           (build-list num-options (curry + min-tower-height)))
         (menu-items (map number->string tower-heights))]
    (make-menu-game 
      (list "Choose Tower Size:"
            menu-items
            tower-heights))))

; Initializer: positive?
; Defines the number of options to choose from, where the options are
; the height of the tower to move. The smallest option is always a
; tower of height 3.
; output: the value #f, always
(define make-game 
  (compose-components make-start-screen 
                      make-towers-game 
                      finish-screen))

(define game (make-game 4))

(module+ test
  (big-bang
    (game 'initial-state)
    [to-draw (game 'to-draw)]
    [on-key (game 'on-key) ]
    [stop-when (game 'stop-when)]))
