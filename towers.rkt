#lang racket

; TODO
; - Organize the tower drawing stuff. Perhaps make them an object?

(provide
  ;draw-disk
  ;tower-height tower-top tower-rest
  ;tower-empty? tower-non-empty? tower-can-add?
  ;tower-add
  ;draw-tower draw-towers
  make-towers-game)
(require 2htdp/image)
(require 2htdp/universe)
(require "component-state.rkt")

; TODO
; - Organize the tower drawing stuff. Perhaps make them an object?
; - There's trouble with drawing all of the towers. The images change
;   if I remove the largest disk from all of the towers. Though, to be
;   fair, this means that sizes changes just once per game.
; - Hande trying to remove from an empty tower, or trying to put a
;   disk on a tower that's too large. 

(define disk-height 20)
; A disk is a positive number.
(define (draw-disk disk)
  (overlay (text (number->string disk) (sub1 disk-height) 'white)
           (rectangle (+ 10 (* disk 30)) disk-height 'solid 'black)))

(module+ test
  (draw-disk 0)
  (draw-disk 1)
  (draw-disk 2)
  (draw-disk 10)
  (draw-disk 20))

; A tower is a list of numbers of strictly decreasing size.
; The first element (car tower) is the largest number, and (cdr tower)
; is, itself, a tower.
(define tower-height length)
(define tower-top last)
(define (tower-rest tower)
  (take tower (sub1 (length tower))))
(define tower-bottom car)
(define (tower-add tower disk)
  (append tower (list disk)))
(define tower-empty? null?)
(define tower-non-empty? pair?)
(define (tower-can-add? tower disk)
  (or (tower-empty? tower) (< disk (tower-top tower))))
(define (make-tower-with-bottom-disk disk)
  (reverse (cdr (build-list (+ disk 1) identity))))

; tower? -> image?
(define (draw-tower tower)
  (define (iter starting-point tower scene)
    (if (tower-empty? tower)
      scene
      (let [(disk (draw-disk (car tower)))] 
        (iter (+ starting-point disk-height)
              (cdr tower)
              (underlay/align/offset
                'middle 'bottom
                scene
                0
                (- starting-point)
                disk)))))
  (iter 0
        tower
        empty-image))

(module+ test
  (define initial-tower (reverse (build-list 7 identity)))
  ; Draw smaller towers, too.
  ;(draw-tower (take 
  (draw-tower initial-tower))

(define list-max (lambda (lst) (apply max lst)))

(define (horizontal-image-append images)
  (apply beside images))

(define (draw-towers towers blank-tower)
  (let* [(width-between-towers 50)
         (separator
           (rectangle width-between-towers
                      (image-height blank-tower)
                      'solid 'white))
         (tower-pics 
           (map (lambda (tower)
                  (if (tower-empty? tower)
                    blank-tower
                    (let [(tower-pic (draw-tower tower))]
                      (underlay/align/offset
                        'middle 'bottom
                        blank-tower
                        0
                        0
                        tower-pic))))
                towers))]
    (horizontal-image-append (add-between tower-pics separator))))

(define (replace-ref lst index value)
  (cond [(null? lst) lst]
        [(zero? index) (cons value (cdr lst))]
        [else (cons (car lst) 
                    (replace-ref (cdr lst) (sub1 index) value))]))
(define (remove-from-towers towers index)
  (replace-ref towers 
               index 
               (tower-rest (list-ref towers index))))
(define (add-to-towers towers index disk)
  (replace-ref towers 
               index 
               (tower-add (list-ref towers index) disk)))
(define (get-from-towers towers index)
  (tower-top (list-ref towers index)))


; Game:
; - There's three pegs (towers).
; - Pressing a number should remove a disk from a peg, if the peg has
;   a disk.
;   - After this point, there should be a disk "in limbo". This disk
;     is not on any peg.
;   - After pressing another number, the "in limbo" disk should go on
;     the peg specified by that number, if the disk can go on that
;     peg.


; limbo: (or/c boolean? positive?)
;   #f for no disk in limbo
;   positive? for disk in limbo
; towers: list[tower]
(define-struct game-state [limbo towers])
(define (make-towers-game widest-disk)
  (define num-towers 3)
  (define initial-tower (make-tower-with-bottom-disk widest-disk))
  (define height (* disk-height (tower-height initial-tower)))
  (define widest-disk-width (image-width (draw-disk widest-disk)))
  (define (initial-state->state initial)
    (if (state? initial)
      initial
      (make-state 
        #t 
        (make-game-state 
          #f 
          (cons initial-tower 
                (make-list (sub1 num-towers) null))))))
  (define (to-draw state)
    (let* [(widest-disk-width (image-width (draw-disk widest-disk)))
           (blank-tower (rectangle widest-disk-width 
                                   height 
                                   'solid 'white))
           (pub (state-public state))
           (towers (draw-towers (game-state-towers pub) blank-tower))
           (limbo-area (empty-scene (image-width towers)
                                    (* disk-height 3)))
           (limbo (if (game-state-limbo pub)
                    (overlay (draw-disk (game-state-limbo pub))
                             limbo-area)
                    limbo-area))
           (game-image (above limbo towers))]
      (overlay game-image 
               (empty-scene (image-width game-image)
                            (image-height game-image)))))
  (define (on-key state key)
    (let* [(pub (state-public state))
           (towers (game-state-towers pub))
           (limbo (game-state-limbo pub))
           (validate-index (lambda (index)
                             (if (number? (string->number index))
                               (sub1 (string->number index))
                               #f)))
           (valid-index? 
             (lambda (i) (and (number? i) (< i num-towers))))
           (index (validate-index key))]
      (if (eq? limbo #f)
        ; No limbo disk.
        (cond [(and (valid-index? index)
                    (tower-non-empty? (list-ref towers index)))
               (set-state-public
                 state
                 (make-game-state
                   (get-from-towers towers index)
                   (remove-from-towers towers index)))]
              [(string=? key "s")
               (begin (save-image (draw-towers towers)
                                  "current-output.jpg")
                      state)]
              [(string=? key "q") 
               (set-state-private state #f)]
              [else state])
        ; limbo disk.
        (cond [(valid-index? index)
               (if (tower-can-add? (list-ref towers index) limbo)
                 (set-state-public 
                   state
                   (make-game-state 
                     #f
                     (add-to-towers towers index limbo)))
                 state)]
              [else state]))))
  (define stop-when (compose1 (curry eq? #f) state-private))
  (lambda (dispatch)
    (case dispatch
      [(to-draw) to-draw]
      [(on-key) on-key]
      [(stop-when) stop-when]
      [(initial-state->state) initial-state->state])))
(module+ test
  (define num-towers 3)
  (define game (make-towers-game 7))
  (big-bang
    ((game 'initial-state->state) #f)
    [to-draw (game 'to-draw)]
    [on-key (game 'on-key) ]
    [stop-when (game 'stop-when)]))


