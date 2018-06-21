#lang racket

(require 2htdp/image)
(require 2htdp/universe)

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

; A tower is a list of numbers of strictly decreasing size.
; The first element (car tower) is the largest number, and (cdr tower)
; is, itself, a tower.
(define tower-height length)
(define tower-top last)
(define (tower-bottom tower)
  (take tower (sub1 (length tower))))
(define (tower-add tower disk)
  (append tower (list disk)))
(define tower-empty? null?)
(define tower-non-empty? pair?)
(define (tower-can-add? tower disk)
  (or (tower-empty? tower) (< disk (tower-top tower))))

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

(define list-max (lambda (lst) (apply max lst)))

; list[images]  -> image
(define (horizontal-image-append images)
  (if (null? images)
    empty-image
    (let [(append-rest (horizontal-image-append (cdr images)))]
      (overlay/xy (car images)
                  (image-width (car images))
                  0
                  append-rest))))

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

(define initial-tower (list 7 6 5 4 3 2 1))
(define (replace-ref lst index value)
  (cond [(null? lst) lst]
        [(zero? index) (cons value (cdr lst))]
        [else (cons (car lst) 
                    (replace-ref (cdr lst) (sub1 index) value))]))
(define (remove-from-towers towers index)
  (replace-ref towers 
               index 
               (tower-bottom (list-ref towers index))))
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
(define-struct state [limbo towers])
(define num-towers 3)
(big-bang
  (make-state #f (cons initial-tower 
                       (make-list (sub1 num-towers) null)))

  [to-draw 
    (let* [(widest-disk (car initial-tower))
           (widest-disk-width (image-width (draw-disk widest-disk)))
           (height (* disk-height (tower-height initial-tower)))
           (blank-tower (rectangle widest-disk-width 
                                   height 
                                   'solid 'white))]
      (lambda (state)
        (let* [(towers (draw-towers (state-towers state) blank-tower))
               (limbo-area (empty-scene (image-width towers)
                                        (* disk-height 3)))
               (limbo (if (state-limbo state)
                        (overlay (draw-disk (state-limbo state))
                                 limbo-area)
                        limbo-area))
               (game-image (above limbo towers))]
          (overlay game-image (empty-scene (image-width game-image)
                                           (image-height game-image))))))]

  [on-key
    (lambda (state key)
      (let* [(towers (state-towers state))
             (limbo (state-limbo state))
             (validate-index (lambda (index)
                               (if (number? (string->number index))
                                 (sub1 (string->number index))
                                 #f)))
             (index (validate-index key))]
        (if (eq? limbo #f)
          ; No limbo disk.
          (cond [(and (number? index) (< index num-towers)
                      (tower-non-empty? (list-ref towers index)))
                 (make-state (get-from-towers towers index)
                             (remove-from-towers towers index))]
                [(string=? key "s")
                 (begin (save-image (draw-towers towers)
                                    "current-output.jpg")
                        state)]
                [(string=? key "q") #f ]
                [else state])
          ; limbo disk.
          (cond [(and (number? index) (< index num-towers))
                 (if (tower-can-add? (list-ref towers index) limbo)
                    (make-state #f
                                (add-to-towers towers index limbo))
                    state)]
                [else state]))))]
  [stop-when boolean?])


