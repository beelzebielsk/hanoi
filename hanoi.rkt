#lang racket

(require 2htdp/image)
(require 2htdp/universe)

(define disk-height 20)
; A disk is a positive number.
(define (draw-disk disk)
  (rectangle (+ 40 (* disk 20)) disk-height 'solid 'black))

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
  (< disk (tower-top tower)))

; image? positive? -> positive?
; Takes an image, and the width of a larger image.
; Returns the x-offset that would center (horizontally) the smaller
; image in the larger image.
;
; Assumption in the naming scheme is that the larger picture is an
; underlay for the smaller picture.
(define (bottom-center/x over under-width)
  (let [(smaller-midpoint (/ (image-width over) 2))
        (larger-midpoint (/ under-width 2))]
    (- larger-midpoint smaller-midpoint)))

; tower? -> image?
(define (draw-tower tower)
  (define (iter starting-point tower scene)
    (if (tower-empty? tower)
      scene
      (let [(disk (draw-disk (car tower)))] 
        (iter (+ starting-point disk-height)
              (cdr tower)
              (underlay/align/offset
                'left 'bottom
                scene
                (bottom-center/x disk (image-width scene))
                (- starting-point)
                disk)))))
  (iter 0
        tower
        empty-image))

(define tower-separation 50)
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

(define (draw-towers towers)
  (define widest-disk 
    (list-max (map list-max (filter tower-non-empty? towers))))
  (define highest-tower (* disk-height 
                           (list-max (map tower-height towers))))
  (define blank-tower 
    (rectangle (image-width (draw-disk widest-disk))
               highest-tower
               'solid 'white))
  (define separator
    (rectangle tower-separation
               highest-tower
               'solid 'white))
  (define tower-pics 
    (map (lambda (tower)
           (if (tower-empty? tower)
             blank-tower
             (let [(tower-pic (draw-tower tower))]
               (underlay/align/offset
                 'left 'bottom
                 blank-tower
                 (bottom-center/x tower-pic
                                  (image-width blank-tower))
                 0
                 tower-pic))))
         towers))
  (horizontal-image-append (add-between tower-pics separator)))

(define initial-tower (list 5 4 3 2 1))
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
    (lambda (state)
      (let* [(towers (draw-towers (state-towers state)))
             (tower-area (overlay
                           towers
                           (empty-scene (image-width towers)
                                        (image-height towers))))
            (limbo-area (empty-scene (image-width towers)
                                     (* disk-height 3)))
            (limbo (if (state-limbo state)
                     (overlay (draw-disk (state-limbo state))
                              limbo-area)
                     limbo-area))]
        (above limbo tower-area)))]
  [on-key
    (lambda (state key)
      (if (eq? (state-limbo state) #f)
        ; No limbo disk.
        (cond [(string=? key "1") 
               (make-state (get-from-towers (state-towers state) 0)
                           (remove-from-towers (state-towers state) 0))]
              [(string=? key "2") 
               (make-state (get-from-towers (state-towers state) 1)
                           (remove-from-towers (state-towers state) 1))]
              [(string=? key "3") 
               (make-state (get-from-towers (state-towers state) 2)
                           (remove-from-towers (state-towers state) 2))]
              [(string=? key "s")
               (begin (save-image (draw-towers (state-towers state))
                                  "current-output.jpg")
                      state)]
              [(string=? key "q") #f ])
        ; limbo disk.
        (cond [(string=? key "1") 
               (make-state #f
                           (add-to-towers (state-towers state) 0 (state-limbo state)))]
              [(string=? key "2") 
               (make-state #f
                           (add-to-towers (state-towers state) 1 (state-limbo state)))]
              [(string=? key "3") 
               (make-state #f
                           (add-to-towers (state-towers state) 2 (state-limbo state)))])))]
  [stop-when boolean?])


