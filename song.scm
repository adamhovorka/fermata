;; Base Definitions
(define tempo 120)
(define time-signature (/ 4 4))
(define speed (/ tempo 60 4))

(define resolution 256)
(define sample-rate 48000)
(define pi 3.141592653589793238)

;;; {{{ Utilities

;; This is like the opposite of absolute value.
;; It makes the square wave a lot easier.
(define (sign n)
  (cond ((negative? n) -1)
        ((positive? n) 1)
        (else 0)))

;; 'Cause the Scheme `modulo` function only works on integers...
(define (% x m) (- x (* (floor (/ x m)) m)))

;; The input to this function is the distance from A over middle C.
(define (note->freq note)
        (* (expt 2 (/ note 12)) 440))

;; Imposes a volume on instrument
;; Volume is a decimal - e.g. 0.5 = half
(define (vol instrument volume)
        (lambda (pos) (* (instrument pos) volume)))

;; No effect = identity function
(define (no-effect x) x)

;;;  }}}
;;; {{{ Rendering functions

;; Caps the output at the resolution so it doesn't b0rk everything else.
(define (cap x)
        (cond ((< x 0) 0)
              ((>= x resolution) (- resolution 1))
              (else x)))

;; Justifies the output to the specified resolution.
;; The input to this function should be in the range -1 < x < 1
(define (justify x)
        (let ((res (/ resolution 2)))
             (cap (inexact->exact (floor
                  (+ (* x res) res))))))

;; Takes in a function and outputs the sampled hex values.
(define (publish func len)
	(set! len (/ len speed))
        (let loop ((position 0))
             (print (justify (func (/ position sample-rate))))
             (if (< position (* sample-rate len))
                 (loop (+ position 1)))))

;;;  }}}
;;; {{{ Basic instruments

;; Basic sine wave instrument
(define (sine-wave hertz)
        (lambda (pos)
                (sin (* pos hertz
                        2 pi))))

;; Basic square wave instrument
(define (square-wave hertz)
        (lambda (pos)
                (sign (sin (* pos hertz
                             2 pi)))))

;; Basic triangle wave instrument
(define (triangle-wave hertz)
        (lambda (pos)
                (- (abs (- (%
                           (* pos hertz 4)
                           4) 2)) 1)))

;; Basic sawtooth wave instrument
(define (sawtooth-wave hertz)
        (lambda (pos)
                (* 2 (- (* pos hertz)
                        (floor (+ 0.5 (* pos hertz)))))))

;; Basic random noise instrument
(define (random-noise hertz)
        (lambda (pos)
                (- (/ (random resolution)
                      (/ resolution 2)) 1)))

;;;  }}}
;;; {{{ "Means of combination"

;; Combines several notes or sequences into one
(define (chord . notes)
        (lambda (pos)
                (/ (apply + (map
                            (lambda (instrument) (instrument pos))
                            notes))
                   (length notes))))

(define (sequence instrument notes)
        (define times
                (let loop ((in (map cadr notes))
                           (out '())
                           (time 0))
                     (if (null? in) out
                         (loop (cdr in)
                               (append out (list (/ (+ time (car in)) speed)))
                               (+ time (car in))))))
        (lambda (pos)
                (let loop ((notes notes) (times times))
                          (if (null? times) 0  ; If you can find a better
                                               ; way to fix this, do it.
                              (if (<= pos (car times))
                                  (if (eq? (caar notes) 'off) 0
                                      (((car instrument)
                                                (note->freq (caar notes))) pos))
                                  (loop (cdr notes) (cdr times)))))))

;;;  }}}
;;; {{{ Comprehension

(define (transpose notes amount)
        (let loop ((in notes) (out '()))
                  (if (null? in) out
                      (loop (cdr in)
                            (if (eq? (caar in) 'off)
                                (append out (list (car in)))
                                (append out (list
                                        (cons (+ (caar in) amount)
                                              (cdar in)))))))))

;;;  }}}

(define test-instrument
        ;     waveform    A     D     S   R   effect
        (list square-wave 0.125 0.125 0.5 0.2 no-effect))

(define scale
  '((0 0.25)
    (2 0.125)
    (4 0.125)
    (off 0.125)
    (5 0.375)))

(define scale-with-finale
        (chord (sequence test-instrument scale)
               (sequence test-instrument '((off 0.625) (9 0.375)))
               ;(sequence test-instrument '((off 0.625) (12 0.375)))
        ))

(publish scale-with-finale 1)
(exit)
