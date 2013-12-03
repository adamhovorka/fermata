;; Base Definitions
(define tempo 120)
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

;; Combines several notes or sequences into one
(define (chord . notes)
        (lambda (pos)
                (/ (apply + (map
                            (lambda (instrument) (instrument pos))
                            notes))
                   (length notes))))

;;; }}}

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
	(define speed (/ tempo 60 4))
	(set! len (/ len speed))
        (let loop ((position 0))
             (print (justify (func (/ position sample-rate))))
             (if (< position (* sample-rate len))
                 (loop (+ position 1)))))

;;; }}}

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

;;; }}}

;(define test-instrument
        ;     waveform    A     D     S   R   effect
       ;(list square-wave 0.125 0.125 0.5 0.2 no-effect))

;; Temporary notes
;; We need to find a better way to do this.
(define waveform square-wave)
(define b (waveform (note->freq 2)))
(define c (waveform (note->freq 3)))
(define d (waveform (note->freq 5)))
(define e (waveform (note->freq 7)))
(define f (waveform (note->freq 9)))
(define g (waveform (note->freq 10)))
(define a (waveform (note->freq 12)))

;; Test chord sequence
(define I   (chord c e g))
(define II  (chord c f a))
(define III (chord b d g))
(define IV  (chord c e g))

(publish I   0.25)
(publish II  0.125)
(publish III 0.125)
(publish IV  0.5)
(exit)
