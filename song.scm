;; Base Definitions
(define resolution 256)
(define sample-rate 48000)
(define pi 3.141592653589793238)

;; This is like the opposite of absolute value.
;; It makes the square wave a lot easier.
(define (sign n)
  (cond ((negative? n) -1)
        ((positive? n) 1)
        (else 0)))

;; 'Cause the Scheme `modulo` function only works on integers...
(define (% x m) (- x (* (floor (/ x m)) m)))

;; TODO Note Lengths
(define tempo 120)

;; The input to this function is the distance from A over middle C.
(define (note->freq note)
        (* (expt 2 (/ note 12)) 440))

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

;; Imposes a volume on instrument
;; Volume is a decimal - e.g. 0.5 = half
(define (vol instrument volume)
        (lambda (pos) (* (instrument pos) volume)))

;; Temporary notes
;; We need to find a better way to do this.
(define b (sawtooth-wave (note->freq 2)))
(define c (sawtooth-wave (note->freq 3)))
(define d (sawtooth-wave (note->freq 5)))
(define e (sawtooth-wave (note->freq 7)))
(define f (sawtooth-wave (note->freq 9)))
(define g (sawtooth-wave (note->freq 10)))
(define a (sawtooth-wave (note->freq 12)))

;; Combines several notes or sequences into one
(define (chord . notes)
        (lambda (pos)
                (/ (apply + (map
                            (lambda (instrument) (instrument pos))
                            notes))
                   (length notes))))

;; Test chord sequence
(define I   (chord c e g))
(define II  (chord c f a))
(define III (chord b d g))
(define IV  (chord c e g))

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
(define (publish func)
        (let loop ((position 0))
             (print (justify (func (/ position sample-rate))))
             (if (< position (* sample-rate 1))
                 (loop (+ position 1)))))

(publish I)
(publish II)
(publish III)
(publish IV)
(exit)
