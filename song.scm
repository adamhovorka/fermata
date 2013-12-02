;; Base Definitions
(define resolution 256)
(define sample-rate 48000)
(define pi 3.141592653589793238)

;; Note Lengths
(define tempo 120)

(define (note->freq note)
        (* (expt 2 (/ note 12)) 440))

(define (instrument hertz)
        (lambda (pos)
                (sin (* pos hertz
                        2 pi))))

(define (vol instrument volume)
        (lambda (pos) (* (instrument pos) volume)))

;(define aoverc (vol (instrument (note->freq 0)) 0.5))
(define b (instrument (note->freq 2)))
(define c (instrument (note->freq 3)))
(define d (instrument (note->freq 5)))
(define e (instrument (note->freq 7)))
(define f (instrument (note->freq 9)))
(define g (instrument (note->freq 10)))
(define a (instrument (note->freq 12)))

(define (chord . notes)
        (lambda (pos)
                (/ (apply + (map
                            (lambda (instrument) (instrument pos))
                            notes))
                   (length notes))))

(define I   (chord c e g))
(define II  (chord c f a))
(define III (chord b d g))
(define IV  (chord c e g))

(define (cap x)
        (cond ((< x 0) 0)
              ((>= x resolution) (- resolution 1))
              (else x)))

(define (justify x)
        (let ((res (/ resolution 2)))
             (cap (inexact->exact (floor
                  (+ (* x res) res))))))

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
