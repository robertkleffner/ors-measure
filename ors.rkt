#lang racket

(module+ test
    (require rackunit))

(struct ors-config (lat-top lon-left lat-step lon-step))

(define (ors x0 y0 heightfun config)
    (define h0 (heightfun x0 y0))
    (define lat0 (to-lat x0 config))
    (define lon0 (to-lon x0 config))
    (define (r x y)
        (haversine-dist lat0 lon0 (to-lat y config) (to-lon x config)))
    (define (u x y)
        (/ (- h0 (heightfun x y)) (r x y)))
    (define (fu x y) (f (u x y)))

    (sqrt (trapezoid2 fu xs ys)))

(define (to-lat y config)
    (+ (config-lat-top config) (* (config-lat-step config) y)))

(define (to-lon x config)
    (+ (config-lon-left config) (* (config-lon-step config) x)))

(define f-coeff (/ 1 (* pi pi pi)))

(define (f u)
    (define atanu (atan u))
    (* f-coeff (- (* 2 u atanu) (log (add1 (* u u))) (sqr atanu))))

;; trapezoid2 : (Number, Number -> Number) -> [Number] -> [Number] -> Number
;; 2D Trapezoid integration.
(define (trapezoid2 f xs ys)
    (when (or (empty? xs) (empty? ys))
        (error 'trapezoid2 "Input sample lists cannot be empty."))
    (define lx (sub1 (length xs)))
    (define ly (sub1 (length ys)))
    (define h (/ (- (list-ref xs lx) (first xs)) lx))
    (define k (/ (- (list-ref ys ly) (first ys)) ly))
    (define coeff (/ (* h k) 4))
    (* coeff
        (for*/fold ([sum 0])
                   ([(ex ix) (in-indexed xs)]
                    [(ey iy) (in-indexed ys)])
            (displayln (string-append "ix: " (number->string ix) ", iy: " (number->string iy)))
            (displayln (get-weight ix iy lx ly))
            (+ sum (* (get-weight ix iy lx ly) (f ex ey))))))

(define (get-weight ix iy len-x len-y)
    (cond
        [(or (and (equal? ix 0) (equal? iy 0)) (and (equal? ix len-x) (equal? iy len-y)))
         1]
        [(or (equal? ix 0) (equal? ix len-x) (equal? iy 0) (equal? iy len-y))
         2]
        [else
         4]))

;; haversine-dist : Latitude, Longitude, Latitude, Longitude -> Meters
(define (haversine-dist lat1 lon1 lat2 lon2)
    (define earth-mean-radius-m 6371000)
    (define dist-lat (degrees->radians (- lat2 lat1)))
    (define dist-lon (degrees->radians (- lon2 lon1)))
    (define lat1-rad (degrees->radians lat1))
    (define lat2-rad (degrees->radians lat2))
    (define a (+ (sqr (sin (/ dist-lat 2))) (* (sqr (sin (/ dist-lon 2))) (cos lat1-rad) (cos lat2-rad))))
    (define c (* 2 (atan (sqrt a) (sqrt (- 1 a)))))
    (* earth-mean-radius-m c))

(module+ test
    (check-equal? (haversine-dist 45 145 45 145) 0)
    (check-equal? (round (haversine-dist 45 144 45 145)) 78626.0)
    (check-equal? (round (haversine-dist 44 145 45 145)) 111195.0)
    (check-equal? (round (haversine-dist 44 144 45 145)) 136578.0))