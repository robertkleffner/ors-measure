#lang racket

(require "ors.rkt")

(module+ test
    (require rackunit))

(define (earth-rhumb-destination lat lon distance bearing)
    (rhumb-destination lat lon distance bearing 6371000))

(define (rhumb-destination lat lon distance bearing radius)
    (define angular-dist (/ distance radius))
    (define lat-r (degrees->radians lat))
    (define lon-r (degrees->radians lon))
    (define bearing-r (degrees->radians bearing))
    (define d-theta (* angular-dist (cos bearing-r)))
    (define new-lat (+ lat-r d-theta))
    (define normalized-lat
        (if (> (abs new-lat) (/ pi 2))
            (if (> new-lat 0) (- pi new-lat) (- (- 0 pi) new-lat))
            new-lat))
    (define d-phi ()))