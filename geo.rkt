#lang racket

(module+ test
    (require rackunit))

(provide haversine-dist
         rhumb-dest-point
         normalize-lat
         normalize-lon)

(define (haversine-dist lat1 lon1 lat2 lon2)
    (define earth-mean-radius-m 6371000)
    (define dist-lat (degrees->radians (- lat2 lat1)))
    (define dist-lon (degrees->radians (- lon2 lon1)))
    (define lat1-rad (degrees->radians lat1))
    (define lat2-rad (degrees->radians lat2))
    (define a (+ (sqr (sin (/ dist-lat 2))) (* (sqr (sin (/ dist-lon 2))) (cos lat1-rad) (cos lat2-rad))))
    (define c (* 2 (atan (sqrt a) (sqrt (- 1 a)))))
    (* earth-mean-radius-m c))

(define (rhumb-dest-point lat lon dist-m bearing)
    (define quarter-pi (/ pi 4))
    (define earth-mean-radius-m 6371000)
    (define lat-rad (degrees->radians lat))
    (define bearing-rad (degrees->radians bearing))
    (define angular-dist (/ dist-m earth-mean-radius-m))
    (define delta-lat (* angular-dist (cos bearing-rad)))
    (define new-lat (+ lat-rad delta-lat))
    (define proj-lat-dist (log (/ (tan (+ quarter-pi (/ new-lat 2))) (tan (+ quarter-pi (/ lat-rad 2))))))
    (define q (if (> (abs proj-lat-dist) 0.000000000001) (/ delta-lat proj-lat-dist) (cos lat-rad)))
    (define delta-lon (* angular-dist (/ (sin bearing-rad) q)))
    (define new-lon (+ (degrees->radians lon) delta-lon))
    (cons (normalize-lat (radians->degrees new-lat))
          (normalize-lon (radians->degrees new-lon))))

(define (normalize-lat lat)
    (if (> (abs lat) 90)
        (if (> lat 0)
            (- 180 lat)
            (- (- 180) lat))
        lat))

(define (normalize-lon lon)
    (cond
        [(< lon -180) (normalize-lon (+ lon 360))]
        [(> lon 180) (normalize-lon (- lon 360))]
        [else lon]))

(module+ test
    (check-equal? (haversine-dist 45 145 45 145) 0)
    (check-equal? (round (haversine-dist 45 144 45 145)) 78626.0)
    (check-equal? (round (haversine-dist 44 145 45 145)) 111195.0)
    (check-equal? (round (haversine-dist 44 144 45 145)) 136578.0)

    (check-equal? (rhumb-dest-point 51.125556 1.338056 40230 116.636111)
        (cons 50.963333 1.8525)))