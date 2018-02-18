#lang racket

(require "geo.rkt")

(provide file->ors-grid
         port->ors-grid
         ors-grid->ors
         generate-flag-pole
         eight-thousand-meter-flag-pole)

(struct ors-grid (cols rows ll-lat ll-lon step grid) #:transparent)

(define f-coeff (/ 4 (* pi pi pi)))

(define (f u)
    (cond
        [(<= u 0) 0]
        [else
         (define atanu (atan u))
         (* f-coeff (- (* 2 u atanu) (log (add1 (* u u))) (sqr atanu)))]))

(define (generate-flag-pole ll-lat ll-lon cols rows step height)
    (define peak-col (inexact->exact (truncate (/ cols 2.0))))
    (define peak-row (inexact->exact (truncate (/ rows 2.0))))
    (define grid
        (for/list ([y (in-range rows)])
            (for/list ([x (in-range cols)])
                (if (and (= peak-col x) (= peak-row y))
                    height
                    0))))
    (ors-grid cols rows ll-lat ll-lon step grid))

(define (eight-thousand-meter-flag-pole) (generate-flag-pole 34.787916666663 74.038472222256 3964 3238 0.000277777778 8000))

(define (integrate-grid-midpoint f xs ys xstep ystep)
    (for*/sum ([x (in-range xs)]
               [y (in-range ys)])
        (* (f x y) xstep ystep)))

(define (ors-grid->ors grid)
    (define elev (ors-grid-grid grid))
    (define cols (ors-grid-cols grid))
    (define rows (ors-grid-rows grid))
    (define ll-lat (ors-grid-ll-lat grid))
    (define ll-lon (ors-grid-ll-lon grid))
    (define step (ors-grid-step grid))

    (define (heightfun col row)
        (list-ref (list-ref elev row) col))
    (define (col->lon col)
        (+ ll-lon (* col step)))
    (define (row->lat row)
        (+ ll-lat (* row step)))

    (define peak-col (inexact->exact (truncate (/ cols 2.0))))
    (define peak-row (inexact->exact (truncate (/ rows 2.0))))
    (define peak-height (heightfun peak-col peak-row))
    (define peak-lon (col->lon peak-col))
    (define peak-lat (row->lat peak-row))

    (define lr-lon (normalize-lon (+ ll-lon (* step cols))))
    (define tl-lat (normalize-lat (- ll-lat (* step rows))))

    (define x-cell-dist (/ (haversine-dist ll-lat ll-lon ll-lat lr-lon) cols))
    (define y-cell-dist (/ (haversine-dist ll-lat ll-lon tl-lat ll-lon) rows))

    (define (r x y)
        (haversine-dist peak-lat peak-lon (row->lat y) (col->lon x)))
    (define (u x y)
        (define dist (r x y))
        (if (= dist 0)
            0
            (/ (- peak-height (heightfun x y)) dist)))
    (define (fu x y) (f (u x y)))

    (sqrt (integrate-grid-midpoint fu cols rows x-cell-dist y-cell-dist)))

(define (file->ors-grid filename)
    (call-with-input-file filename port->ors-grid #:mode 'text))

(define (port->ors-grid port)
    (define get-value (compose1 string->number second string-split))
    (define cols (get-value (read-line port)))
    (define rows (get-value (read-line port)))
    (define ll-lon (get-value (read-line port)))
    (define ll-lat (get-value (read-line port)))
    (define step (get-value (read-line port)))
    (read-line port)
    (define grid
        ;; reversed so rows are in order of increasing latitude
        (reverse
            (for/list ([r (in-range rows)])
                (map string->number (string-split (read-line port))))))
    (ors-grid cols rows ll-lat ll-lon step grid))