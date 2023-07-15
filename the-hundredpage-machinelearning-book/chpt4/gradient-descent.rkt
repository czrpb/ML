#lang racket

(displayln "Starting....\n")
(flush-output)

(require plot)
(plot-new-window? #t)

; Parameters
(define csv-file (make-parameter #f))
(define epochs (make-parameter 1))

; parse command-line
(define action
  (command-line
   #:once-each
   [("--csv-file") c "CSV filename" (csv-file c)]
   [("--epochs") e "Epochs to train" (epochs (string->number e))]
   ))

(define (csv-file-read csv-file field-sep)
  (let* [
         (csv-line-split (λ (line) (regexp-split field-sep (string-trim line "\""))))

         (csv-fh (open-input-file csv-file #:mode 'text))
         (header-line (csv-line-split (read-line csv-fh)))
         (header-line-len (length header-line))
         ]
    (for/list [(line (in-lines csv-fh))]
      (let [(fields (csv-line-split line))]
        (if [(negate =) header-line-len (length fields)]
            (raise "Failed to split record correctly; wrong number of resultant fields.")
            (map string->number fields)
            )
        )
      )
    )
  )

(define data (csv-file-read (csv-file) #px"\\s*\"?,\"?\\s*"))

(define (ŷ w b) (λ (x) (+ (* w x) b)))

(define (optimize data w b α)
  (let* [
         (data-len (length data))
         (∂w (λ (y x) (* -2 x (- y (+ (* w x) b)))))
         (∂b (λ (y x) (* -2 (- y (+ (* w x) b)))))
         (update-w (λ (w ∂w) (- w (* (/ 1 data-len) α ∂w))))
         (update-b (λ (b ∂b) (- b (* (/ 1 data-len) α ∂b))))
         ]
    (match-let
        [
         ((list Δw Δb) (foldl (λ (xy acc)
                                (list
                                 (+ (first acc) (∂w (second xy) (first xy)))
                                 (+ (second acc) (∂b (second xy) (first xy)))))
                              (list 0 0)
                              data))
         ]
      (list (update-w w Δw) (update-b b Δb))
      )
    )
  )

(define [train data epochs]
  (let* [
         (w 0)
         (b 0)
         (α 0.001)
         (snapshot
          (compose zero? (curryr remainder (quotient epochs 10))))
         ]
    (let loop [(epochs epochs)
               (w w)
               (b b)
               (regressions '())]
      (if (zero? epochs) (cons (list w b) regressions)
          (let [(new-wb (optimize data w b α))]
            (loop (sub1 epochs) (first new-wb) (second new-wb)
                  (if (snapshot epochs)
                      (cons (list w b) regressions)
                      regressions
                      )
                  )
            )
          )
      )
    )
  )
(define regressions (train data (epochs)))

(define (mse data w b)
  (let* [
         (data-len (length data))
         (ŷ (ŷ w b))
         (err (λ (x y) (- y (ŷ x))))
         (err² (compose sqr err))
         (map-err² (curry map (curry apply err²)))
         (sum-err² (compose (curry apply +) map-err²))
         (total-err (sum-err² data))
         ]
    (/ total-err data-len)
    )
  )

(pretty-print regressions)
(pretty-print (mse data (caar regressions) (cadar regressions)))
(pretty-print ((ŷ (caar regressions) (cadar regressions)) 23))

(define (make-plot renderers)
  (plot #:height 600 #:width 600
        #:x-min -0.5 #:x-max 51 #:y-min -0.5 #:y-max 27.5
        renderers
        )
  )

(make-plot
 (append
  (list (points data #:sym 'fullcircle #:fill-color 'blue #:alpha 0.4))
  (map
   (compose (curry function #:color 'red) (curry apply ŷ))
   (reverse (cdr regressions)))
  (list (function #:color 'green (ŷ (caar regressions) (cadar regressions))))
  )
 )
;(function (ŷ (caar regressions) (cadar regressions)))
