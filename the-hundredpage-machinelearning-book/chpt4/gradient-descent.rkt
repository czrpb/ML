#lang racket

(displayln "Starting....\n")

(require plot)
(plot-new-window? #t)

; Parameters
(define csv-file (make-parameter #f))

; parse command-line
(define action
  (command-line
   #:once-each
   [("--csv-file") c "CSV filename" (csv-file c)]
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
                                (printf "xy: ~a, acc: ~a\n" xy acc)
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
(define an-epoch (optimize data 0 0 0.001))
(pretty-print an-epoch)

(define (mse data w b)
  (let* [
         (data-len (length data))
         (predicted-y (λ (x) (+ (* w x) b)))
         (err (λ (y x) (- y (predicted-y x))))
         (err² (compose sqr err))
         (map-err² (curry map (curry apply err²)))
         (sum-err² (compose (curry apply +) map-err²))
         ]
    (printf "data: ~a, w: ~a, b: ~a\n" data w b)
    (/ (sum-err² data) data-len)
    )
  )

(pretty-print (mse data (first an-epoch) (second an-epoch)))

(plot #:x-min -0.5 #:x-max 51 #:y-min -0.5 #:y-max 27.5
      (points data))

