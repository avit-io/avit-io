#lang racket/base
;; Puntini: a dot engine, after the picture language of SICP 2.2.4.
;; A painter is a procedure from a point of the unit square (u, v ∈ [0, 1], v down) to an ink
;; ∈ [0, 1], or #f outside the picture. Painters combine like Henderson's: over, beside, below,
;; transform-painter. Then they are applied: stipple turns the ink into dots, denser where it is
;; darker, and svg writes them in the two Ocra themes.
;; racket tools/puntini.rkt   (runs the check)

(require racket/math (only-in racket/port with-output-to-string))
(provide disc sphere glow gradient rect segment
         over mask scale-ink transform-painter beside below flip-horiz
         stipple svg ocra-light ocra-dark)

;; --- primitive painters: shapes and their shadows ---

(define (dist u v cu cv) (sqrt (+ (sqr (- u cu)) (sqr (- v cv)))))

(define ((disc cu cv r [ink 1.0]) u v) (if (<= (dist u v cu cv) r) ink 0.0))

;; a lit ball: Lambert with the light from (lx, ly, lz), the dots gather on the side in shadow
(define ((sphere cu cv r #:light [l '(-0.5 -0.6 0.62)] #:ambient [amb 0.08]) u v)
  (define x (/ (- u cu) r)) (define y (/ (- v cv) r)) (define d2 (+ (sqr x) (sqr y)))
  (cond [(> d2 1) 0.0]
        [else (define z (sqrt (- 1 d2)))
              (define lit (max 0.0 (+ (* x (car l)) (* y (cadr l)) (* z (caddr l)))))
              (- 1.0 (min 1.0 (+ amb lit)))]))

;; a halo: ink that falls off with the distance, for light and for haze
(define ((glow cu cv r [ink 1.0]) u v) (* ink (exp (- (sqr (/ (dist u v cu cv) r))))))

;; a filled box from (u0, v0) to (u1, v1)
(define ((rect u0 v0 u1 v1 [ink 1.0]) u v) (if (and (<= u0 u u1) (<= v0 v v1)) ink 0.0))

;; a stroke from a to b, w thick: pointers and frames
(define ((segment a b w [ink 1.0]) u v)
  (define-values (ax ay dx dy) (values (car a) (cdr a) (- (car b) (car a)) (- (cdr b) (cdr a))))
  (define t (max 0.0 (min 1.0 (/ (+ (* (- u ax) dx) (* (- v ay) dy)) (+ (sqr dx) (sqr dy))))))
  (if (<= (dist u v (+ ax (* t dx)) (+ ay (* t dy))) (/ w 2)) ink 0.0))

;; from ink a at the top to ink b at the bottom
(define ((gradient a b) u v) (+ a (* (- b a) v)))

;; --- combinators: the means of combination ---

(define ((over . ps) u v)                                     ; the darkest of the painters
  (for/fold ([k 0.0]) ([p ps]) (define i (p u v)) (if i (max k i) k)))
(define ((mask p shape) u v) (and (> (shape u v) 0) (p u v)))  ; p, only where shape has ink
(define ((scale-ink p k) u v) (let ([i (p u v)]) (and i (* k i))))

;; SICP's frame: the painter drawn in the parallelogram origin, edge1, edge2 of the unit square
(define ((transform-painter p origin e1 e2) u v)
  (define-values (ox oy) (values (car origin) (cdr origin)))
  (define-values (ax ay bx by) (values (- (car e1) ox) (- (cdr e1) oy) (- (car e2) ox) (- (cdr e2) oy)))
  (define det (- (* ax by) (* ay bx)))
  (define s (/ (- (* (- u ox) by) (* (- v oy) bx)) det))
  (define t (/ (- (* ax (- v oy)) (* ay (- u ox))) det))
  (and (<= 0 s 1) (<= 0 t 1) (p s t)))

(define (beside a b) (over (transform-painter a '(0 . 0) '(1/2 . 0) '(0 . 1))
                           (transform-painter b '(1/2 . 0) '(1 . 0) '(1/2 . 1))))
(define (below a b) (over (transform-painter a '(0 . 0) '(1 . 0) '(0 . 1/2))
                          (transform-painter b '(0 . 1/2) '(1 . 1/2) '(0 . 1))))
(define (flip-horiz p) (transform-painter p '(1 . 0) '(0 . 0) '(1 . 1)))

;; --- apply: the dots ---

;; Candidates in random order, kept with probability ink^0.8 and never closer than a spacing that
;; grows where the ink is light. Deterministic for a seed.
(define (stipple painter w h #:n n #:seed seed #:spacing [near0 2.1] #:spread [spread 9] #:cell [cell 12])
  (random-seed seed)
  (define grid (make-hash))
  (define (key x y) (cons (quotient (exact-floor x) cell) (quotient (exact-floor y) cell)))
  (define (near? x y spacing)
    (for*/or ([i (in-range -1 2)] [j (in-range -1 2)])
      (define k (key x y))
      (for/or ([p (hash-ref grid (cons (+ (car k) i) (+ (cdr k) j)) '())])
        (< (+ (sqr (- x (car p))) (sqr (- y (cdr p)))) (sqr spacing)))))
  (reverse
   (for/fold ([dots '()]) ([_ n])
     (define u (random)) (define v (random))
     (define x (* w u)) (define y (* h v))
     (define b (or (painter u v) 0.0))
     (cond [(and (< (random) (expt b 0.8)) (not (near? x y (+ near0 (* spread (expt (- 1 b) 1.5))))))
            (hash-update! grid (key x y) (λ (l) (cons (cons x y) l)) '())
            (cons (cons x y) dots)]
           [else dots]))))

;; the two themes of Ocra: ink on paper
(define ocra-light '("#15130f" . "#efe0b4"))                 ; nero on ocra-pastello
(define ocra-dark '("#d6b866" . "#15130f"))                  ; ocra on nero

;; the dots as an SVG document; extra is more SVG on top (text, a frame), in the same ink
(define (svg dots w h #:theme [theme ocra-light] #:r [r 1.3] #:label [label ""]
             #:background [bg (λ (paper) (format "<rect width=\"100%\" height=\"100%\" fill=\"~a\"/>\n" paper))]
             #:extra [extra (λ (ink) "")])
  (define ink (car theme))
  (with-output-to-string
    (λ ()
      (printf "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 ~a ~a\" width=\"~a\" height=\"~a\" role=\"img\" aria-label=\"~a\">\n" w h w h label)
      (display (bg (cdr theme)))
      (printf "<g fill=\"~a\">\n" ink)
      (for ([p dots])
        (printf "<circle cx=\"~a\" cy=\"~a\" r=\"~a\"/>\n" (/ (round (* 10 (car p))) 10) (/ (round (* 10 (cdr p))) 10) r))
      (printf "</g>\n~a</svg>\n" (extra ink)))))

;; the check: dots follow the ink, none where there is none, the same dots for the same seed,
;; and a lit ball is darker on the side away from the light
(module+ main
  (define (count dots pred) (for/sum ([p dots]) (if (pred (car p) (cdr p)) 1 0)))
  (define half (beside (gradient 0.2 0.2) (gradient 0.9 0.9)))
  (define d (stipple half 200 100 #:n 60000 #:seed 1))
  (define left (count d (λ (x y) (< x 100))))
  (unless (< (* 2 left) (- (length d) left)) (error 'puntini "dense side not denser: ~a of ~a" left (length d)))
  (unless (= 0 (length (stipple (disc 0.5 0.5 0.2 0.0) 100 100 #:n 20000 #:seed 1))) (error 'puntini "dots with no ink"))
  (unless (equal? d (stipple half 200 100 #:n 60000 #:seed 1)) (error 'puntini "not deterministic"))
  (define b (stipple (sphere 0.5 0.5 0.4) 200 200 #:n 80000 #:seed 2))
  (unless (< (* 2 (count b (λ (x y) (< (+ x y) 200)))) (count b (λ (x y) (> (+ x y) 200))))
    (error 'puntini "the lit side is not lighter"))
  (unless (equal? ((transform-painter (gradient 0 1) '(0 . 0) '(1 . 0) '(0 . 1/2)) 0.3 0.25) 0.5)
    (error 'puntini "frame transform"))
  (unless (and (= 1.0 ((segment '(0 . 0) '(1 . 1) 0.02) 0.5 0.5)) (= 0.0 ((segment '(0 . 0) '(1 . 1) 0.02) 0.5 0.6))
               (= 1.0 ((rect 0.2 0.2 0.4 0.4) 0.3 0.3)) (= 0.0 ((rect 0.2 0.2 0.4 0.4) 0.5 0.3)))
    (error 'puntini "rect or segment"))
  (printf "puntini: ok (~a dots, ~a on the light side)\n" (length d) left))
