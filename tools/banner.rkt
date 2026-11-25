#lang racket/base
;; The profile banner: a black hole and its accretion disk, drawn by stippling.
;; The dots are the light: white on black in the dark banner, black on white in the light one,
;; like a photographic plate and its negative. Same seed, same dots, both files.
;; racket tools/banner.rkt   (writes banner-light.svg and banner-dark.svg)

(require racket/math)

(define W 1280) (define H 320)
(define cx 960) (define cy 160) (define R 62)                ; the black hole and its shadow radius
(define tilt 0.2)                                            ; how much of the disk's face we see

;; brightness ∈ [0, 1] at a point of the canvas
(define (brightness x y)
  (define u (/ (- x cx) R)) (define v (/ (- y cy) R))
  (define rho (sqrt (+ (sqr u) (sqr v))))
  (define beam (- 1 (* 0.45 (/ u (max rho 1e-9)))))          ; Doppler: the side coming towards us is brighter
  ;; the thin disk, seen almost edge-on: in front of the shadow below, hidden by it above
  (define rd (sqrt (+ (sqr u) (sqr (/ v tilt)))))
  (define disk
    (if (and (< 1.5 rd 3.6) (or (> v 0) (> rho 1)))
        (* 0.95 (expt (/ 1.5 rd) 1.2) (- 1 (* 0.4 (/ u rd))))
        0))
  ;; the far side of the disk, bent by gravity into a halo around the shadow, and the photon ring
  (define halo (if (> rho 1) (* 0.55 (exp (- (sqr (/ (- rho 1.45) 0.32))))) 0))
  (define ring (* 0.9 (exp (- (sqr (/ (- rho 1.08) 0.07))))))
  (define stars (if (> rho 1) (* 0.00004 (max 0 (min 1 (/ (- x 640) 240)))) 0)) ; a few, away from the name
  (max 0 (min 1 (+ stars (* beam (+ halo ring)) disk))))

;; Stippling: candidates in random order, kept with probability brightness, never too close.
(define cell 12)
(define (stipple n)
  (random-seed 1916)                                         ; Schwarzschild's solution
  (define grid (make-hash))
  (define (key x y) (cons (quotient (exact-floor x) cell) (quotient (exact-floor y) cell)))
  (define (near? x y spacing)
    (for*/or ([i (in-range -1 2)] [j (in-range -1 2)])
      (define k (key x y))
      (for/or ([p (hash-ref grid (cons (+ (car k) i) (+ (cdr k) j)) '())])
        (< (+ (sqr (- x (car p))) (sqr (- y (cdr p)))) (sqr spacing)))))
  (for/fold ([dots '()]) ([_ n])
    (define x (* W (random))) (define y (* H (random)))
    (define b (brightness x y))
    (cond [(and (< (random) (expt b 0.8)) (not (near? x y (+ 2.1 (* 9 (expt (- 1 b) 1.5))))))
           (hash-update! grid (key x y) (λ (l) (cons (cons x y) l)) '())
           (cons (cons x y) dots)]
          [else dots])))

(define dots (reverse (stipple 1500000)))

(define (banner file ink paper)
  (with-output-to-file file #:exists 'truncate
    (λ ()
      (printf "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 ~a ~a\" width=\"~a\" height=\"~a\" role=\"img\" aria-label=\"alvise vitturi — formally verified observability in agda\">\n" W H W H)
      (printf "<rect width=\"100%\" height=\"100%\" fill=\"~a\"/>\n<g fill=\"~a\">\n" paper ink)
      (for ([p dots])
        (printf "<circle cx=\"~a\" cy=\"~a\" r=\"1.3\"/>\n" (/ (round (* 10 (car p))) 10) (/ (round (* 10 (cdr p))) 10)))
      (printf "</g>\n<g fill=\"~a\">\n" ink)
      (printf "<text x=\"72\" y=\"168\" font-family=\"serif\" font-size=\"64\" letter-spacing=\"2\">alvise vitturi</text>\n")
      (printf "<text x=\"74\" y=\"212\" font-family=\"monospace\" font-size=\"16\" letter-spacing=\"4\" opacity=\"0.7\">FORMALLY VERIFIED OBSERVABILITY · AGDA</text>\n")
      (printf "</g>\n</svg>\n"))))

(banner "banner-light.svg" "#111" "#fff")
(banner "banner-dark.svg" "#eee" "#0b0b0b")
