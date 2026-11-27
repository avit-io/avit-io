#lang racket/base
;; The profile banner: a black hole and its accretion disk, drawn by stippling.
;; The dots are the light: ochre on black in the dark banner, nero on ochre in the light one,
;; like a photographic plate and its negative. Same seed, same dots, both files.
;; racket tools/banner.rkt   (writes banner-light.svg and banner-dark.svg)

(require racket/math "puntini.rkt")

(define W 1280) (define H 320)
(define cx 960) (define cy 160) (define R 62)                ; the black hole and its shadow radius
(define tilt 0.2)                                            ; how much of the disk's face we see

;; brightness ∈ [0, 1] at a point of the canvas
(define (brightness x y)
  (define u (/ (- x cx) R)) (define v (/ (- y cy) R))
  (define rho (sqrt (+ (sqr u) (sqr v))))
  (define side (- 1 (* 0.25 (/ u (max rho 1e-9)))))          ; Doppler, mild: the side coming towards us is brighter
  ;; the thin disk, seen almost edge-on: in front of the shadow, across it
  (define rd (sqrt (+ (sqr u) (sqr (/ v tilt)))))
  (define disk
    (if (and (< 1.25 rd 3.8) (or (> v 0) (> rho 1.05)))
        (* 0.9 (expt (/ 1.25 rd) 0.8) (- 1 (* 0.25 (/ u rd))) (min 1 (* 3 (- 3.8 rd))))
        0))
  ;; the far side of the disk, bent by gravity: a thick arc over the shadow, thin at its sides,
  ;; and a fainter, thinner one under it; then the photon ring, hugging the shadow
  (define s (abs (/ v (max rho 1e-9))))                      ; 1 above and below, 0 at the sides
  (define arc
    (cond [(<= rho 1) 0]
          [(< v 0) (* 0.8 (exp (- (sqr (/ (- rho 1.3) (+ 0.06 (* 0.32 s)))))))]
          [else (* 0.4 (exp (- (sqr (/ (- rho 1.12) (+ 0.04 (* 0.08 s)))))))]))
  (define ring (* 0.95 (exp (- (sqr (/ (- rho 1.04) 0.05))))))
  (define stars (if (> rho 1) (* 0.00004 (max 0 (min 1 (/ (- x 640) 240)))) 0)) ; a few, away from the name
  (max 0 (min 1 (+ stars (* side (max arc ring)) disk))))

(define dots (stipple (λ (u v) (brightness (* W u) (* H v))) W H #:n 1500000 #:seed 1916)) ; Schwarzschild

(define (banner file theme)
  (with-output-to-file file #:exists (quote truncate)
    (λ () (display (svg dots W H #:theme theme #:label "alvise vitturi — type theory in agda and racket"
                        #:extra (λ (ink) (format "<g fill=\"~a\">\n<text x=\"72\" y=\"168\" font-family=\"serif\" font-size=\"64\" letter-spacing=\"2\">alvise vitturi</text>\n<text x=\"74\" y=\"212\" font-family=\"monospace\" font-size=\"16\" letter-spacing=\"4\" opacity=\"0.7\">TYPE THEORY · AGDA · RACKET</text>\n</g>\n" ink)))))))

(banner "banner-light.svg" ocra-light)                   ; Ocra: nero on ocra-pastello
(banner "banner-dark.svg" ocra-dark)                     ; Ocra: ocra on nero
