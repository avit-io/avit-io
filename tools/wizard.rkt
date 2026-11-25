#lang racket/base
;; The avatar: a wizard with a wand in one hand and, in the other, the eval/apply circle; drawn
;; by stippling like the banner, black dots on a white disc. The scene is a list of shapes,
;; painted in order: each gives the ink at a point, or #f where it is not.
;; racket tools/wizard.rkt   (writes wizard.svg)

(require racket/math racket/list)

(define S 400) (define C 200) (define frame 184)            ; the canvas and the disc

;; --- geometry ---

(define (inside-polygon? pts x y)                           ; even-odd rule
  (for/fold ([in #f]) ([a pts] [b (append (cdr pts) (list (car pts)))])
    (if (and (not (eq? (> (cdr a) y) (> (cdr b) y)))
             (< x (+ (car a) (/ (* (- (car b) (car a)) (- y (cdr a))) (- (cdr b) (cdr a))))))
        (not in)
        in)))

(define (segment-distance x y a b)
  (define-values (dx dy) (values (- (car b) (car a)) (- (cdr b) (cdr a))))
  (define t (max 0 (min 1 (/ (+ (* (- x (car a)) dx) (* (- y (cdr a)) dy)) (+ (sqr dx) (sqr dy))))))
  (sqrt (+ (sqr (- x (+ (car a) (* t dx)))) (sqr (- y (+ (cdr a) (* t dy)))))))

(define (edge-distance pts x y)
  (for/fold ([d +inf.0]) ([a pts] [b (append (cdr pts) (list (car pts)))]) (min d (segment-distance x y a b))))

(define (ellipse-r x y cx cy rx ry) (sqrt (+ (sqr (/ (- x cx) rx)) (sqr (/ (- y cy) ry)))))

;; --- shapes: x y → ink, or #f ---

;; A filled polygon with a dark outline; shade gives the ink inside.
(define ((polygon pts shade) x y)
  (cond [(< (edge-distance pts x y) 1.6) 0.95]
        [(inside-polygon? pts x y) (shade x y)]
        [else #f]))

(define ((ellipse cx cy rx ry shade) x y)
  (define r (ellipse-r x y cx cy rx ry))
  (cond [(< (abs (- r 1)) (/ 1.6 (min rx ry))) 0.95]
        [(< r 1) (shade x y)]
        [else #f]))

(define ((stroke a b width ink) x y) (and (< (segment-distance x y a b) width) ink))

;; An arc of the circle (cx cy r), from angle t0 to t1 (radians, clockwise on screen), with an
;; arrowhead at t1.
(define ((arrow cx cy r t0 t1) x y)
  (define d (sqrt (+ (sqr (- x cx)) (sqr (- y cy)))))
  (define t (let ([a (atan (- y cy) (- x cx))]) (if (< a t0) (+ a (* 2 pi)) a)))
  (define tip (cons (+ cx (* r (cos t1))) (+ cy (* r (sin t1)))))
  (define back (- t1 0.3))
  (define head (list tip
                     (cons (+ cx (* (+ r 7) (cos back))) (+ cy (* (+ r 7) (sin back))))
                     (cons (+ cx (* (- r 7) (cos back))) (+ cy (* (- r 7) (sin back))))))
  (cond [(inside-polygon? head x y) 1]
        [(and (< (abs (- d r)) 2.2) (<= t0 t (- t1 0.2))) 1]
        [else #f]))

;; --- light: from the wand, up on the right ---

(define (lit base x y) (max 0 (min 1 (+ base (* 0.18 (/ (- 230 x) 100)) (* 0.08 (/ (- y 200) 100))))))
(define (folds x y) (* 0.2 (sin (/ (- x (* 0.25 (- y 170))) 7))))

;; --- the wizard ---

(define robe '((158 . 168) (242 . 168) (262 . 250) (292 . 388) (108 . 388) (138 . 250)))
(define hat '((206 . 30) (186 . 52) (166 . 112) (246 . 112) (226 . 66)))
(define beard '((176 . 150) (224 . 150) (232 . 190) (216 . 238) (200 . 268) (184 . 238) (168 . 190)))
(define sleeve-left '((160 . 168) (184 . 200) (150 . 188) (122 . 174) (106 . 162) (126 . 138) (142 . 152)))   ; raised, holding the circle
(define sleeve-right '((238 . 170) (256 . 168) (276 . 222) (306 . 244) (286 . 268) (258 . 252) (246 . 214)))

(define orb-x 112) (define orb-y 102) (define orb-r 46)      ; the eval/apply circle

(define scene
  (list
   ;; the disc's frame: two rings
   (λ (x y) (let ([d (sqrt (+ (sqr (- x C)) (sqr (- y C))))])
              (and (or (< (abs (- d frame)) 2.2) (< (abs (- d (- frame 7))) 0.9)) 1)))
   ;; robe, sleeves, hands
   (polygon robe (λ (x y) (lit (+ 0.66 (folds x y)) x y)))
   (polygon sleeve-right (λ (x y) (lit (+ 0.72 (folds x y)) x y)))
   (polygon sleeve-left (λ (x y) (lit 0.8 x y)))
   (ellipse 116 150 9 8 (λ (x y) 0.12))
   (ellipse 290 254 8 9 (λ (x y) 0.12))
   ;; the wand, and its light
   (stroke '(276 . 318) '(322 . 128) 2.2 0.95)
   (λ (x y) (let ([d (sqrt (+ (sqr (- x 324)) (sqr (- y 122))))]
                  [a (atan (- y 122) (- x 324))])
              (and (< d 22)
                   (if (< d 3.5) 1 (and (< (abs (sin (* 4 a))) (* 0.35 (- 1 (/ d 22)))) 0.9)))))
   ;; head: face, beard, hat
   (ellipse 200 142 24 22 (λ (x y) (lit 0.08 x y)))
   (stroke '(186 . 133) '(197 . 136) 2.2 0.95)               ; brows, and eyes
   (stroke '(203 . 136) '(214 . 133) 2.2 0.95)
   (stroke '(191 . 141) '(195 . 141) 1.6 0.95)
   (stroke '(205 . 141) '(209 . 141) 1.6 0.95)
   (polygon beard (λ (x y) (+ 0.1 (* 0.1 (abs (sin (/ x 2.5)))))))
   (ellipse 200 149 4.5 7 (λ (x y) (lit 0.12 x y)))           ; the nose, over the moustache
   (polygon hat (λ (x y) (lit 0.85 x y)))
   (ellipse 206 112 62 10 (λ (x y) (lit 0.9 x y)))
   ;; the eval/apply circle: two arrows that chase each other
   (ellipse orb-x orb-y orb-r orb-r (λ (x y) 0))
   (arrow orb-x orb-y (- orb-r 13) (* -0.92 pi) (* -0.08 pi))
   (arrow orb-x orb-y (- orb-r 13) (* 0.08 pi) (* 0.92 pi))))

;; ink ∈ [0, 1] at a point: the first shape that covers it, painted last to first
(define (ink x y)
  (define d (sqrt (+ (sqr (- x C)) (sqr (- y C)))))
  (cond [(> d (+ frame 3)) #f]                               ; outside the disc: transparent
        [else (or (for/or ([s (reverse scene)]) (s x y)) 0)]))

;; Stippling, as in the banner: kept with probability ink, never too close.
(define cell 10)
(define (stipple n)
  (random-seed 1985)                                         ; SICP, first edition
  (define grid (make-hash))
  (define (key x y) (cons (quotient (exact-floor x) cell) (quotient (exact-floor y) cell)))
  (define (near? x y spacing)
    (for*/or ([i (in-range -1 2)] [j (in-range -1 2)])
      (define k (key x y))
      (for/or ([p (hash-ref grid (cons (+ (car k) i) (+ (cdr k) j)) '())])
        (< (+ (sqr (- x (car p))) (sqr (- y (cdr p)))) (sqr spacing)))))
  (for/fold ([dots '()]) ([_ n])
    (define x (* S (random))) (define y (* S (random)))
    (define b (or (ink x y) 0))
    (cond [(and (< (random) (expt b 0.8)) (not (near? x y (+ 2.7 (* 8 (expt (- 1 b) 1.5))))))
           (hash-update! grid (key x y) (λ (l) (cons (cons x y) l)) '())
           (cons (cons x y) dots)]
          [else dots])))

(define dots (reverse (stipple 700000)))

(with-output-to-file "wizard.svg" #:exists 'truncate
  (λ ()
    (printf "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 ~a ~a\" width=\"~a\" height=\"~a\" role=\"img\" aria-label=\"a wizard with a wand and the eval/apply circle\">\n" S S S S)
    (printf "<circle cx=\"~a\" cy=\"~a\" r=\"~a\" fill=\"#fff\"/>\n<g fill=\"#111\">\n" C C (+ frame 3))
    (for ([p dots])
      (printf "<circle cx=\"~a\" cy=\"~a\" r=\"1.5\"/>\n" (/ (round (* 10 (car p))) 10) (/ (round (* 10 (cdr p))) 10)))
    (printf "</g>\n<g fill=\"#111\" font-family=\"monospace\" font-size=\"11\" text-anchor=\"middle\">\n")
    (printf "<text x=\"~a\" y=\"~a\">eval</text>\n" orb-x (- orb-y 3))
    (printf "<text x=\"~a\" y=\"~a\">apply</text>\n" orb-x (+ orb-y 12))
    (printf "</g>\n</svg>\n")))
