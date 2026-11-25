#lang racket/base
;; The avatar: a wizard with a wand in one hand and the eval/apply circle in the other, standing
;; on a tiled floor in perspective; drawn by stippling, black dots on a white disc.
;; The scene is solid: cones, spheres and capsules, seen by a camera from three quarters and lit
;; by the star of the wand. Each pixel's light is found by ray marching (distance fields); the
;; dots follow the darkness, the outlines follow the edges between objects.
;; racket tools/wizard.rkt   (writes wizard.svg; deterministic)

(require racket/math racket/list)

(define S 400) (define C 200) (define frame 184)            ; the canvas and the disc
(define G 300)                                               ; the light is computed on a G×G grid

;; --- vectors ---

(define (v x y z) (vector x y z))
(define (vx a) (vector-ref a 0)) (define (vy a) (vector-ref a 1)) (define (vz a) (vector-ref a 2))
(define (v+ a b) (v (+ (vx a) (vx b)) (+ (vy a) (vy b)) (+ (vz a) (vz b))))
(define (v- a b) (v (- (vx a) (vx b)) (- (vy a) (vy b)) (- (vz a) (vz b))))
(define (v* a k) (v (* (vx a) k) (* (vy a) k) (* (vz a) k)))
(define (dot a b) (+ (* (vx a) (vx b)) (* (vy a) (vy b)) (* (vz a) (vz b))))
(define (len a) (sqrt (dot a a)))
(define (unit a) (v* a (/ 1.0 (len a))))
(define (cross a b) (v (- (* (vy a) (vz b)) (* (vz a) (vy b))) (- (* (vz a) (vx b)) (* (vx a) (vz b))) (- (* (vx a) (vy b)) (* (vy a) (vx b)))))

;; --- distance fields ---

(define (sphere p c r) (- (len (v- p c)) r))
(define (capsule p a b r)
  (define pa (v- p a)) (define ba (v- b a))
  (define h (max 0.0 (min 1.0 (/ (dot pa ba) (dot ba ba)))))
  (- (len (v- pa (v* ba h))) r))
(define (cone p y0 y1 r0 r1)                                 ; vertical, on the axis, radius r0 at y0, r1 at y1
  (define radial (sqrt (+ (sqr (vx p)) (sqr (vz p)))))
  (define t (max 0.0 (min 1.0 (/ (- (vy p) y0) (- y1 y0)))))
  (define side (* (- radial (+ r0 (* t (- r1 r0)))) (/ (- y1 y0) (sqrt (+ (sqr (- y1 y0)) (sqr (- r0 r1)))))))
  (max side (- y0 (vy p)) (- (vy p) y1)))
(define (tilted-cone p tip base r)                           ; from base (radius r) to tip, along any axis
  (define axis (v- tip base)) (define h (len axis)) (define u (v* axis (/ 1.0 h)))
  (define q (v- p base)) (define along (dot q u))
  (define radial (len (v- q (v* u along))))
  (define t (max 0.0 (min 1.0 (/ along h))))
  (max (* (- radial (* r (- 1 t))) (/ h (sqrt (+ (sqr h) (sqr r))))) (- along) (- along h)))

;; --- the scene: (distance . material) ---

(define wand-tip (v 1.02 2.62 0.42))
(define orb (v -1.0 2.86 0.32)) (define orb-r 0.36)

(define (scene p)
  (argmin car
          (list
           (cons (vy p) 'floor)
           (cons (cone p 0.0 1.85 0.95 0.36) 'robe)
           (cons (capsule p (v -0.3 1.75 0.05) (v -0.86 2.42 0.28) 0.15) 'robe)          ; the raised arm
           (cons (tilted-cone p (v -0.42 1.95 0.1) (v -0.84 2.38 0.27) 0.22) 'robe)      ; its sleeve, wide at the wrist
           (cons (capsule p (v 0.32 1.75 0.05) (v 0.78 1.32 0.38) 0.15) 'robe)           ; the arm with the wand
           (cons (tilted-cone p (v 0.42 1.62 0.18) (v 0.78 1.32 0.38) 0.22) 'robe)
           (cons (sphere p (v -0.9 2.5 0.3) 0.1) 'skin)                                   ; hands
           (cons (sphere p (v 0.82 1.3 0.42) 0.1) 'skin)
           (cons (capsule p (v 0.66 0.55 0.4) wand-tip 0.025) 'wand)
           (cons (sphere p (v 0 2.2 0.02) 0.27) 'skin)                                    ; head
           (cons (sphere p (v 0 2.13 0.26) 0.06) 'skin)                                   ; nose
           (cons (tilted-cone p (v 0 1.25 0.36) (v 0 2.08 0.16) 0.25) 'beard)
           (cons (cone p 2.36 2.4 0.62 0.62) 'hat)                                        ; brim
           (cons (tilted-cone p (v 0.12 3.45 -0.12) (v 0 2.38 0) 0.33) 'hat)
           (cons (sphere p orb orb-r) 'orb))))

(define (distance p) (car (scene p)))

(define (normal p)
  (define e 0.002)
  (unit (v (- (distance (v+ p (v e 0 0))) (distance (v- p (v e 0 0))))
           (- (distance (v+ p (v 0 e 0))) (distance (v- p (v 0 e 0))))
           (- (distance (v+ p (v 0 0 e))) (distance (v- p (v 0 0 e)))))))

(define (march o d)                                          ; → (t . material), or #f
  (let loop ([t 0.0] [i 0])
    (cond [(or (> i 160) (> t 30.0)) #f]
          [else (define h (scene (v+ o (v* d t))))
                (if (< (car h) 0.001) (cons t (cdr h)) (loop (+ t (* 0.8 (car h))) (+ i 1)))])))

(define (shadow p l)                                         ; soft shadow towards the point l, ∈ [0, 1]
  (define d (v- l p)) (define far (len d)) (define u (v* d (/ 1.0 far)))
  (let loop ([t 0.03] [k 1.0])
    (cond [(> t (- far 0.12)) k]
          [else (define h (distance (v+ p (v* u t))))
                (if (< h 0.0005) 0.0 (loop (+ t (max 0.01 h)) (min k (/ (* 10 h) t))))])))

;; --- the camera: three quarters, from a little above ---

(define eye (v 2.4 2.9 6.0)) (define target (v -0.05 1.55 0))
(define fwd (unit (v- target eye))) (define right (unit (cross fwd (v 0 1 0)))) (define up (cross right fwd))
(define zoom 2.45)

(define (ray sx sy)                                          ; screen coordinates in [-1, 1], y up
  (unit (v+ fwd (v* (v+ (v* right sx) (v* up sy)) (/ 1.0 zoom)))))

(define (project p)                                          ; → canvas (x . y)
  (define q (v- p eye)) (define z (dot q fwd))
  (cons (+ C (* C zoom (/ (dot q right) z))) (- C (* C zoom (/ (dot q up) z)))))

;; --- light ---

(define key (v -1.5 3.2 7.0))                                ; a soft light from the front left, for the form
(define albedo
  (hash 'robe 0.55 'hat 0.5 'skin 0.95 'beard 1.0 'wand 0.2 'orb 1.0 'floor-light 1.0 'floor-dark 0.12))

(define (floor-tile p)
  (cond [(> (sqrt (+ (sqr (vx p)) (sqr (vz p)))) 3.2) 'none] ; the floor ends: white paper beyond
        [(odd? (+ (exact-floor (/ (vx p) 0.6)) (exact-floor (/ (vz p) 0.6)))) 'floor-dark]
        [else 'floor-light]))

;; → (brightness ∈ [0, 1] . what was hit)
(define (shade sx sy)
  (define d (ray sx sy))
  (define hit (march eye d))
  (define p (and hit (v+ eye (v* d (car hit)))))
  (define m (and hit (if (eq? (cdr hit) 'floor) (floor-tile p) (cdr hit))))
  (cond
    [(or (not hit) (eq? m 'none)) (cons 1.0 'sky)]
    [else
     (define n (normal p))
     (define lit
       (for/sum ([l (list wand-tip key)] [power (list 1.5 0.7)])
         (define to (v- l p)) (define dist (len to))
         (define diffuse (max 0.0 (dot n (v* to (/ 1.0 dist)))))
         (define fall (if (eq? l wand-tip) (/ 1.0 (+ 1.0 (* 0.12 (sqr dist)))) 1.0))
         (* power diffuse fall (shadow (v+ p (v* n 0.004)) l))))
     (define rim (* 0.25 (expt (- 1 (max 0.0 (dot n (v* d -1.0)))) 3)))  ; a little light on the contours
     (cons (min 1.0 (* (hash-ref albedo m) (+ 0.2 lit rim))) (cdr hit))]))

;; --- the light, on a grid; the edges between objects ---

(define grid (make-vector (* G G)))
(for* ([j G] [i G])
  (define sx (- (* 2.0 (/ (+ i 0.5) G)) 1.0)) (define sy (- 1.0 (* 2.0 (/ (+ j 0.5) G))))
  (vector-set! grid (+ i (* j G)) (shade sx sy)))

(define (cell i j) (vector-ref grid (+ (max 0 (min (- G 1) i)) (* (max 0 (min (- G 1) j)) G))))
(define (edge? i j)                                          ; another object next to this pixel
  (define m (cdr (cell i j)))
  (and (not (memq m '(floor sky)))
       (for/or ([o '((1 . 0) (0 . 1) (-1 . 0) (0 . -1))])
         (not (eq? (cdr (cell (+ i (car o)) (+ j (cdr o)))) m)))))

;; ink ∈ [0, 1] at a canvas point, or #f outside the disc
(define (ink x y)
  (define d (sqrt (+ (sqr (- x C)) (sqr (- y C)))))
  (cond [(> d (+ frame 3)) #f]
        [(or (< (abs (- d frame)) 2.2) (< (abs (- d (- frame 7))) 0.9)) 1.0]       ; the frame
        [(> d (- frame 9)) 0.0]
        [else
         (define i (exact-floor (* G (/ x S)))) (define j (exact-floor (* G (/ y S))))
         (if (edge? i j) 0.95 (- 1.0 (car (cell i j))))]))

;; --- the eval/apply circle, drawn on the orb as it faces us: two arrows chasing each other ---

(define orb-c (project orb))
(define orb-pr (- (car (project (v+ orb (v* right orb-r)))) (car orb-c)))     ; its radius on the canvas

(define (inside-triangle? a b c x y)
  (define (s p q) (- (* (- (car q) (car p)) (- y (cdr p))) (* (- (cdr q) (cdr p)) (- x (car p)))))
  (define s1 (s a b)) (define s2 (s b c)) (define s3 (s c a))
  (or (and (>= s1 0) (>= s2 0) (>= s3 0)) (and (<= s1 0) (<= s2 0) (<= s3 0))))

(define (arrows x y)
  (define r (* 0.72 orb-pr))
  (define dx (- x (car orb-c))) (define dy (- y (cdr orb-c)))
  (define d (sqrt (+ (sqr dx) (sqr dy))))
  (define t (atan dy dx))
  (define (head t1)                                          ; a small triangle at angle t1, along the circle
    (define back (- t1 0.32))
    (inside-triangle? (cons (* r (cos t1)) (* r (sin t1)))
                      (cons (* (+ r 5) (cos back)) (* (+ r 5) (sin back)))
                      (cons (* (- r 5) (cos back)) (* (- r 5) (sin back)))
                      dx dy))
  (or (sparkle x y)
      (and (< (abs (- d r)) 1.6)
           (or (< (* -0.9 pi) t (* -0.18 pi)) (< (* 0.1 pi) t (* 0.82 pi))))
      (head (* -0.1 pi)) (head (* 0.9 pi))))

;; the star of the wand: a core and eight rays, where the light comes from
(define star (project wand-tip))
(define (sparkle x y)
  (define dx (- x (car star))) (define dy (- y (cdr star)))
  (define d (sqrt (+ (sqr dx) (sqr dy))))
  (and (< d 20) (or (< d 3) (< (abs (sin (* 4 (atan dy dx)))) (* 0.3 (- 1 (/ d 20)))))))

;; --- stippling, as in the banner ---

(define cellsize 10)
(define (stipple n)
  (random-seed 1985)                                         ; SICP, first edition
  (define near (make-hash))
  (define (key x y) (cons (quotient (exact-floor x) cellsize) (quotient (exact-floor y) cellsize)))
  (define (near? x y spacing)
    (for*/or ([i (in-range -1 2)] [j (in-range -1 2)])
      (define k (key x y))
      (for/or ([p (hash-ref near (cons (+ (car k) i) (+ (cdr k) j)) '())])
        (< (+ (sqr (- x (car p))) (sqr (- y (cdr p)))) (sqr spacing)))))
  (for/fold ([dots '()]) ([_ n])
    (define x (* S (random))) (define y (* S (random)))
    (define b (if (arrows x y) 1.0 (or (ink x y) 0.0)))
    (cond [(and (< (random) (expt b 0.8)) (not (near? x y (+ 2.4 (* 8 (expt (- 1 b) 1.5))))))
           (hash-update! near (key x y) (λ (l) (cons (cons x y) l)) '())
           (cons (cons x y) dots)]
          [else dots])))

(define dots (reverse (stipple 700000)))

(with-output-to-file "wizard.svg" #:exists 'truncate
  (λ ()
    (printf "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 ~a ~a\" width=\"~a\" height=\"~a\" role=\"img\" aria-label=\"a wizard with a wand and the eval/apply circle\">\n" S S S S)
    (printf "<circle cx=\"~a\" cy=\"~a\" r=\"~a\" fill=\"#fff\"/>\n<g fill=\"#111\">\n" C C (+ frame 3))
    (for ([p dots])
      (printf "<circle cx=\"~a\" cy=\"~a\" r=\"1.35\"/>\n" (/ (round (* 10 (car p))) 10) (/ (round (* 10 (cdr p))) 10)))
    (printf "</g>\n<g fill=\"#111\" font-family=\"monospace\" font-size=\"~a\" text-anchor=\"middle\">\n" (exact-round (* 0.36 orb-pr)))
    (printf "<text x=\"~a\" y=\"~a\">eval</text>\n" (exact-round (car orb-c)) (exact-round (- (cdr orb-c) (* 0.08 orb-pr))))
    (printf "<text x=\"~a\" y=\"~a\">apply</text>\n" (exact-round (car orb-c)) (exact-round (+ (cdr orb-c) (* 0.38 orb-pr))))
    (printf "</g>\n</svg>\n")))
