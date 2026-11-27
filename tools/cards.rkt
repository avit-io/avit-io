#lang racket/base
;; The profile's sections as Ocra cards: GitHub takes no CSS, so ochre reaches the README as
;; images. The content is data below; each card is written twice, nero on ochre and ochre on nero,
;; squared (what you read), with a stippled ball in the corner, lit like the bottega's sphere.
;; racket tools/cards.rkt   (writes cards/<name>-light.svg and cards/<name>-dark.svg)

(require racket/string racket/list racket/math "puntini.rkt")

;; (name title subtitle (group (idx name desc [dep]) …) …)
(define cards
  '((constellation "constellation" "a single ecosystem. each library proves one law, then composes."
     ("agda · observability"
      ("01" "prometea" "semantic framework for prometheus metrics, meaning without syntax")
      ("02" "henql" "typed promql dsl: the type checker is your linter" "prometea")
      ("03" "loqeul" "total query algebra unifying logql & elastic, correct optimizer")
      ("04" "agdovana" "formally verified prometheus alerts → yaml" "prometea")
      ("05" "penelope" "verifiable grafana dashboards, panels & layout proven" "henql · loqeul"))
     ("racket · bottega"
      ("06" "bottega" "a total mltt core in racket, pie-style: Π, Σ, ≡, refl by computation")
      ("07" "disegno" "#lang disegno: dagger pipelines declared, not run" "bottega")
      ("08" "prospettiva" "#lang prospettiva: one model, many environments, nix" "bottega")
      ("09" "affresco" "#lang affresco: the elm architecture, total, server-side" "bottega")
      ("10" "puntini" "a stippling engine after sicp's picture language: these images"))
     ("agda · substrate"
      ("11" "janus" "agda↔haskell ffi bridge: decode ∘ encode ≡ id, proven closed")
      ("12" "cardea" "service config as a monotone map over a lattice of env-vars")
      ("13" "ibisfs" "a filesystem as a lattice, not a hierarchy" "janus")
      ("14" "gallicinium" "server-side elm in agda: the model never leaves the server" "janus")
      ("15" "efesta" "type-indexed, deferred-execution dagger sdk" "janus")
      ("16" "dagger_gleam" "the same idea in gleam: pipelines as typed data")
      ("17" "piforge" "nix toolchains for agda, haskell, elm, gleam & latex, prebuilt")))
    (writing "writing" "essays, a compendium, and a blog with live agda and racket."
     ("words"
      ("·" "copula" "deriving HoTT's univalence from the meaning of judgement")
      ("·" "vfpia" "\"prova o taci\": a latex compendium to stump's vfp in agda")
      ("·" "blog" "avit-io.github.io: essays, eval/apply, bottega")))
    (stack "stack" "what the work is made of."
     ("tools"
      ("·" "languages" "agda · racket / scheme · haskell (ffi / malonzo) · gleam")
      ("·" "foundations" "type theory · HoTT / univalence · sicp · lattices & orders")
      ("·" "tooling" "nix / nixos · dagger.io · latex (classicthesis)")
      ("·" "domain" "prometheus · loki · elastic · grafana, formally verified")))))

;; Ocra: paper, ink, the names, the muted ink
(define themes
  '((light "#efe0b4" "#15130f" "#15130f" "#594c33")
    (dark  "#15130f" "#ebddb2" "#d6b866" "#a99b78")))

(define W 880) (define pad 32) (define row 26)
(define mono "'IBM Plex Mono', ui-monospace, SFMono-Regular, Menlo, Consolas, monospace")
(define serif "Alegreya, Charter, 'Iowan Old Style', Georgia, serif")

(define (esc s) (string-replace (string-replace (string-replace s "&" "&amp;") "<" "&lt;") ">" "&gt;"))

(define (card-height groups) (+ pad 70 (for/sum ([g groups]) (+ 44 (* row (length (cdr g))))) pad -10))

;; a lit ball in the top right corner, the same dots in both themes
(define (ball h)
  (define r 30) (define cx (- W pad r)) (define cy (+ pad r -8))
  (stipple (λ (u v) ((sphere (/ cx W) (/ cy W) (/ r W)) u (* v (/ h W)))) W h #:n 60000 #:seed 1985 #:spacing 1.5 #:spread 5))

(define (card name title subtitle . groups)
  (define h (card-height groups))
  (define dots (ball h))
  (for ([t themes])
    (define-values (paper ink names muted) (apply values (cdr t)))
    (with-output-to-file (format "cards/~a-~a.svg" name (car t)) #:exists 'truncate
      (λ ()
        (printf "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 ~a ~a\" width=\"~a\" height=\"~a\" role=\"img\" aria-label=\"~a\">\n" W h W h (esc title))
        (printf "<rect width=\"100%\" height=\"100%\" fill=\"~a\"/>\n<g fill=\"~a\">\n" paper ink)
        (for ([p dots]) (printf "<circle cx=\"~a\" cy=\"~a\" r=\"0.9\"/>\n" (/ (round (* 10 (car p))) 10) (/ (round (* 10 (cdr p))) 10)))
        (printf "</g>\n")
        (printf "<text x=\"~a\" y=\"~a\" fill=\"~a\" font-family=\"~a\" font-size=\"12\" letter-spacing=\"2.4\">~a</text>\n" pad (+ pad 12) muted mono (string-upcase title))
        (printf "<text x=\"~a\" y=\"~a\" fill=\"~a\" font-family=\"~a\" font-size=\"19\" font-style=\"italic\">~a</text>\n" pad (+ pad 44) ink serif (esc subtitle))
        (for/fold ([y (+ pad 82)]) ([g groups])
          (printf "<text x=\"~a\" y=\"~a\" fill=\"~a\" font-family=\"~a\" font-size=\"11\" letter-spacing=\"2\">~a</text>\n" pad y muted mono (esc (string-upcase (car g))))
          (printf "<rect x=\"~a\" y=\"~a\" width=\"~a\" height=\"1\" fill=\"~a\"/>\n" pad (+ y 8) (- W pad pad) ink)
          (for ([r (cdr g)] [i (in-naturals)])
            (define ry (+ y 34 (* i row)))
            (printf "<g font-family=\"~a\" font-size=\"13.5\"><text x=\"~a\" y=\"~a\" fill=\"~a\">~a</text><text x=\"~a\" y=\"~a\" fill=\"~a\" font-weight=\"600\">~a</text><text x=\"~a\" y=\"~a\" fill=\"~a\">~a~a</text></g>\n"
                    mono pad ry muted (esc (first r)) (+ pad 34) ry names (esc (second r)) (+ pad 160) ry ink (esc (third r))
                    (if (= (length r) 4) (format "<tspan dx=\"12\" fill=\"~a\">↳ ~a</tspan>" muted (esc (fourth r))) "")))
          (+ y 44 (* row (length (cdr g)))))
        (printf "</svg>\n")))))

(for ([c cards]) (apply card c))
