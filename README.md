<p align="center">
  <picture>
    <source media="(prefers-color-scheme: dark)" srcset="./banner-dark.svg" />
    <img src="./banner-light.svg" width="100%" alt="alvise vitturi — formally verified observability in agda" />
  </picture>
</p>

<p align="center"><code>making invalid states unrepresentable — one dependently-typed observability tool at a time</code></p>

<p align="center">
  <img src="https://img.shields.io/badge/agda-2.8-d71921?style=flat-square&labelColor=141414" />
  <img src="https://img.shields.io/badge/type_theory-HoTT-d71921?style=flat-square&labelColor=141414" />
  <img src="https://img.shields.io/badge/nix-flakes-d71921?style=flat-square&labelColor=141414" />
  <img src="https://img.shields.io/badge/correct-by_construction-d71921?style=flat-square&labelColor=141414" />
</p>

---

### ● building

a **formally verified observability stack in agda** — metrics, logs, alerts and
dashboards that are *correct by construction*. no dsl accepts a malformed query, no
alert inverts a threshold, no dashboard panel renders empty: the type checker rejects
them before they ever leave the source file.

the bet — **type theory is the cheapest place to catch an ops outage.** a proof is a
test that covers *all* inputs, not a finite sample.

---

### ● constellation

a single ecosystem. each library proves one law, then composes.

```
observability
  01  prometea      semantic framework for prometheus metrics — meaning, no syntax
  02  henql         typed promql dsl — the type checker is your linter        ↳ prometea
  03  loqeul        total query algebra unifying logql & elastic, correct optimizer
  04  agdovana      formally verified prometheus alerts → yaml                ↳ prometea
  05  penelope      verifiable grafana dashboards, panels & layout proven     ↳ henql · loqeul

substrate
  06  janus         agda↔haskell ffi bridge — decode ∘ encode ≡ id, proven closed
  07  cardea        service config as a monotone map over a lattice of env-vars
  08  ibisfs        a filesystem as a lattice, not a hierarchy                ↳ janus
  09  gallicinium   server-side elm in agda — the model never leaves the server ↳ janus
  10  efesta        type-indexed, deferred-execution dagger sdk              ↳ janus
  11  dagger_gleam  the same idea in gleam — pipelines as typed data
  12  piforge       nix toolchains for agda, haskell, elm, gleam & latex — prebuilt
```

---

### ● writing

```
  copula              deriving HoTT's univalence from the meaning of judgement
  vfpia               "prova o taci" — a latex compendium to stump's vfp in agda
  avit-io.github.io   essays & notes, with live agda snippets
```

---

### ● stack

```
  languages    agda · haskell (ffi / malonzo) · gleam · racket
  foundations  type theory · HoTT / univalence · lattice & order theory
  tooling      nix / nixos · dagger.io · latex (classicthesis)
  domain       prometheus · loki · elastic · grafana — formally verified
```

---

### ● signs

<p align="center">
  <img src="https://github-readme-stats.vercel.app/api?username=avit-io&show_icons=true&hide_border=true&title_color=d71921&icon_color=d71921&text_color=888888&bg_color=00000000" alt="stats" />
</p>

<p align="center">
  <img src="https://github-readme-stats.vercel.app/api/top-langs/?username=avit-io&layout=compact&hide_border=true&title_color=d71921&text_color=888888&bg_color=00000000" alt="top languages" />
</p>

---

### ● connect

<p align="left">
  <a href="https://www.linkedin.com/in/alvisevitturi"><img src="https://img.shields.io/badge/linkedin-alvise_vitturi-d71921?style=flat-square&logo=linkedin&logoColor=white&labelColor=141414" /></a>
  <a href="mailto:avit-io@proton.me"><img src="https://img.shields.io/badge/email-avit--io-d71921?style=flat-square&logo=protonmail&logoColor=white&labelColor=141414" /></a>
  <a href="https://avit-io.github.io"><img src="https://img.shields.io/badge/web-avit--io.github.io-d71921?style=flat-square&logo=hugo&logoColor=white&labelColor=141414" /></a>
</p>

<sub><code>● making invalid states unrepresentable · certainty by construction</code></sub>
