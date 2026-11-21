# Architecture and how the website talks to R

```
                 +--------------------------+
 Browser  -----> | web/index.html (JS twin) |   mode A: standalone, no R needed
    |            +--------------------------+
    |  fetch()   +--------------------------+   mode B: JSON over HTTP
    +----------> | plumber API (run_api())  | ----> poissonCloud R functions
                 +--------------------------+
 Browser  -----> | Shiny app (run_app())    | ----> poissonCloud R functions   mode C: R renders the UI
                 +--------------------------+
```

| Mode | What runs the maths | When to use |
|---|---|---|
| A. Static page | JavaScript port of `poisson_pmf`, NB pmf, quantiles, queue simulation | public demo (GitHub Pages), no server |
| B. Page + API | R (`inst/plumber/api.R`) | prove that the web page and the R package agree: button **Ask R** |
| C. Shiny | R, in-process | full lab: autoscaling policies, anomaly detector, fitted model on the dataset |

## Mode B in detail
1. `Rscript -e 'poissonCloud::run_api()'` starts a server on `http://127.0.0.1:8000`.
2. The page calls `GET /risk?lambda=500&capacity=600&theta=43` with `fetch()`.
3. plumber runs `overload_probability()` and returns JSON `{"poisson":[...],"negbin":[...]}`; the CORS filter in `api.R` lets the browser read it.
4. The page prints R's value next to its own JavaScript value (they agree to ~1e-9).

## Why a JS twin at all?
A GitHub Pages site cannot run R. The JavaScript port is small (about 60 lines for the distributions) and is cross-checked against R/scipy; the Shiny app and API are the reference implementation.
