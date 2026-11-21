# poissonCloud: a digital twin for cloud capacity planning

> **Question.** How many servers does a cloud API need to keep its SLA at minimum cost when traffic is random?
> **Answer.** Start from the Poisson law, show where it breaks (daily seasonality + bursts), fix it with
> a seasonal negative-binomial model, then test policies on a simulated autoscaling cluster.

## Headline results (synthetic dataset, 4 test days with 4 incidents)

| Policy | Cost ($) | SLO violations |
|---|---|---|
| Static peak (9 instances) | 86.4 | 0.101 % |
| Reactive (utilisation 80 %) | 54.6 | 0.116 % |
| Poisson predictive (best grid value) | 43.2 | 1.31 % |
| **Hybrid: NB forecast + surge guard** | **53.6 (-38 %)** | **0.064 %** |

* Poisson 99.9 % capacity planning actually covers only **83 %** of the 10-s windows (NB: 99.8 %).
* Seasonal NB: AIC 686,577 vs 1,226,175 for seasonal Poisson. Anomaly detection: 4/4 incidents, **0** false alarms/day (Poisson: 235-270/day).

## Install

```r
# from the repository root
install.packages("remotes")
remotes::install_local(".", dependencies = TRUE)      # or: remotes::install_github("your-user/poissonCloud")
library(poissonCloud)
run_app()      # Shiny dashboard
run_api()      # REST API on http://127.0.0.1:8000
```

## Repository layout (one repo for everything)

| Path | Content |
|---|---|
| `R/`, `DESCRIPTION`, `NAMESPACE`, `tests/`, `vignettes/` | the R package |
| `inst/extdata/` | data: `cloud_traffic_10s.csv.gz` (loaded by default), Excel files |
| `inst/app/` | Shiny dashboard (`poissonCloud::run_app()`) |
| `inst/plumber/` | REST API (`poissonCloud::run_api()`) |
| `web/` | standalone website (JavaScript twin of the core formulas) |
| `analysis/` | `run_analysis.R` reproduces every deck number; `results.json` = reference values |
| `slides/` | PowerPoint presentation|
| `docs/` | architecture, how the website talks to R, publication guide |

## Reproduce

```bash
Rscript analysis/run_analysis.R              # same numbers computed by the R package
Rscript -e 'devtools::document(); devtools::test(); devtools::check()'
```

All data are **simulated**; see `inst/extdata/cloud_traffic_10s.xlsx` (sheet `README`) for the generating process.
