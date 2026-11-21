#' poissonCloud: Poisson models and a digital twin for cloud capacity planning
#'
#' The package follows one engineering story:
#' \enumerate{
#'   \item \strong{Baseline}: Poisson distribution ([poisson_pmf()], [required_capacity()]).
#'   \item \strong{Diagnosis}: why it fails on traffic ([poisson_dispersion_test()], [fit_traffic_model()]).
#'   \item \strong{Better models}: seasonal NB model ([fit_traffic_model()]) and Hawkes bursts ([fit_hawkes()]).
#'   \item \strong{Digital twin}: [simulate_autoscaling()] with boot delay, queue and SLO.
#'   \item \strong{Decisions}: [compare_policies()], [tune_policy()], [detect_anomalies()].
#'   \item \strong{Tools}: [run_app()] (Shiny dashboard) and [run_api()] (REST API).
#' }
#' @keywords internal
"_PACKAGE"
