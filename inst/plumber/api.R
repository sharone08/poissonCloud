# REST API for poissonCloud. Run with poissonCloud::run_api()
library(poissonCloud)

#* Allow browser front-ends (CORS)
#* @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  if (req$REQUEST_METHOD == "OPTIONS") { res$status <- 200; return(list()) }
  plumber::forward()
}

#* Health check
#* @get /health
function() list(status = "ok")

#* Overload probability under Poisson and negative-binomial traffic
#* @param lambda Mean requests per window
#* @param capacity Capacity per window
#* @param theta NB size (smaller = burstier)
#* @get /risk
function(lambda = 400, capacity = 450, theta = 40) {
  l <- as.numeric(lambda); c0 <- as.numeric(capacity); th <- as.numeric(theta)
  list(poisson = overload_probability(l, c0), negbin = overload_probability(l, c0, theta = th),
       ratio = overload_probability(l, c0, theta = th) / max(overload_probability(l, c0), 1e-300))
}

#* Capacity needed for an SLA under Poisson and negative-binomial traffic
#* @param lambda Mean requests per window
#* @param sla Share of windows to absorb
#* @param theta NB size
#* @get /capacity
function(lambda = 400, sla = 0.999, theta = 40) {
  l <- as.numeric(lambda); s <- as.numeric(sla); th <- as.numeric(theta)
  list(poisson = required_capacity(l, s), negbin = required_capacity(l, s, theta = th))
}
