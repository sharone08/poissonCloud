# Shiny dashboard: digital twin of an autoscaling cloud API
library(shiny)
library(poissonCloud)

d <- load_traffic()
train <- which(d$day <= 7); test <- which(d$day >= 11)
model <- fit_traffic_model(d$requests[train], d$timestamp[train])
mu <- predict(model, d$timestamp)

ui <- navbarPage("poissonCloud | cloud traffic digital twin",
  tabPanel("1. Risk calculator",
    sidebarLayout(
      sidebarPanel(
        sliderInput("lam", "Mean requests per window", 50, 3000, 500, step = 10),
        sliderInput("capx", "Capacity (x mean)", 1.0, 2.0, 1.25, step = 0.01),
        sliderInput("theta", "NB size theta (large = Poisson, small = bursty)", 5, 500, round(model$theta)),
        sliderInput("sla", "SLA quantile", 0.9, 0.99999, 0.999, step = 0.0001)),
      mainPanel(plotOutput("pmf"), tableOutput("risk")))),
  tabPanel("2. Autoscaling lab",
    sidebarLayout(
      sidebarPanel(
        selectInput("pol", "Policy", c("Static peak" = "static", "Reactive" = "reactive", "Poisson predictive" = "poisson",
                                       "NB predictive" = "nb", "Hybrid (NB + surge guard)" = "hybrid"), "hybrid"),
        sliderInput("par", "Parameter (utilisation or quantile level)", 0.3, 0.99999, 0.999, step = 0.0001),
        sliderInput("delay", "Instance boot delay (s)", 10, 300, 60, step = 10),
        sliderInput("day", "Test day", 11, 14, 11, step = 1)),
      mainPanel(plotOutput("twin"), tableOutput("kpi")))),
  tabPanel("3. Anomaly detector",
    sidebarLayout(
      sidebarPanel(
        sliderInput("alpha", "log10 alpha (per tail)", -9, -2, -4, step = 1),
        sliderInput("run", "Consecutive bins for an alarm", 1, 6, 3, step = 1),
        radioButtons("mod", "Reference model", c("Seasonal NB" = "nb", "Seasonal Poisson" = "poisson")),
        sliderInput("aday", "Test day", 11, 14, 12, step = 1)),
      mainPanel(plotOutput("anom"), textOutput("anom_txt")))))

server <- function(input, output) {
  output$pmf <- renderPlot({
    plot_pmf(input$lam, capacity = input$lam * input$capx, theta = input$theta,
             main = "Poisson (bars) vs bursty NB (line); red = overload zone")
  })
  output$risk <- renderTable({
    cap <- input$lam * input$capx
    data.frame(Model = c("Poisson", "Negative binomial"),
               `Overload probability` = c(overload_probability(input$lam, cap), overload_probability(input$lam, cap, input$theta)),
               `Capacity for SLA` = c(required_capacity(input$lam, input$sla), required_capacity(input$lam, input$sla, input$theta)),
               check.names = FALSE)
  }, digits = 6)
  sim <- reactive({
    idx <- which(d$day == input$day); y <- d$requests[idx]; m <- mu[idx]; dl <- input$delay / 10
    inst <- switch(input$pol,
      static = policy_static(length(idx), ceiling(quantile(d$requests[train], 0.999) / 150)),
      reactive = policy_reactive(y, input$par, delay_bins = dl),
      poisson = policy_predictive(qpois(input$par, m)),
      nb = policy_predictive(qnbinom(input$par, size = model$theta, mu = m)),
      hybrid = policy_hybrid(y, m, model$theta, input$par, delay_bins = dl))
    list(idx = idx, y = y, inst = inst, res = simulate_autoscaling(y, inst))
  })
  output$twin <- renderPlot({
    s <- sim(); h <- as.numeric(format(d$timestamp[s$idx], "%H")) + as.numeric(format(d$timestamp[s$idx], "%M")) / 60
    par(mar = c(4, 4, 2, 4))
    plot(h, s$y / 10, type = "l", col = "#BFD7EA", xlab = "Hour", ylab = "Requests/s", main = "Demand vs capacity")
    lines(h, s$inst * 15, col = "#2A9D8F", lwd = 2)
    points(h[s$res$violation], s$y[s$res$violation] / 10, col = "#E63946", pch = 19, cex = 0.6)
    legend("topleft", bty = "n", lty = 1, col = c("#BFD7EA", "#2A9D8F"), legend = c("demand", "capacity"))
  })
  output$kpi <- renderTable({
    r <- sim()$res
    data.frame(`Cost ($)` = r$cost, `SLO violation rate` = r$violation_rate, `Dropped share` = r$dropped_share,
               `Avg instances` = r$avg_instances, check.names = FALSE)
  }, digits = 5)
  an <- reactive({
    idx <- which(d$day == input$aday); m <- mu[idx]
    th <- if (input$mod == "nb") model$theta else Inf
    list(idx = idx, res = detect_anomalies(d$requests[idx], m, th, 10^input$alpha, input$run))
  })
  output$anom <- renderPlot({
    a <- an(); r <- a$res; h <- seq_along(a$idx) / 360
    plot(h, r$count / 10, type = "l", col = "#5F6F82", xlab = "Hour", ylab = "Requests/s", main = "Alarms (red)")
    lines(h, r$expected / 10, col = "#2A9D8F", lwd = 2)
    points(h[r$alarm], r$count[r$alarm] / 10, col = "#E63946", pch = 19, cex = 0.7)
  })
  output$anom_txt <- renderText({
    a <- an(); lab <- if ("is_incident" %in% names(d)) sum(d$is_incident[a$idx]) else NA
    sprintf("%d alarm bins | %d truly anomalous bins in the labels", sum(a$res$alarm), lab)
  })
}

shinyApp(ui, server)
