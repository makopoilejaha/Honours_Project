rm(list = ls())

library(circular)
library(ggplot2)
library(latex2exp)
library(grid)


data(fisherB8,fisherB18,fisherB13, package = "circular")

#convert observations to numeric values and then from degrees to radians
x <- as.numeric(unlist(fisherB8))
x <- x * pi / 180

x <- (2 * x) %% (2 * pi) #for axial data

#sample size
n <- length(x)

#observations exactly at the zero direction: for alpha < 1 the wNG2PLD density 
n_zero <- sum(x == 0)
if (n_zero > 0) message(n_zero, " observations at 0: wNG2PLD fit is effectively restricted to alpha >= 1.")


## ===================================================


lerch_phi <- function(z, s, a, terms = 5000, tol = 1e-12) {
  total <- 0
  k <- 0
  repeat {
    term <- z^k / (a + k)^s
    total <- total + term
    if (term < tol * total || k >= terms) break
    k <- k + 1
  }
  total
}


#### ---- Wrapped Exponential(theta) closed form -------------------

d_wexp <- function(phi, theta) {
  phi <- phi %% (2 * pi)
  theta * exp(-theta * phi) / (1 - exp(-2 * pi * theta))
}


#### ---- Wrapped Lindley(theta) closed form ------------------------

d_wlindley <- function(phi, theta) {
  phi <- phi %% (2 * pi)
  r <- exp(-2 * pi * theta)
  theta^2 / (theta + 1) *
    exp(-theta * phi) *
    ((1 + phi) / (1 - r) + (2 * pi * r) / (1 - r)^2)
}


#### ---- Wrapped XLindley(theta) closed form -----------------------


d_wxlindley <- function(phi, theta) {
  phi <- phi %% (2 * pi)
  r <- exp(-2 * pi * theta)
  theta^2 / (1 + theta)^2 *
    exp(-theta * phi) *
    ((2 + theta + phi) / (1 - r) + (2 * pi * r) / (1 - r)^2)
}


#### ---- Wrapped Quasi-Lindley(theta, alpha) -- closed form -----------


d_wql <- function(phi, theta, alpha) {
  phi <- phi %% (2 * pi)
  r <- exp(-2 * pi * theta)
  theta / (alpha + 1) *
    exp(-theta * phi) *
    ((alpha + theta * phi) / (1 - r) + (2 * pi * theta * r) / (1 - r)^2)
}


#### ---- Wrapped two-parameter Lindley(theta, a) -- closed form -------

d_w2pld <- function(phi, theta, a) {
  phi <- phi %% (2 * pi)
  r <- exp(-2 * pi * theta)
  theta^2 / (theta + a) *
    exp(-theta * phi) *
    ((1 + a * phi) / (1 - r) + (2 * pi * a * r) / (1 - r)^2)
}


#### ---- NG2PLD wrapped density -- Theorem 1 closed form --------------

d_wng2pld <- function(phi, theta, alpha) {
  phi <- phi %% (2 * pi)
  r <- exp(-2 * pi * theta)
  term1 <- 1 / (1 - r)
  term2 <- theta^(alpha - 2) * (2 * pi)^(alpha - 1) / gamma(alpha) *
    sapply(phi, function(l) lerch_phi(r, 1 - alpha, l / (2 * pi)))
  theta^2 / (theta + 1) * exp(-theta * phi) * (term1 + term2)
}


## =====================================================================
## Negative log-likelihoods for optim()
## =====================================================================

nll_wexp <- function(par, x) {
  theta <- par[1]
  if (!is.finite(theta) || theta <= 0) return(1e10)
  dens <- d_wexp(x, theta)
  if (any(!is.finite(dens)) || any(dens <= 0)) return(1e10)
  -sum(log(dens))
}

nll_wlindley <- function(par, x) {
  theta <- par[1]
  if (!is.finite(theta) || theta <= 0) return(1e10)
  dens <- d_wlindley(x, theta)
  if (any(!is.finite(dens)) || any(dens <= 0)) return(1e10)
  -sum(log(dens))
}

nll_wxlindley <- function(par, x) {
  theta <- par[1]
  if (!is.finite(theta) || theta <= 0) return(1e10)
  dens <- d_wxlindley(x, theta)
  if (any(!is.finite(dens)) || any(dens <= 0)) return(1e10)
  -sum(log(dens))
}

nll_wql <- function(par, x) {
  theta <- par[1]; alpha <- par[2]
  if (!is.finite(theta) || theta <= 0 || !is.finite(alpha) || alpha < 0) return(1e10)
  dens <- d_wql(x, theta, alpha)
  if (any(!is.finite(dens)) || any(dens <= 0)) return(1e10)
  -sum(log(dens))
}

nll_w2pld <- function(par, x) {
  theta <- par[1]; a <- par[2]
  if (!is.finite(theta) || theta <= 0 || !is.finite(a) || a < 0) return(1e10)
  dens <- d_w2pld(x, theta, a)
  if (any(!is.finite(dens)) || any(dens <= 0)) return(1e10)
  -sum(log(dens))
}

nll_wng2pld <- function(par, x) {
  theta <- par[1]; alpha <- par[2]
  if (!is.finite(theta) || theta <= 0 || !is.finite(alpha) || alpha <= 0) return(1e10)
  dens <- d_wng2pld(x, theta, alpha)
  if (any(!is.finite(dens)) || any(dens <= 0)) return(1e10)
  -sum(log(dens))
}


## =====================================================================
## MLE with optim(): multi-start for the two-parameter models
## =====================================================================

#run L-BFGS-B from every row of 'starts' and keep the highest maximum
fit_multi <- function(nll, starts, lower, upper, x, label) {
  starts <- as.matrix(starts)
  fits <- lapply(seq_len(nrow(starts)), function(i)
    optim(par = starts[i, ], fn = nll, x = x, method = "L-BFGS-B",
          lower = lower, upper = upper, control = list(maxit = 3000)))
  vals <- sapply(fits, `[[`, "value")
  fit <- fits[[which.min(vals)]]
  if (fit$convergence != 0) warning(label, ": no convergence.")
  if (fit$value >= 1e10) warning(label, ": best fit is on the penalty plateau.")
  ## how many starts reached the reported maximum (a crude check on multimodality)
  attr(fit, "n_hits") <- sum(abs(vals - fit$value) < 1e-4)
  fit
}

fit_wexp <- function(x) {
  fit <- fit_multi(nll_wexp, c(0.1, 1, 5), 0.0001, 20, x, "wexp")
  list(par = c(theta = fit$par[1], alpha = NA), logLik = -fit$value,
       k = 1, convergence = fit$convergence)
}

fit_wlindley <- function(x) {
  fit <- fit_multi(nll_wlindley, c(0.1, 1, 5), 0.0001, 20, x, "wlindley")
  list(par = c(theta = fit$par[1], alpha = NA), logLik = -fit$value,
       k = 1, convergence = fit$convergence)
}

fit_wxlindley <- function(x) {
  fit <- fit_multi(nll_wxlindley, c(0.1, 1, 5), 0.0001, 20, x, "wxlindley")
  list(par = c(theta = fit$par[1], alpha = NA), logLik = -fit$value,
       k = 1, convergence = fit$convergence)
}

#starts include the nested submodels: alpha = theta gives WL, large alpha approaches WE
fit_wqld <- function(x, th_we, th_wl) {
  starts <- rbind(c(1, 1), c(th_wl, th_wl), c(th_we, 20), c(th_we, 1),
                  expand.grid(c(0.1, 0.5, 2), c(0, 1, 10)))
  fit <- fit_multi(nll_wql, starts, c(0.0001, 0), c(20, 20), x, "wql")
  list(par = c(theta = unname(fit$par[1]), alpha = unname(fit$par[2])), logLik = -fit$value,
       k = 2, convergence = fit$convergence)
}

#starts include the nested submodels: a = 0 gives WE, a = 1 gives WL

fit_w2pld <- function(x, th_we, th_wl) {
  starts <- rbind(c(1, 1), c(th_we, 0), c(th_wl, 1),
                  expand.grid(c(0.1, 0.5, 2), c(0, 1, 10)))
  fit <- fit_multi(nll_w2pld, starts, c(0.0001, 0), c(20, 20), x, "w2pld")
  list(par = c(theta = unname(fit$par[1]), alpha = unname(fit$par[2])), logLik = -fit$value,
       k = 2, convergence = fit$convergence)
}

#the wNG2PLD likelihood is multimodal in (theta, alpha) because wrapping hides the winding number
fit_wng2pld <- function(x, th_we, th_wl, n_grid = 20, n_best = 10) {
  ## lower bound on theta is 0.01: below that r is so close to 1 that the Lerch series
  ## needs an impractical number of terms, and the density is essentially uniform
  lower <- c(0.01, 0.05); upper <- c(20, 30)
  th <- exp(seq(log(lower[1]), log(upper[1]), length.out = n_grid))
  al <- exp(seq(log(lower[2]), log(upper[2]), length.out = n_grid))
  g  <- expand.grid(theta = th, alpha = al)
  g$nll <- apply(g, 1, function(p) nll_wng2pld(p, x))
  starts <- rbind(as.matrix(g[order(g$nll)[1:n_best], 1:2]),
                  c(th_we, 1), c(th_wl, 2), c(1, 2))
  fit <- fit_multi(nll_wng2pld, starts, lower, upper, x, "wng2pld")
  list(par = c(theta = unname(fit$par[1]), alpha = unname(fit$par[2])), logLik = -fit$value,
       k = 2, convergence = fit$convergence, n_hits = attr(fit, "n_hits"))
}


## =====================================================================
## Fit all six models
## =====================================================================

fit_we   <- fit_wexp(x)
fit_wl   <- fit_wlindley(x)
fit_wxl  <- fit_wxlindley(x)
fit_wql  <- fit_wqld(x, fit_we$par["theta"], fit_wl$par["theta"])
fit_w2pl <- fit_w2pld(x, fit_we$par["theta"], fit_wl$par["theta"])
fit_wng  <- fit_wng2pld(x, fit_we$par["theta"], fit_wl$par["theta"])

#nesting check: a model can never have a lower maximised log-likelihood than its submodel
stopifnot(fit_w2pl$logLik >= max(fit_we$logLik, fit_wl$logLik) - 1e-6,
          fit_wng$logLik  >= max(fit_we$logLik, fit_wl$logLik) - 1e-6,
          fit_wql$logLik  >= fit_wl$logLik - 1e-6)

results <- data.frame(
  Dataset = "Outwash pebbles (B.8)",
  Model   = c("Wrapped Exponential", "Wrapped Lindley", "Wrapped XLindley",
              "Wrapped Quasi-Lindley", "Wrapped 2-par Lindley", "Wrapped NG2PLD"),
  n       = n,
  theta   = c(fit_we$par["theta"],  fit_wl$par["theta"],  fit_wxl$par["theta"],
              fit_wql$par["theta"], fit_w2pl$par["theta"], fit_wng$par["theta"]),
  alpha   = c(fit_we$par["alpha"],  fit_wl$par["alpha"],  fit_wxl$par["alpha"],
              fit_wql$par["alpha"], fit_w2pl$par["alpha"], fit_wng$par["alpha"]),
  LogLik  = c(fit_we$logLik, fit_wl$logLik, fit_wxl$logLik,
              fit_wql$logLik, fit_w2pl$logLik, fit_wng$logLik),
  k       = c(fit_we$k, fit_wl$k, fit_wxl$k, fit_wql$k, fit_w2pl$k, fit_wng$k)
)

results$AIC <- -2 * results$LogLik + 2 * results$k
results$BIC <- -2 * results$LogLik + log(n) * results$k
results <- results[order(results$AIC), ]

print(results, row.names = FALSE)


## =====================================================================
## check if each fitted density is valid (integrates to 1)
## =====================================================================

#tight tolerance so that the 8 printed decimals are meaningful
int1 <- function(f) round(integrate(f, lower = 0, upper = 2 * pi, rel.tol = 1e-10)$value, 8)

cat("\nIntegral of fitted density over [0, 2*pi):\n")
cat("  Wrapped Exponential:   ", int1(function(phi) d_wexp(phi, fit_we$par["theta"])), "\n")
cat("  Wrapped Lindley:       ", int1(function(phi) d_wlindley(phi, fit_wl$par["theta"])), "\n")
cat("  Wrapped XLindley:      ", int1(function(phi) d_wxlindley(phi, fit_wxl$par["theta"])), "\n")
cat("  Wrapped Quasi-Lindley: ", int1(function(phi) d_wql(phi, fit_wql$par["theta"], fit_wql$par["alpha"])), "\n")
cat("  Wrapped 2-par Lindley: ", int1(function(phi) d_w2pld(phi, fit_w2pl$par["theta"], fit_w2pl$par["alpha"])), "\n")
cat("  Wrapped NG2PLD:        ", int1(function(phi) d_wng2pld(phi, fit_wng$par["theta"], fit_wng$par["alpha"])), "\n")


## =====================================================================
## density plot over histogram of the data
## =====================================================================

phi_grid <- seq(0, 2 * pi, length.out = 1000)
phi_grid <- phi_grid[-length(phi_grid)]   #remove duplicated circular endpoint

dens_df <- rbind(
  data.frame(phi = phi_grid,
             density = d_wexp(phi_grid, fit_we$par["theta"]),
             Model = "Wrapped Exponential"),
  data.frame(phi = phi_grid,
             density = d_wlindley(phi_grid, fit_wl$par["theta"]),
             Model = "Wrapped Lindley"),
  data.frame(phi = phi_grid,
             density = d_wxlindley(phi_grid, fit_wxl$par["theta"]),
             Model = "Wrapped XLindley"),
  data.frame(phi = phi_grid,
             density = d_wql(phi_grid, fit_wql$par["theta"], fit_wql$par["alpha"]),
             Model = "Wrapped Quasi-Lindley"),
  data.frame(phi = phi_grid,
             density = d_w2pld(phi_grid, fit_w2pl$par["theta"], fit_w2pl$par["alpha"]),
             Model = "Wrapped 2-par Lindley"),
  data.frame(phi = phi_grid,
             density = d_wng2pld(phi_grid, fit_wng$par["theta"], fit_wng$par["alpha"]),
             Model = "Wrapped NG2PLD")
)

dens_df$Model <- factor(dens_df$Model,
                        levels = c("Wrapped NG2PLD", "Wrapped 2-par Lindley",
                                   "Wrapped Quasi-Lindley", "Wrapped Lindley",
                                   "Wrapped XLindley", "Wrapped Exponential"))

#axis labels
pi_breaks <- c(0, pi / 2, pi, 3 * pi / 2, 2 * pi)
pi_labels <- latex2exp::TeX(c("$0$", "$\\pi/2$", "$\\pi$", "$3\\pi/2$", "$2\\pi$"))



hist_breaks <- seq(0, 2 * pi, length.out = 10)

#histogram with all six fitted densities
plot_all <- ggplot() +
  
  geom_histogram(
    aes(x = x, y = after_stat(density)),
    breaks = hist_breaks,
    closed = "left",
    fill = "grey85",
    colour = "black"
  ) +
  
  geom_line(
    data = dens_df,
    aes(x = phi, y = density, colour = Model, linetype = Model),
    linewidth = 0.9
  ) +
  
  scale_x_continuous(
    breaks = pi_breaks,
    labels = pi_labels,
    limits = c(0, 2 * pi),
    expand = c(0, 0)
  ) +
  
  ## Okabe-Ito: colourblind-safe, distinguishable in greyscale print too
  scale_colour_manual(
    values = c("Wrapped NG2PLD"          = "#D55E00",
               "Wrapped 2-par Lindley"   = "#0072B2",
               "Wrapped Quasi-Lindley"   = "#009E73",
               "Wrapped Lindley"         = "#CC79A7",
               "Wrapped XLindley"        = "#E69F00",
               "Wrapped Exponential"     = "#000000")
  ) +
  
  scale_linetype_manual(
    values = c("solid", "dashed", "dotdash", "dotted", "longdash", "twodash")
  ) +
  
  theme_classic() +
  
  labs(
    x = expression(lambda),   #lambda = doubled axial angle; say so in the caption
    y = expression(g(lambda)),
    colour = NULL,
    linetype = NULL
  ) +
  
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.text = element_text(size = 11)
  )

#display plot
plot_all


#============================================================================

class_width    <- 2 * pi / 9
class_readings <- c("mid", "lower")

lo_ng <- c(0.01, 0.05); hi_ng <- c(20, 30)     # same search region as the fits


## ---- 1. Standard errors --------------------------------------------------

se_1par <- function(fit, nll) {
  sqrt(1 / optimHess(unname(fit$par["theta"]), nll, x = x)[1, 1])
}
V_ng    <- solve(optimHess(unname(fit_wng$par), nll_wng2pld, x = x))
se_ng   <- sqrt(diag(V_ng))
corr_ng <- cov2cor(V_ng)[1, 2]

results$se_theta <- NA; results$se_alpha <- NA
results$se_theta[results$Model == "Wrapped Exponential"] <- se_1par(fit_we,  nll_wexp)
results$se_theta[results$Model == "Wrapped Lindley"]     <- se_1par(fit_wl,  nll_wlindley)
results$se_theta[results$Model == "Wrapped XLindley"]    <- se_1par(fit_wxl, nll_wxlindley)
results$se_theta[results$Model == "Wrapped NG2PLD"]      <- se_ng[1]
results$se_alpha[results$Model == "Wrapped NG2PLD"]      <- se_ng[2]

cat("\nTable 4 with standard errors:\n")
print(results, row.names = FALSE, digits = 4)
cat(sprintf("Estimated corr(theta_hat, alpha_hat) for the wNG2PLD: %.2f\n", corr_ng))


## ---- 2. Wald and profile likelihood intervals ----------------------------
th_hat <- unname(fit_wng$par[1]); al_hat <- unname(fit_wng$par[2])
lhat   <- fit_wng$logLik

## profile in alpha: fix alpha, maximise over theta 

prof_alpha <- function(a) {
  -min(sapply(c(0.5, 1, 2) * th_hat, function(t0)
    optim(t0, function(t) nll_wng2pld(c(t, a), x), method = "L-BFGS-B",
          lower = lo_ng[1], upper = hi_ng[1])$value))
}
prof_theta <- function(t) {
  -min(sapply(pmin(c(0.5, 1, 2) * al_hat, 29), function(a0)
    optim(max(a0, 1), function(a) nll_wng2pld(c(t, a), x), method = "L-BFGS-B",
          lower = 1, upper = hi_ng[2])$value))
}
## alpha >= 1 in the profile: with observations at 0 the likelihood 

profile_ci <- function(pf, est, lower, upper, level) {
  cut <- qchisq(level, 1) / 2
  h <- function(v) pf(v) - lhat + cut
  c(if (h(lower) < 0) uniroot(h, c(lower, est), tol = 1e-4)$root else NA,
    if (h(upper) < 0) uniroot(h, c(est, upper), tol = 1e-4)$root else NA)
}

ci_tab <- data.frame(
  parameter = c("theta", "alpha"),
  estimate  = c(th_hat, al_hat),
  se        = se_ng,
  wald_lo   = c(th_hat, al_hat) - 1.96 * se_ng,
  wald_hi   = c(th_hat, al_hat) + 1.96 * se_ng)
p95 <- rbind(profile_ci(prof_theta, th_hat, 0.05, 10, 0.95),
             profile_ci(prof_alpha, al_hat, 1,  hi_ng[2], 0.95))
p99 <- rbind(profile_ci(prof_theta, th_hat, 0.05, 10, 0.99),
             profile_ci(prof_alpha, al_hat, 1,  hi_ng[2], 0.99))
ci_tab$prof95_lo <- p95[, 1]; ci_tab$prof95_hi <- p95[, 2]
ci_tab$prof99_lo <- p99[, 1]; ci_tab$prof99_hi <- p99[, 2]
cat("\nwNG2PLD interval estimates:\n")
print(ci_tab, row.names = FALSE, digits = 4)


## ---- 3. Likelihood ratio tests -------------------------------------------
lr_row <- function(label, ll_full, ll_we, ll_wl) {
  D_we <- 2 * (ll_full - ll_we); D_wl <- 2 * (ll_full - ll_wl)
  data.frame(likelihood = label,
             D_vs_WE = D_we, p_vs_WE = pchisq(D_we, 1, lower.tail = FALSE),
             D_vs_WL = D_wl, p_vs_WL = pchisq(D_wl, 1, lower.tail = FALSE))
}
lr_tab <- lr_row("pointwise", fit_wng$logLik, fit_we$logLik, fit_wl$logLik)

## grouped likelihood

G_ng <- function(lambda, theta, alpha, M = 3000) {      # equation (7)
  m <- 0:(M - 1)
  s <- sapply(lambda, function(l)
    sum(pgamma(2 * pi * theta * m, alpha, lower.tail = FALSE) -
          pgamma(theta * l + 2 * pi * theta * m, alpha, lower.tail = FALSE)))
  theta * (1 - exp(-theta * lambda)) / ((theta + 1) * (1 - exp(-2 * pi * theta))) +
    s / (theta + 1)
}
G_we <- function(lambda, theta) (1 - exp(-theta * lambda)) / (1 - exp(-2 * pi * theta))
G_wl <- function(lambda, theta)
  sapply(lambda, function(l) if (l <= 0) 0 else integrate(d_wlindley, 0, l, theta = theta)$value)

## probability of the arc [lo, hi); an arc with lo < 0 wraps through the origin
arc_prob <- function(G, lo, hi, ...) {
  ifelse(lo < 0, G(pmin(hi, 2 * pi), ...) + 1 - G(2 * pi + lo, ...),
         ifelse(hi >= 2 * pi, 1, G(pmin(hi, 2 * pi), ...)) - G(lo, ...))
}
grouped_nll <- function(G, par, cl) {
  pr <- do.call(arc_prob, c(list(G, cl$lo, cl$hi), as.list(par)))
  if (any(!is.finite(pr)) || any(pr <= 0)) return(1e10)
  -sum(cl$counts * log(pr))
}
make_classes <- function(reading) {
  tab <- table(round(x, 8)); v <- as.numeric(names(tab))
  lo  <- if (reading == "lower") v else v - class_width / 2
  list(lo = lo, hi = lo + class_width, counts = as.numeric(tab))
}

for (rd in class_readings) {
  cl <- make_classes(rd)
  ## wNG2PLD: multi-start, as for the pointwise fit (the grouped surface
  ## also has several local maxima, and here alpha < 1 is allowed)
  nll_g <- function(p) grouped_nll(G_ng, p, cl)
  gr <- expand.grid(theta = exp(seq(log(0.02), log(10), length.out = 10)),
                    alpha = exp(seq(log(0.06), log(29), length.out = 10)))
  gr$v <- apply(gr, 1, function(p) nll_g(p[1:2]))
  st <- as.matrix(gr[order(gr$v)[1:6], 1:2])
  g_fits <- lapply(seq_len(nrow(st)), function(i)
    optim(st[i, ], nll_g, method = "L-BFGS-B", lower = lo_ng, upper = hi_ng))
  g_ng <- g_fits[[which.min(sapply(g_fits, `[[`, "value"))]]
  g_1 <- function(G) optim(0.3, function(t) grouped_nll(G, t, cl), method = "L-BFGS-B",
                           lower = 0.0001, upper = 20)
  g_we <- g_1(G_we); g_wl <- g_1(G_wl)
  cat(sprintf("Grouped (%s): wNG2PLD theta = %.4f, alpha = %.3f, loglik = %.3f\n",
              rd, g_ng$par[1], g_ng$par[2], -g_ng$value))
  lr_tab <- rbind(lr_tab, lr_row(paste0("grouped, ", rd), -g_ng$value, -g_we$value, -g_wl$value))
}
cat("\nLikelihood ratio tests (reference distribution chi-squared, 1 df):\n")
print(lr_tab, row.names = FALSE, digits = 4)


## ---- 4. Profile likelihood plot for alpha ---------------------------------
a_grid <- sort(unique(c(seq(1, al_hat, length.out = 30),
                        seq(al_hat, hi_ng[2], length.out = 60))))
prof_df <- data.frame(alpha = a_grid, rel = sapply(a_grid, prof_alpha) - lhat)

plot_prof <- ggplot(prof_df, aes(x = alpha, y = rel)) +
  geom_line(linewidth = 0.9) +
  geom_hline(yintercept = -qchisq(c(0.95, 0.99), 1) / 2,
             linetype = c("dashed", "dotted")) +
  theme_classic() +
  labs(x = expression(alpha), y = "Relative profile log likelihood") +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 13))
plot_prof
ggsave("PLpebbls.pdf", plot_prof, width = 6, height = 4.5)
