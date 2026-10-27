#data preparation
dat_trans <- read.csv("transactions.csv", header = TRUE, sep = ",")
dat_trans_t1 <- dat_trans[dat_trans$type == 1, ]
dat_trans_t2 <- dat_trans[dat_trans$type == 2, ]
dat_trans_u1 <- dat_trans[dat_trans$user == 1, ]
dat_trans_u2 <- dat_trans[dat_trans$user == 2, ]

library(dplyr)
library(lubridate)

dfu1 <- data.frame(datetime = dat_trans_u1$date)
dfu2 <- data.frame(datetime = dat_trans_u2$date)
dft1 <- data.frame(datetime = dat_trans_t1$date)
dft2 <- data.frame(datetime = dat_trans_t2$date)


dfu1 <- dfu1 %>%
  mutate(
    datetime_parsed = ymd_hm(datetime),
    hour_only = floor_date(datetime_parsed, unit = "hour"),
    time_only = format(hour_only, "%H:%M")
  )
df_hour_counts_u1 <- dfu1 %>%
  count(time_only) %>%
  arrange(time_only)
df_hour_counts_u1 <- as.matrix(df_hour_counts_u1)


dfu2 <- dfu2 %>%
  mutate(
    datetime_parsed = ymd_hm(datetime),
    hour_only = floor_date(datetime_parsed, unit = "hour"),
    time_only = format(hour_only, "%H:%M")
  )
df_hour_counts_u2 <- dfu2 %>%
  count(time_only) %>%
  arrange(time_only)
df_hour_counts_u2 <- as.matrix(df_hour_counts_u2)


dft1 <- dft1 %>%
  mutate(
    datetime_parsed = ymd_hm(datetime),
    hour_only = floor_date(datetime_parsed, unit = "hour"),
    time_only = format(hour_only, "%H:%M")
  )
df_hour_counts_t1 <- dft1 %>%
  count(time_only) %>%
  arrange(time_only)
df_hour_counts_t1 <- as.matrix(df_hour_counts_t1)


dft2 <- dft2 %>%
  mutate(
    datetime_parsed = ymd_hm(datetime),
    hour_only = floor_date(datetime_parsed, unit = "hour"),
    time_only = format(hour_only, "%H:%M")
  )
df_hour_counts_t2 <- dft2 %>%
  count(time_only) %>%
  arrange(time_only)
df_hour_counts_t2 <- as.matrix(df_hour_counts_t2)


yu1=rep(c(0,15,30,45,60,75,90,105,120,135,150,165,180,195,210,225,240,255,270,285,300,315,330,345),
        c(df_hour_counts_u1[18,2],df_hour_counts_u1[19,2],df_hour_counts_u1[20,2],df_hour_counts_u1[21,2],df_hour_counts_u1[22,2],
          df_hour_counts_u1[1,2],df_hour_counts_u1[2,2],df_hour_counts_u1[3,2],df_hour_counts_u1[4,2],
          df_hour_counts_u1[5,2],df_hour_counts_u1[6,2],df_hour_counts_u1[7,2],df_hour_counts_u1[8,2],df_hour_counts_u1[9,2],
          df_hour_counts_u1[10,2],df_hour_counts_u1[11,2],0,df_hour_counts_u1[12,2],df_hour_counts_u1[13,2],0,
          df_hour_counts_u1[14,2],df_hour_counts_u1[15,2],df_hour_counts_u1[16,2],df_hour_counts_u1[17,2]))

yu2=rep(c(0,15,30,45,60,75,90,105,120,135,150,165,180,195,210,225,240,255,270,285,300,315,330,345),
        c(df_hour_counts_u2[16,2],df_hour_counts_u2[17,2],df_hour_counts_u2[18,2],df_hour_counts_u2[19,2],df_hour_counts_u2[20,2],
          df_hour_counts_u2[1,2],df_hour_counts_u2[2,2],df_hour_counts_u2[3,2],df_hour_counts_u2[4,2],
          df_hour_counts_u2[5,2],df_hour_counts_u2[6,2],df_hour_counts_u2[7,2],0,0,df_hour_counts_u2[8,2],
          df_hour_counts_u2[9,2],df_hour_counts_u2[10,2],0,0,df_hour_counts_u2[11,2],df_hour_counts_u2[12,2],
          df_hour_counts_u2[13,2],df_hour_counts_u2[14,2],df_hour_counts_u2[15,2]))

yt1=rep(c(0,15,30,45,60,75,90,105,120,135,150,165,180,195,210,225,240,255,270,285,300,315,330,345),
        c(df_hour_counts_t1[19,2],df_hour_counts_t1[20,2],df_hour_counts_t1[21,2],df_hour_counts_t1[22,2],df_hour_counts_t1[23,2],
          df_hour_counts_t1[1,2],df_hour_counts_t1[2,2],df_hour_counts_t1[3,2],df_hour_counts_t1[4,2],
          df_hour_counts_t1[5,2],df_hour_counts_t1[6,2],df_hour_counts_t1[7,2],df_hour_counts_t1[8,2],df_hour_counts_t1[9,2],
          df_hour_counts_t1[10,2],df_hour_counts_t1[11,2],df_hour_counts_t1[12,2],df_hour_counts_t1[13,2],
          df_hour_counts_t1[14,2],0,df_hour_counts_t1[15,2],df_hour_counts_t1[16,2],df_hour_counts_t1[17,2],
          df_hour_counts_t1[18,2]))

yt2=rep(c(0,15,30,45,60,75,90,105,120,135,150,165,180,195,210,225,240,255,270,285,300,315,330,345),
        c(df_hour_counts_t2[13,2],df_hour_counts_t2[14,2],df_hour_counts_t2[15,2],df_hour_counts_t2[16,2],df_hour_counts_t2[17,2],
          df_hour_counts_t2[1,2],df_hour_counts_t2[2,2],df_hour_counts_t2[3,2],df_hour_counts_t2[4,2],
          df_hour_counts_t2[5,2],df_hour_counts_t2[6,2],df_hour_counts_t2[7,2],0,0,df_hour_counts_t2[8,2],
          0,0,0,0,df_hour_counts_t2[9,2],df_hour_counts_t2[10,2],0,df_hour_counts_t2[11,2],
          df_hour_counts_t2[12,2]))

#dfeining variable
x_deg <- yt2
x <- x_deg * pi / 180

#sample size
n <- length(x)


## ----------------------------------------------------------------------------
## Lerch transcendent, Phi(z, s, a) = sum_{k=0}^Inf z^k / (a+k)^s

#------------------------------------------------------------------------------
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


#### ---- Wrapped XLindley(theta)  closed form -----------------------


d_wxlindley <- function(phi, theta) {
  phi <- phi %% (2 * pi)
  r <- exp(-2 * pi * theta)
  theta^2 / (1 + theta)^2 *
    exp(-theta * phi) *
    ((2 + theta + phi) / (1 - r) + (2 * pi * r) / (1 - r)^2)
}


#### ---- Wrapped Quasi-Lindley(theta, alpha)  closed form -----------

d_wql <- function(phi, theta, alpha) {
  phi <- phi %% (2 * pi)
  r <- exp(-2 * pi * theta)
  theta / (alpha + 1) *
    exp(-theta * phi) *
    ((alpha + theta * phi) / (1 - r) + (2 * pi * theta * r) / (1 - r)^2)
}


#### ---- Wrapped two-parameter Lindley(theta, a)  closed form -------

d_w2pld <- function(phi, theta, a) {
  phi <- phi %% (2 * pi)
  r <- exp(-2 * pi * theta)
  theta^2 / (theta + a) *
    exp(-theta * phi) *
    ((1 + a * phi) / (1 - r) + (2 * pi * a * r) / (1 - r)^2)
}


#### ---- NG2PLD wrapped density (from commutativity)--------------

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
## MLE with optim(), one fit function per model
## =====================================================================

fit_wexp <- function(x) {
  fit <- optim(par = 1, fn = nll_wexp, x = x, method = "L-BFGS-B",
               lower = 0.0001, upper = 20, control = list(maxit = 3000))
  if (fit$convergence != 0) warning("wexp: no convergence.")
  list(par = c(theta = fit$par[1], alpha = NA), logLik = -fit$value,
       k = 1, convergence = fit$convergence)
}

fit_wlindley <- function(x) {
  fit <- optim(par = 1, fn = nll_wlindley, x = x, method = "L-BFGS-B",
               lower = 0.0001, upper = 20, control = list(maxit = 3000))
  if (fit$convergence != 0) warning("wlindley: no convergence.")
  list(par = c(theta = fit$par[1], alpha = NA), logLik = -fit$value,
       k = 1, convergence = fit$convergence)
}

fit_wxlindley <- function(x) {
  fit <- optim(par = 1, fn = nll_wxlindley, x = x, method = "L-BFGS-B",
               lower = 0.0001, upper = 20, control = list(maxit = 3000))
  if (fit$convergence != 0) warning("wxlindley: no convergence.")
  list(par = c(theta = fit$par[1], alpha = NA), logLik = -fit$value,
       k = 1, convergence = fit$convergence)
}

fit_wqld <- function(x) {
  fit <- optim(par = c(1, 1), fn = nll_wql, x = x, method = "L-BFGS-B",
               lower = c(0.0001, 0), upper = c(20, 20),
               control = list(maxit = 3000))
  if (fit$convergence != 0) warning("wql: no convergence.")
  list(par = c(theta = fit$par[1], alpha = fit$par[2]), logLik = -fit$value,
       k = 2, convergence = fit$convergence)
}

fit_w2pld <- function(x) {
  fit <- optim(par = c(1, 1), fn = nll_w2pld, x = x, method = "L-BFGS-B",
               lower = c(0.0001, 0.0001), upper = c(20, 20),
               control = list(maxit = 3000))
  if (fit$convergence != 0) warning("w2pld: no convergence.")
  list(par = c(theta = fit$par[1], alpha = fit$par[2]), logLik = -fit$value,
       k = 2, convergence = fit$convergence)
}

fit_wng2pld <- function(x) {
  ## lower bound on theta raised from 0.0001 to 0.01: below that, r is so
  ## close to 1 that the Lerch series needs an impractical number of terms
  ## to converge, and the density there is indistinguishable from uniform
  ## in any case -- nothing real is lost by excluding it.
  fit <- optim(par = c(1, 2), fn = nll_wng2pld, x = x, method = "L-BFGS-B",
               lower = c(0.01, 0.05), upper = c(20, 30),
               control = list(maxit = 3000))
  if (fit$convergence != 0) warning("wng2pld: no convergence.")
  list(par = c(theta = fit$par[1], alpha = fit$par[2]), logLik = -fit$value,
       k = 2, convergence = fit$convergence)
}


## =====================================================================
## Fit all five models
## =====================================================================

fit_we   <- fit_wexp(x)
fit_wl   <- fit_wlindley(x)
fit_wxl  <- fit_wxlindley(x)
fit_wql  <- fit_wqld(x)
fit_w2pl <- fit_w2pld(x)
fit_wng  <- fit_wng2pld(x)

results <- data.frame(
  Dataset = "Turtles",
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
## check if each fitted density is valid (integrates to 1)?
## =====================================================================

cat("\nIntegral of fitted density over [0, 2*pi):\n")

cat("  Wrapped Exponential:   ",
    round(integrate(function(phi) d_wexp(phi, fit_we$par["theta"]),
                    lower = 0, upper = 2 * pi)$value, 8), "\n")

cat("  Wrapped Lindley:       ",
    round(integrate(function(phi) d_wlindley(phi, fit_wl$par["theta"]),
                    lower = 0, upper = 2 * pi)$value, 8), "\n")

cat("  Wrapped XLindley:      ",
    round(integrate(function(phi) d_wxlindley(phi, fit_wxl$par["theta"]),
                    lower = 0, upper = 2 * pi)$value, 8), "\n")

cat("  Wrapped Quasi-Lindley: ",
    round(integrate(function(phi) d_wql(phi, fit_wql$par["theta"], fit_wql$par["alpha"]),
                    lower = 0, upper = 2 * pi)$value, 8), "\n")

cat("  Wrapped 2-par Lindley: ",
    round(integrate(function(phi) d_w2pld(phi, fit_w2pl$par["theta"], fit_w2pl$par["alpha"]),
                    lower = 0, upper = 2 * pi)$value, 8), "\n")

cat("  Wrapped NG2PLD:        ",
    round(integrate(function(phi) d_wng2pld(phi, fit_wng$par["theta"], fit_wng$par["alpha"]),
                    lower = 0, upper = 2 * pi)$value, 8), "\n")


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

#histogram with all five fitted densities
plot_all <- ggplot() +
  
  geom_histogram(
    aes(x = x, y = after_stat(density)),
    bins = 6,
    boundary = 0,
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
    x = expression(lambda),
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

#===============================================================================
class_width    <- 2 * pi / 24
class_readings <- "lower"

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
  ## wNG2PLD: multi-start
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
ggsave("pltrans.pdf",  plot_prof, width = 6, height = 4.5)
