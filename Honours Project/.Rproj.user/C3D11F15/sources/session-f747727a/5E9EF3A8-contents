library(circular)
library(ggplot2)
library(NPCirc)
library(scales)
library(latex2exp)

rm(list = ls())

lam_grid <- seq(0, 360, by = 0.5) * (pi / 180)   # the angle, in radians

#-------------------------------------------------------------------------------
# 1. DENSITY AND DISTRIBUTION FUNCTIONS
#-------------------------------------------------------------------------------
# like a plausible density.
n_terms <- function(theta, alpha, tol = 1e-16, cap = 20000L) {
  if (theta <= 0) stop("theta must be positive")
  p <- max(alpha - 1, 0)
  M <- 60
  for (k in 1:8) {                                   # bounded fixed-point pass
    M_new <- (log(1 / tol) + p * log(2 * pi * max(M, 1))) / (2 * pi * theta) + 20
    if (M_new <= M) break
    M <- min(M_new, cap)
  }
  as.integer(max(60, ceiling(M)))
}

# pdf 
pdf_wng2pld <- function(lam, theta, alpha) {
  M <- n_terms(theta, alpha)
  m <- 0:M
  vapply(lam, function(l) {
    xm <- l + 2 * pi * m
    (theta^2 / (theta + 1)) *
      sum((1 + theta^(alpha - 2) * xm^(alpha - 1) / gamma(alpha)) * exp(-theta * xm))
  }, numeric(1))
}

#cdf
cdf_wng2pld <- function(lam, theta, alpha) {
  M <- n_terms(theta, alpha)
  m <- 0:M
  elem <- theta * (1 - exp(-theta * lam)) /
    ((theta + 1) * (1 - exp(-2 * pi * theta)))
  series <- vapply(lam, function(l) {
    sum(pgamma(2 * pi * theta * m, alpha, lower.tail = FALSE) -
          pgamma(theta * l + 2 * pi * theta * m, alpha, lower.tail = FALSE))
  }, numeric(1))
  elem + series / (theta + 1)
}

## ---- sanity checks --------------------------------------------------------
## Both should print 1
cat("integral of pdf over [0, 2pi):",
    integrate(function(l) pdf_wng2pld(l, theta = 1, alpha = 3), 0, 2 * pi)$value, "\n")
cat("G(2pi):", cdf_wng2pld(2 * pi, theta = 1, alpha = 3), "\n")

## Nesting checks: alpha = 1 must reproduce the wrapped Exponential and
## alpha = 2 the wrapped Lindley.
th <- 1
w_exp <- th * exp(-th * lam_grid) / (1 - exp(-2 * pi * th))
r <- exp(-2 * pi * th)
w_lin <- th^2 / (th + 1) * exp(-th * lam_grid) *
  ((1 + lam_grid) / (1 - r) + 2 * pi * r / (1 - r)^2)
cat("max |wNG2PLD(alpha=1) - wrapped Exponential|:",
    max(abs(pdf_wng2pld(lam_grid, th, 1) - w_exp)), "\n")
cat("max |wNG2PLD(alpha=2) - wrapped Lindley|:   ",
    max(abs(pdf_wng2pld(lam_grid, th, 2) - w_lin)), "\n")
rm(th, w_exp, r, w_lin)

#------------------------------------------------------------------------------

# 2. PARAMETER COMBINATIONS

#-------------------------------------------------------------------------------

params <- data.frame(
  key   = c("y1", "y2", "y3", "y4", "y5"),
  theta = c(1,    1,    1,    2,    0.5),
  alpha = c(1,    2,    3,    3,    3),
  stringsAsFactors = FALSE
)

# Legend labels, in the same order as params$key
lab <- c(
  TeX('$\\theta = 1, \\alpha = 1$'),
  TeX('$\\theta = 1, \\alpha = 2$'),
  TeX('$\\theta = 1, \\alpha = 3$'),
  TeX('$\\theta = 2, \\alpha = 3$'),
  TeX('$\\theta = 0.5, \\alpha = 3$')
)

lty_vals <- c("solid", "dotdash", "twodash", "longdash", "dotted")

col_vals <- c(
  y1 = "grey60",     # theta = 1,   alpha = 1  - wrapped Exponential (reference)
  y2 = "grey25",     # theta = 1,   alpha = 2  - wrapped Lindley (reference)
  y3 = "#D55E00",    # theta = 1,   alpha = 3
  y4 = "#0072B2",    # theta = 2,   alpha = 3
  y5 = "#009E73"     # theta = 0.5, alpha = 3
)

pi_scales <- math_format(.x * pi, format = function(x) x / pi)
LWg <- 1.1   # line thickness

#-------------------------------------------------------------------------------
# 3. LINEAR PLOTS
#-------------------------------------------------------------------------------

build_df <- function(FUN) {
  do.call(rbind, lapply(seq_len(nrow(params)), function(i) {
    data.frame(lam   = lam_grid,
               value = FUN(lam_grid, params$theta[i], params$alpha[i]),
               key   = params$key[i],
               stringsAsFactors = FALSE)
  }))
}

make_plot <- function(df, ylab_expr) {
  ggplot(df, aes(x = lam, y = value, colour = key, linetype = key)) +
    theme_classic() +
    geom_line(linewidth = LWg) +
    scale_colour_manual("", labels = lab, breaks = params$key, values = col_vals) +
    scale_linetype_manual("", labels = lab, breaks = params$key, values = lty_vals) +
    xlab(TeX('$\\lambda$')) +                     # the angle
    ylab(ylab_expr) +
    scale_x_continuous(labels = pi_scales, breaks = seq(0, 2 * pi, pi / 2)) +
    theme(
      axis.title       = element_text(size = 18),
      axis.text        = element_text(size = 16),
      axis.ticks       = element_line(linewidth = 0.6),
      axis.line        = element_line(linewidth = 0.6),
      legend.text      = element_text(size = 16),
      legend.title     = element_text(size = 17),
      legend.key.width = unit(1.6, "cm")
    )
}

## ---- pdf -------------------------------------------------------------------
df_pdf <- build_df(pdf_wng2pld)

print(make_plot(df_pdf, TeX('$g(\\lambda)$')))


## ---- cdf -------------------------------------------------------------------
df_cdf <- build_df(cdf_wng2pld)

print(make_plot(df_cdf, TeX('$G(\\lambda)$')))


#-------------------------------------------------------------------------------
# 4. CIRCULAR (POLAR) DENSITY PLOT

#-------------------------------------------------------------------------------

polar_wng2pld <- function(max_extent = 0.85,
                          rescale     = TRUE,
                          legend_pos  = "topright") {
  
  # largest density value across all the parameter settings being drawn
  gmax <- max(vapply(seq_len(nrow(params)), function(i)
    max(pdf_wng2pld(lam_grid, params$theta[i], params$alpha[i])), numeric(1)))
  
  if (rescale) {
    scale_r <- max_extent / gmax     # one constant for every curve
    extent  <- max_extent
  } else {
    scale_r <- 1                     # true density units, wider window
    extent  <- gmax
  }
  lim <- 1 + extent + 0.12           # window must reach 1 + extent
  
  op <- par(pty = "s", mar = c(1.2, 1.2, 1.2, 1.2))
  on.exit(par(op), add = TRUE)
  
  # empty circular canvas
  plot(kern.den.circ(lam_grid, from = circular(0), to = circular(2 * pi)),
       xlim = c(-lim, lim), ylim = c(-lim, lim),
       main = "", xlab = "", ylab = "", col = "white", lwd = 2)
  
  for (i in seq_len(nrow(params))) {
    curve.circular(scale_r * pdf_wng2pld(x, params$theta[i], params$alpha[i]),
                   add = TRUE, lty = lty_vals[i], col = col_vals[i], lwd = LWg)
  }
  
  legend(legend_pos, legend = lab, lty = lty_vals, col = col_vals,
         cex = 0.75, bty = "n", lwd = LWg, inset = c(-0.04, -0.02), xpd = NA)
  
  invisible(c(gmax = gmax, scale = scale_r, lim = lim))
}

polar_wng2pld()                       # compact, common radial scale

#-------------------------------------------------------------------------------

use_panel <- function(which = c("alpha", "theta", "joint")) {
  which <- match.arg(which)
  cfg <- switch(
    which,
    # (a) effect of alpha, theta = 1 fixed
    alpha = data.frame(theta = c(1, 1, 1, 1),       alpha = c(1, 2, 3, 5)),
    # (b) effect of theta, alpha = 3 fixed
    theta = data.frame(theta = c(0.5, 1, 2, 4),     alpha = c(3, 3, 3, 3)),
    # (c) joint variation
    joint = data.frame(theta = c(0.5, 1, 2, 3),     alpha = c(3, 3, 4, 5))
  )
  cfg$key <- paste0("y", seq_len(nrow(cfg)))
  
  params   <<- cfg[, c("key", "theta", "alpha")]
  lab      <<- vapply(seq_len(nrow(cfg)), function(i)
    TeX(sprintf('$\\theta = %s, \\alpha = %s$', cfg$theta[i], cfg$alpha[i])),
    character(1))
  lty_vals <<- c("solid", "dotdash", "twodash", "longdash")[seq_len(nrow(cfg))]
  col_vals <<- setNames(c("#D55E00", "#0072B2", "#009E73", "grey25")[seq_len(nrow(cfg))],
                        cfg$key)
  message("params/lab/lty_vals/col_vals replaced")
  invisible(params)
}

