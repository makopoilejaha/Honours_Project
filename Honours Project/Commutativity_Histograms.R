
rm(list = ls())
library(circular) 

lerch_phi <- function(z, s, a, terms = 200) {
  k <- 0:(terms - 1)
  sum(z^k / (a + k)^s)
}


## Mixing weight

mix_weight <- function(theta) {
  theta / (theta + 1)
}


## Wrapped Exponential(theta)
# Closed form 

g1_wrapped_exp <- function(lambda, theta) {
  lambda <- lambda %% (2 * pi)
  theta * exp(-theta * lambda) / (1 - exp(-2 * pi * theta))
}


## Wrapped Gamma(alpha, theta) 


g2_wrapped_gamma <- function(lambda, theta, alpha) {
  lambda <- lambda %% (2 * pi)
  r <- exp(-2 * pi * theta)
  theta^alpha * (2 * pi)^(alpha - 1) * exp(-theta * lambda) / gamma(alpha) *
    sapply(lambda, function(l) lerch_phi(r, 1 - alpha, l / (2 * pi)))
}


## NG2PLD wrapped density


g_ng2pld <- function(lambda, theta, alpha) {
  lambda <- lambda %% (2 * pi)
  r <- exp(-2 * pi * theta)
  term1 <- 1 / (1 - r)
  term2 <- theta^(alpha - 2) * (2 * pi)^(alpha - 1) / gamma(alpha) *
    sapply(lambda, function(l) lerch_phi(r, 1 - alpha, l / (2 * pi)))
  theta^2 / (theta + 1) * exp(-theta * lambda) * (term1 + term2)
}


## Mixture-of-wrapped-densities, Route B 

g_mixture_of_wrapped <- function(lambda, theta, alpha) {
  p <- mix_weight(theta)
  p * g1_wrapped_exp(lambda, theta) + (1 - p) * g2_wrapped_gamma(lambda, theta, alpha)
}



## Analytic check: do g_ng2pld() and g_mixture_of_wrapped() agree?

#arbitrarily chosen plausible values
theta_true <- 2
alpha_true <- 3

lambda_grid <- seq(0, 2 * pi, length.out = 500)
lambda_grid <- lambda_grid[-length(lambda_grid)]   #exclude 2*pi, which wraps back to 0

#x-axis ticks and labels in terms of pi
pi_breaks <- seq(0, 2 * pi, by = pi / 2)
pi_labels <- expression(0, pi/2, pi, 3*pi/2, 2*pi)

g_theorem1   <- g_ng2pld(lambda_grid, theta_true, alpha_true)
g_mix_wrap   <- g_mixture_of_wrapped(lambda_grid, theta_true, alpha_true)

max_diff <- max(abs(g_theorem1 - g_mix_wrap))
cat("Max absolute difference between the two routes:", max_diff, "\n")
# Should be roughly 0 

plot(lambda_grid, g_theorem1, type = "l", lwd = 3, col = "black",
     xlab = expression(lambda), ylab = expression(g(lambda)),
     main = "", xaxt = "n")
axis(1, at = pi_breaks, labels = pi_labels)
lines(lambda_grid, g_mix_wrap, lwd = 2, lty = 2, col = "red")
legend("topright",
       legend = c("g(lambda)", "p*g1 + (1-p)*g2"),
       col = c("black", "red"), lwd = c(3, 2), lty = c(1, 2),cex=0.65)



## Empirical check OF whether simulated data (mix-then-wrap) match

sim_ng2pld <- function(n, theta, alpha) {
  p <- mix_weight(theta)
  comp <- rbinom(n, size = 1, prob = p)   #simulating b from a bernoulli distribution; 1 = Exp, 0 = Gamma(alpha, theta)
  
  x <- numeric(n)
  x[comp == 1] <- rexp(sum(comp == 1), rate = theta) #draw from exponential
  x[comp == 0] <- rgamma(sum(comp == 0), shape = alpha, rate = theta) #draw from gamma
  
  x %% (2 * pi)   #wrapping stage, we wrap X here
}

set.seed(1)
lambda_sim <- sim_ng2pld(n = 500, theta = theta_true, alpha = alpha_true)

hist(lambda_sim, breaks = 40, freq = FALSE, col = "grey85", border = "white",
     xlab = expression(lambda),
     main = "n=500", xaxt = "n")
axis(1, at = pi_breaks, labels = pi_labels)
lines(lambda_grid, g_theorem1, lwd = 2, col = "black")
lines(lambda_grid, g_mix_wrap, lwd = 2, lty = 2, col = "red")
legend("topright",
       legend = c(" g(lambda)", "p*g1 + (1-p)*g2", "Simulated histogram"),
       col = c("black", "red", "grey65"), lwd = c(2, 2, 8), lty = c(1, 2, 1),cex=0.66)



## PIT check on the same simulated sample 
#This checks whether our simulated data is consistent with the one drawn from the PDF. If the data really is from the PDF then plugging it back in should give us a uniform density

G_ng2pld <- function(lambda, theta, alpha) {
  # Direct numerical integration of g_ng2pld as a CDF check
  sapply(lambda, function(l) {
    integrate(function(x) g_ng2pld(x, theta, alpha), lower = 0, upper = l)$value
  })
}
set.seed(1)
u_pit <- G_ng2pld(lambda_sim, theta_true, alpha_true)

hist(u_pit, breaks = 30, freq = FALSE, main = "PIT check",
     xlab = "u = G(lambda_sim)")
abline(h = 1, col = "red", lty = 2, lwd = 2)

x_pit <- circular(2 * pi * u_pit, units = "radians")

kuiper.test(x_pit)
watson.test(x_pit)   # default dist is uniform