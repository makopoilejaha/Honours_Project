library(parallel)
library(dplyr)
library(ggplot2)
library(xtable)


## Vectorised density and Simulation

lerch_phi <- function(z, s, a, tol = 1e-12, max_terms = 500) {
  
  out <- numeric(length(a))
  z_power <- 1
  
  for (k in 0:(max_terms - 1)) {
    
    term <- z_power / (a + k)^s
    out <- out + term
    
    # Stop once the additional contribution is negligible
    # for all observations.
    if (k > 0 && max(abs(term)) < tol) break
    
    z_power <- z_power * z
  }
  
  out
}

g_ng2pld <- function(lambda, theta, alpha) {
  lambda <- lambda %% (2 * pi)
  r <- exp(-2 * pi * theta)
  term1 <- 1 / (1 - r)
  term2 <- theta^(alpha - 2) * (2 * pi)^(alpha - 1) / gamma(alpha) *
    lerch_phi(r, 1 - alpha, lambda / (2 * pi))
  theta^2 / (theta + 1) * exp(-theta * lambda) * (term1 + term2)
}

mix_weight <- function(theta) theta / (theta + 1)

sim_ng2pld <- function(n, theta, alpha) {
  p <- mix_weight(theta)
  comp <- rbinom(n, size = 1, prob = p)
  x <- numeric(n)
  x[comp == 1] <- rexp(sum(comp == 1), rate = theta)
  x[comp == 0] <- rgamma(sum(comp == 0), shape = alpha, rate = theta)
  x %% (2 * pi)
}


## MLE Fitting Functions


nll_ng2pld <- function(par, lambda_data) {
  theta <- exp(par[1])
  alpha <- exp(par[2])
  dens <- g_ng2pld(lambda_data, theta, alpha)
  if (any(!is.finite(dens)) || any(dens <= 0)) return(1e10)
  nll <- -sum(log(dens))
  if (!is.finite(nll)) return(1e10)
  nll
}


fit_ng2pld_mle <- function(lambda_data, n_starts = 20,
                           theta_range = c(0.1, 5), alpha_range = c(0.3, 5)) {
  starts_theta <- runif(n_starts, theta_range[1], theta_range[2])
  starts_alpha <- runif(n_starts, alpha_range[1], alpha_range[2])
  
  best_nll <- Inf
  best_par <- c(NA, NA)
  n_converged <- 0
  
  for (i in seq_len(n_starts)) {
    init <- c(log(starts_theta[i]), log(starts_alpha[i]))
    fit <- tryCatch(
      optim(par = init, fn = nll_ng2pld, lambda_data = lambda_data,
            method = "L-BFGS-B",
            lower = c(log(0.001), log(0.001)),
            upper = c(log(150), log(150)),
            control = list(maxit = 500)),
      error = function(e) NULL
    )
    if (is.null(fit) || fit$convergence != 0) next
    n_converged <- n_converged + 1
    if (fit$value < best_nll) {
      best_nll <- fit$value
      best_par <- fit$par
    }
  }
  
  if (n_converged == 0) return(c(theta_hat = NA, alpha_hat = NA, nll = NA, n_converged = 0))
  c(theta_hat = exp(best_par[1]), alpha_hat = exp(best_par[2]), nll = best_nll, n_converged = n_converged)
}

# 3. Multi-combination Parameter Recovery 


default_param_grid <- data.frame(
  alpha_true = c(0.5, 1, 1.5, 2, 2.5),
  theta_true = c(2, 1, 3, 2.5, 1.5)
)

run_grid_recovery_study <- function(param_grid = default_param_grid,
                                    sample_sizes = c(50, 100, 200, 500),
                                    B = 200, n_starts = 20, n_cores = NULL,
                                    seed = 1, verbose = TRUE,
                                    checkpoint_path = "grid_results_checkpoint.rds") {
  
  if (is.null(n_cores)) {
    n_cores <- max(1, detectCores() - 1)
  }
  
  param_combinations <- param_grid
  n_combos <- nrow(param_combinations)
  
  # Resume from a checkpoint if one exists 
  all_results <- data.frame()
  done_combo_ids <- character(0)
  
  if (file.exists(checkpoint_path)) {
    all_results <- readRDS(checkpoint_path)
    combo_status <- aggregate(n ~ theta_true + alpha_true, data = all_results,
                              FUN = function(x) length(unique(x)))
    completed <- combo_status[combo_status$n >= length(sample_sizes), ]
    done_combo_ids <- paste(completed$theta_true, completed$alpha_true)
    if (verbose && nrow(completed) > 0) {
      cat(sprintf("Resuming from checkpoint: %d of %d combos already done.\n",
                  nrow(completed), n_combos))
    }
  }
  
  single_rep <- function(b, theta_t, alpha_t, n, n_starts) {
    lambda_data <- sim_ng2pld(n, theta_t, alpha_t)
    fit_ng2pld_mle(lambda_data, n_starts = n_starts)
  }
  
  # Build the cluster once and reuse it for the entire grid 
  cl <- makeCluster(n_cores)
  on.exit(stopCluster(cl), add = TRUE)
  
  clusterExport(cl, varlist = c("sim_ng2pld", "mix_weight", "fit_ng2pld_mle",
                                "nll_ng2pld", "g_ng2pld", "lerch_phi",
                                "single_rep"),
                envir = environment())
  
  clusterSetRNGStream(cl, seed)
  
  for (c_idx in seq_len(n_combos)) {
    theta_t <- param_combinations$theta_true[c_idx]
    alpha_t <- param_combinations$alpha_true[c_idx]
    
    if (paste(theta_t, alpha_t) %in% done_combo_ids) {
      if (verbose) {
        cat(sprintf("Skipping combo %d/%d (theta=%.2f, alpha=%.2f), already in checkpoint\n",
                    c_idx, n_combos, theta_t, alpha_t))
      }
      next
    }
    
    if (verbose) {
      cat(sprintf("\n==================================================\n"))
      cat(sprintf("Combo %d/%d: theta_true = %.2f, alpha_true = %.2f\n",
                  c_idx, n_combos, theta_t, alpha_t))
      cat(sprintf("==================================================\n"))
    }
    
    for (n in sample_sizes) {
      if (verbose) {
        cat(sprintf("  n = %d (B = %d, cores = %d)... ", n, B, n_cores))
      }
      
      reps_list <- parLapply(cl, seq_len(B), single_rep,
                             theta_t = theta_t, alpha_t = alpha_t,
                             n = n, n_starts = n_starts)
      
      reps_mat <- do.call(rbind, reps_list)
      theta_hats <- reps_mat[, "theta_hat"]
      alpha_hats <- reps_mat[, "alpha_hat"]
      
      ok <- !is.na(theta_hats) & !is.na(alpha_hats)
      if (verbose) cat(sprintf("Done (%d/%d converged)\n", sum(ok), B))
      
      theta_hats <- theta_hats[ok]
      alpha_hats <- alpha_hats[ok]
      
      all_results <- rbind(all_results, data.frame(
        theta_true = theta_t, alpha_true = alpha_t, n = n, parameter = "theta",
        true_value = theta_t, estimate = mean(theta_hats), sd = sd(theta_hats),
        bias = mean(theta_hats) - theta_t, mse = mean((theta_hats - theta_t)^2),
        convergence_rate = mean(ok)
      ))
      
      all_results <- rbind(all_results, data.frame(
        theta_true = theta_t, alpha_true = alpha_t, n = n, parameter = "alpha",
        true_value = alpha_t, estimate = mean(alpha_hats), sd = sd(alpha_hats),
        bias = mean(alpha_hats) - alpha_t, mse = mean((alpha_hats - alpha_t)^2),
        convergence_rate = mean(ok)
      ))
    }
    
    saveRDS(all_results, checkpoint_path)
    if (verbose) cat(sprintf("Checkpoint saved (%d combos done so far)\n", c_idx))
  }
  
  write.csv(all_results, sub("\\.rds$", ".csv", checkpoint_path), row.names = FALSE)
  all_results
}


## 

grid_results <- run_grid_recovery_study(
  param_grid = default_param_grid,
  sample_sizes = c(50, 100, 200, 500),
  B = 200,
  n_starts = 20,
  seed = 1
)


## Results table and plots

saveRDS(grid_results, "ng2pld_recovery_results.rds")
write.csv(grid_results, "ng2pld_recovery_results.csv", row.names = FALSE)

## Summary table
results_table <- grid_results %>%
  arrange(parameter, alpha_true, n) %>%
  mutate(
    estimate = round(estimate, 4),
    sd = round(sd, 4),
    bias = round(bias, 4),
    mse = round(mse, 4),
    convergence_pct = round(100 * convergence_rate, 1)
  ) %>%
  select(parameter, theta_true, alpha_true, n, true_value,
         estimate, sd, bias, mse, convergence_pct)

print(results_table)

latex_table <- xtable(
  results_table,
  caption = "Parameter recovery: estimate, standard deviation, bias and MSE across modal regions",
  label = "tab:ng2pld_recovery",
  digits = c(0, 0, 2, 2, 0, 2, 4, 4, 4, 4, 1)
)

print(latex_table, include.rownames = FALSE, file = "ng2pld_recovery_table.tex")

## MSE plots
mse_plot <- ggplot(
  grid_results,
  aes(x = n, y = mse, colour = factor(alpha_true),
      group = interaction(theta_true, alpha_true))
) +
  geom_line() +
  geom_point(size = 2) +
  facet_wrap(~ parameter, scales = "free_y",
             labeller = as_labeller(c(theta = "theta", alpha = "alpha"))) +
  scale_x_continuous(breaks = sort(unique(grid_results$n))) +
  labs(
    x = "Sample size (n)",
    y = "MSE",
    colour = "alpha (modal region)",
    title = "MSE of MLE estimates across modal regions"
  ) +
  theme_bw()

ggsave("ng2pld_mse_plot.pdf", mse_plot, width = 8, height = 5)


