# =============================================================================
# 05_utils.R — Utility Functions for Anomaly Detection Engine
# Zero-Dependency Healthcare Provider Fraud Detection
# =============================================================================

# ---------- Feature Normalization ----------

z_score_normalize <- function(X) {
  # Standardize each column to mean=0, sd=1
  # X: numeric matrix (rows = observations, cols = features)
  X <- as.matrix(X)
  n <- nrow(X)
  means <- colMeans(X)
  sds   <- sqrt(colSums((t(t(X) - means))^2) / (n - 1))
  sds[sds == 0] <- 1  # prevent division by zero for constant features
  scaled <- t((t(X) - means) / sds)
  attr(scaled, "center") <- means
  attr(scaled, "scale")  <- sds
  return(scaled)
}

min_max_scale <- function(X) {
  # Scale each column to [0, 1]
  X <- as.matrix(X)
  mins <- apply(X, 2, min)
  maxs <- apply(X, 2, max)
  ranges <- maxs - mins
  ranges[ranges == 0] <- 1
  scaled <- t((t(X) - mins) / ranges)
  attr(scaled, "min")   <- mins
  attr(scaled, "range") <- ranges
  return(scaled)
}

# ---------- Composite Risk Scoring ----------

composite_risk_score <- function(mvg_score, centrality_score, benford_score,
                                  w_mvg = 0.50, w_net = 0.30, w_ben = 0.20) {
  # Weighted fusion of all three detection methods
  # All scores should be in [0, 1] where 1 = most suspicious
  #
  # Weights:
  #   MVG (statistical anomaly)   = 50%   (primary signal)
  #   Network centrality          = 30%   (collusion signal)
  #   Benford's Law deviation     = 20%   (data fabrication signal)
  
  composite <- w_mvg * mvg_score + w_net * centrality_score + w_ben * benford_score
  return(composite)
}

# ---------- Display Formatting ----------

format_probability <- function(p, digits = 4) {
  # Format very small probabilities in scientific notation
  ifelse(p < 1e-10,
         formatC(p, format = "e", digits = digits),
         formatC(p, format = "f", digits = digits))
}

format_percentage <- function(x, digits = 1) {
  paste0(formatC(x * 100, format = "f", digits = digits), "%")
}

# ---------- Terminal Output Aesthetics ----------

print_banner <- function() {
  cat("\n")
  cat("╔══════════════════════════════════════════════════════════════════╗\n")
  cat("║                                                                ║\n")
  cat("║   HEALTHCARE PROVIDER ANOMALY DETECTION ENGINE                 ║\n")
  cat("║   Zero-Dependency | Scratch-Built Linear Algebra               ║\n")
  cat("║                                                                ║\n")
  cat("║   Multivariate Gaussian  ·  Eigenvector Centrality             ║\n")
  cat("║   Benford's Law  ·  Composite Risk Scoring                     ║\n")
  cat("║                                                                ║\n")
  cat("╚══════════════════════════════════════════════════════════════════╝\n")
  cat("\n")
}

progress_bar <- function(current, total, label = "", width = 40) {
  pct  <- current / total
  filled <- round(pct * width)
  empty  <- width - filled
  bar <- paste0("[", paste(rep("█", filled), collapse = ""),
                paste(rep("░", empty), collapse = ""), "]")
  cat(sprintf("\r  %s %s %3.0f%%  %s", label, bar, pct * 100,
              ifelse(current == total, "\n", "")))
  flush.console()
}

section_header <- function(title) {
  cat("\n")
  cat(paste0("── ", title, " ", paste(rep("─", max(1, 60 - nchar(title))), collapse = ""), "\n"))
}

# ---------- Mathematical Helpers ----------

safe_log <- function(x, base = exp(1)) {
  # Logarithm that handles zero values gracefully
  x[x <= 0] <- .Machine$double.xmin
  log(x, base)
}

is_positive_definite <- function(M) {
  # Check if a matrix is symmetric positive definite
  # via eigenvalue decomposition
  if (!isSymmetric(M)) return(FALSE)
  eigenvalues <- eigen(M, symmetric = TRUE, only.values = TRUE)$values
  all(eigenvalues > 0)
}

regularize_covariance <- function(Sigma, lambda = 1e-6) {
  # Add small ridge to diagonal for numerical stability
  # Sigma + lambda * I
  n <- nrow(Sigma)
  Sigma + lambda * diag(n)
}

# ---------- Risk Classification ----------

classify_risk <- function(composite_score) {
  # Classify into risk tiers based on composite score
  cut(composite_score,
      breaks = c(-Inf, 0.3, 0.5, 0.7, 0.85, Inf),
      labels = c("LOW", "MODERATE", "ELEVATED", "HIGH", "CRITICAL"),
      right  = FALSE)
}

risk_color <- function(level) {
  # Return a color for each risk level (for ggplot2)
  colors <- c(
    "LOW"       = "#2ecc71",
    "MODERATE"  = "#f1c40f",
    "ELEVATED"  = "#e67e22",
    "HIGH"      = "#e74c3c",
    "CRITICAL"  = "#8e44ad"
  )
  colors[as.character(level)]
}

cat("[✓] Utility functions loaded.\n")
