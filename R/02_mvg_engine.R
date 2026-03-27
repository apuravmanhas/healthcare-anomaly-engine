# =============================================================================
# 02_mvg_engine.R — Scratch-Built Multivariate Gaussian Anomaly Detection
# Zero-Dependency Healthcare Provider Fraud Detection
# =============================================================================
# Implements the full MVG probability density function using native R matrix
# algebra. No ML packages. Pure math.
#
# p(x; μ, Σ) = (1 / ((2π)^(n/2) |Σ|^(1/2))) * exp(-0.5 * (x-μ)^T Σ^(-1) (x-μ))
# =============================================================================

# ---------- Core Linear Algebra ----------

compute_mean_vector <- function(X) {
  # Compute column-wise mean vector μ
  # X: m x n matrix (m observations, n features)
  X <- as.matrix(X)
  mu <- colMeans(X)
  return(mu)
}

compute_covariance_matrix <- function(X) {
  # Compute the n x n covariance matrix Σ from scratch
  # Σ = (1/(m-1)) * (X - μ)^T (X - μ)
  #
  # We do NOT use cov(). We compute this via matrix multiplication.
  
  X <- as.matrix(X)
  m <- nrow(X)
  n <- ncol(X)
  
  # Step 1: Compute mean vector
  mu <- compute_mean_vector(X)
  
  # Step 2: Center the data matrix (subtract mean from each row)
  # X_centered[i,j] = X[i,j] - μ[j]
  X_centered <- t(t(X) - mu)  # vectorized subtraction across rows
  
  # Step 3: Compute covariance via matrix multiplication
  # Σ = (X_c)^T %*% X_c / (m - 1)
  Sigma <- (t(X_centered) %*% X_centered) / (m - 1)
  
  return(Sigma)
}

multivariate_gaussian_pdf <- function(x, mu, Sigma) {
  # Compute the multivariate Gaussian probability density for a single point x
  #
  # p(x; μ, Σ) = (1 / ((2π)^(n/2) |Σ|^(1/2))) * exp(-0.5 * (x-μ)^T Σ^(-1) (x-μ))
  #
  # x:     numeric vector of length n (single observation)
  # mu:    numeric vector of length n (mean vector)
  # Sigma: n x n covariance matrix
  
  x  <- as.numeric(x)
  mu <- as.numeric(mu)
  n  <- length(mu)
  
  # Deviation from mean
  diff <- x - mu
  
  # Inverse of covariance matrix (via solve — LU decomposition)
  Sigma_inv <- solve(Sigma)
  
  # Determinant of covariance matrix
  det_Sigma <- det(Sigma)
  
  # Mahalanobis distance squared: (x-μ)^T Σ^(-1) (x-μ)
  # Note: diff is a vector, so we treat it as a column vector
  mahal_sq <- as.numeric(t(diff) %*% Sigma_inv %*% diff)
  
  # Normalization constant: 1 / ((2π)^(n/2) * |Σ|^(1/2))
  norm_const <- 1 / ((2 * pi)^(n / 2) * sqrt(abs(det_Sigma)))
  
  # Full PDF
  p <- norm_const * exp(-0.5 * mahal_sq)
  
  return(p)
}

compute_all_probabilities <- function(X, mu, Sigma) {
  # Vectorized computation of MVG PDF for ALL rows in X
  # Returns a vector of probabilities, one per observation
  
  X <- as.matrix(X)
  m <- nrow(X)
  n <- ncol(X)
  
  Sigma_inv <- solve(Sigma)
  det_Sigma <- det(Sigma)
  norm_const <- 1 / ((2 * pi)^(n / 2) * sqrt(abs(det_Sigma)))
  
  # Center all observations
  X_centered <- t(t(X) - mu)
  
  # Compute Mahalanobis distance squared for all rows at once
  # mahal_sq[i] = X_centered[i,] %*% Sigma_inv %*% X_centered[i,]^T
  # Vectorized: diag(X_centered %*% Sigma_inv %*% t(X_centered))
  # But diag() on huge matrix is wasteful. Instead use rowSums trick:
  # mahal_sq = rowSums((X_centered %*% Sigma_inv) * X_centered)
  
  mahal_sq <- rowSums((X_centered %*% Sigma_inv) * X_centered)
  
  # Probabilities
  probabilities <- norm_const * exp(-0.5 * mahal_sq)
  
  return(probabilities)
}

# ---------- Mahalanobis Distance ----------

compute_mahalanobis <- function(X, mu, Sigma) {
  # Compute Mahalanobis distance for each observation
  # D_M(x) = sqrt((x-μ)^T Σ^(-1) (x-μ))
  #
  # Returns a vector of distances
  
  X <- as.matrix(X)
  Sigma_inv <- solve(Sigma)
  X_centered <- t(t(X) - mu)
  
  mahal_sq <- rowSums((X_centered %*% Sigma_inv) * X_centered)
  
  # Return the distance (square root of squared distance)
  return(sqrt(pmax(0, mahal_sq)))
}

# ---------- Threshold Selection ----------

select_threshold <- function(probabilities, contamination = 0.10) {
  # Dynamically select threshold ε using the contamination rate
  # We expect ~contamination fraction of data to be anomalous
  # So ε = the probability value at the (contamination) quantile
  
  epsilon <- quantile(probabilities, probs = contamination, na.rm = TRUE)
  return(as.numeric(epsilon))
}

# ---------- Anomaly Scoring ----------

compute_anomaly_scores <- function(probabilities) {
  # Convert raw probabilities to anomaly scores in [0, 1]
  # Higher score = more anomalous
  #
  # We use: score = 1 - (log(p) - min(log(p))) / (max(log(p)) - min(log(p)))
  # This gives a normalized score where the least probable = 1.0
  
  log_p <- log(pmax(probabilities, .Machine$double.xmin))
  
  min_lp <- min(log_p)
  max_lp <- max(log_p)
  
  if (max_lp == min_lp) {
    return(rep(0.5, length(probabilities)))
  }
  
  # Normalize to [0, 1] then invert (so high = anomalous)
  scores <- 1 - (log_p - min_lp) / (max_lp - min_lp)
  
  return(scores)
}

# ---------- Master Function ----------

run_mvg_anomaly_detection <- function(doctors, feature_cols = NULL) {
  # Full MVG anomaly detection pipeline
  #
  # 1. Extract feature matrix
  # 2. Normalize features
  # 3. Compute mean vector and covariance matrix
  # 4. Compute probabilities for all doctors
  # 5. Select dynamic threshold
  # 6. Flag anomalies
  
  section_header("MULTIVARIATE GAUSSIAN ANOMALY DETECTION")
  
  # Default feature columns
  if (is.null(feature_cols)) {
    feature_cols <- c("years_experience", "consultations_per_month",
                      "avg_billing_amount", "patient_complaints",
                      "prescriptions_per_patient", "online_reviews_count",
                      "review_score")
  }
  
  cat(sprintf("  Features: %s\n", paste(feature_cols, collapse = ", ")))
  cat(sprintf("  Observations: %d\n", nrow(doctors)))
  
  # Step 1: Extract and normalize feature matrix
  X_raw <- as.matrix(doctors[, feature_cols])
  X <- z_score_normalize(X_raw)
  
  cat("  [✓] Feature matrix extracted and z-score normalized\n")
  
  # Step 2: Compute statistics from scratch
  mu    <- compute_mean_vector(X)
  Sigma <- compute_covariance_matrix(X)
  
  cat(sprintf("  [✓] Mean vector computed (%d dimensions)\n", length(mu)))
  cat(sprintf("  [✓] Covariance matrix computed (%dx%d)\n", nrow(Sigma), ncol(Sigma)))
  
  # Regularize if needed for numerical stability
  if (!is_positive_definite(Sigma)) {
    cat("  [!] Covariance matrix not positive definite — applying ridge regularization\n")
    Sigma <- regularize_covariance(Sigma)
  }
  
  cat(sprintf("  [✓] Determinant of Σ: %s\n", formatC(det(Sigma), format = "e", digits = 4)))
  
  # Step 3: Compute probabilities for all doctors
  probabilities <- compute_all_probabilities(X, mu, Sigma)
  
  cat(sprintf("  [✓] Probabilities computed (range: %s to %s)\n",
              format_probability(min(probabilities)),
              format_probability(max(probabilities))))
  
  # Step 4: Compute Mahalanobis distances
  mahal_distances <- compute_mahalanobis(X, mu, Sigma)
  
  cat(sprintf("  [✓] Mahalanobis distances computed (range: %.2f to %.2f)\n",
              min(mahal_distances), max(mahal_distances)))
  
  # Step 5: Compute anomaly scores
  anomaly_scores <- compute_anomaly_scores(probabilities)
  
  # Step 6: Select threshold and flag
  epsilon <- select_threshold(probabilities, contamination = 0.12)
  is_flagged <- probabilities < epsilon
  
  cat(sprintf("  [✓] Dynamic threshold ε = %s\n", format_probability(epsilon)))
  cat(sprintf("  [✓] Flagged %d / %d doctors as anomalies (%.1f%%)\n",
              sum(is_flagged), length(is_flagged),
              100 * sum(is_flagged) / length(is_flagged)))
  
  # Build results
  results <- data.frame(
    doctor_id        = doctors$doctor_id,
    probability      = probabilities,
    mahal_distance   = mahal_distances,
    mvg_anomaly_score = anomaly_scores,
    mvg_flagged      = is_flagged,
    stringsAsFactors = FALSE
  )
  
  return(list(
    results    = results,
    mu         = mu,
    Sigma      = Sigma,
    Sigma_inv  = solve(Sigma),
    epsilon    = epsilon,
    X_normalized = X,
    feature_cols = feature_cols
  ))
}

cat("[✓] MVG anomaly detection engine loaded.\n")
