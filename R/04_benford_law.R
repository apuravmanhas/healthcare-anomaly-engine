# =============================================================================
# 04_benford_law.R — Benford's Law Analysis for Fraud Detection
# Zero-Dependency Healthcare Provider Fraud Detection
# =============================================================================
# Analyzes the leading-digit distribution of billing amounts to detect
# fabricated data. Implements chi-squared test and MAD conformity from scratch.
#
# Benford's Law: P(d) = log10(1 + 1/d) for d = 1, 2, ..., 9
# =============================================================================

# ---------- Leading Digit Extraction ----------

extract_leading_digit <- function(x) {
  # Extract the first non-zero digit from a numeric value
  # e.g., 3847.50 → 3,  0.0052 → 5,  123 → 1
  
  x <- abs(x)
  x <- x[x > 0]  # remove zeros
  
  # Convert to string, strip leading zeros and decimal points
  # Take the first non-zero digit
  digits <- as.integer(substr(format(x, scientific = FALSE, trim = TRUE), 1, 1))
  
  # Handle cases where format gives "0" (small decimals)
  for (i in seq_along(digits)) {
    if (is.na(digits[i]) || digits[i] == 0) {
      s <- format(x[i], scientific = FALSE, trim = TRUE)
      s <- gsub("^0+\\.?0*", "", s)  # strip leading zeros
      if (nchar(s) > 0) {
        digits[i] <- as.integer(substr(s, 1, 1))
      } else {
        digits[i] <- NA
      }
    }
  }
  
  return(digits)
}

# ---------- Benford Expected Distribution ----------

benford_expected <- function() {
  # Returns the theoretical Benford distribution
  # P(d) = log10(1 + 1/d) for d = 1, 2, ..., 9
  
  d <- 1:9
  probs <- log10(1 + 1/d)
  names(probs) <- as.character(d)
  return(probs)
}

# ---------- Observed Distribution ----------

compute_digit_distribution <- function(digits) {
  # Compute the observed frequency distribution of leading digits
  
  digits <- digits[!is.na(digits) & digits >= 1 & digits <= 9]
  n <- length(digits)
  
  counts <- tabulate(digits, nbins = 9)
  names(counts) <- as.character(1:9)
  
  proportions <- counts / n
  
  return(list(
    counts      = counts,
    proportions = proportions,
    n           = n
  ))
}

# ---------- Chi-Squared Test (from scratch) ----------

benford_chi_squared <- function(observed_counts, expected_probs) {
  # Scratch implementation of Pearson's chi-squared goodness-of-fit test
  #
  # χ² = Σ (O_i - E_i)² / E_i
  #
  # observed_counts: integer vector of observed counts for digits 1-9
  # expected_probs: expected probabilities from Benford's Law
  
  n <- sum(observed_counts)
  expected_counts <- expected_probs * n
  
  # Chi-squared statistic
  chi_sq <- sum((observed_counts - expected_counts)^2 / expected_counts)
  
  # Degrees of freedom = 9 - 1 = 8
  df <- 8
  
  # P-value using the incomplete gamma function approximation
  # We use R's built-in pchisq for the p-value calculation
  # (this is a mathematical function, not a statistical package)
  p_value <- 1 - pchisq(chi_sq, df = df)
  
  return(list(
    chi_squared = chi_sq,
    df          = df,
    p_value     = p_value,
    significant = p_value < 0.05,
    observed    = observed_counts,
    expected    = expected_counts
  ))
}

# ---------- Mean Absolute Deviation (MAD) Conformity ----------

benford_mad <- function(observed_probs, expected_probs) {
  # Mean Absolute Deviation from Benford's distribution
  # MAD = (1/9) * Σ |observed_i - expected_i|
  #
  # Classification (Nigrini's scale):
  #   MAD ≤ 0.006  → Close conformity
  #   MAD ≤ 0.012  → Acceptable conformity
  #   MAD ≤ 0.015  → Marginally acceptable
  #   MAD > 0.015  → Nonconforming
  
  mad_value <- mean(abs(observed_probs - expected_probs))
  
  classification <- ifelse(mad_value <= 0.006, "CLOSE CONFORMITY",
                    ifelse(mad_value <= 0.012, "ACCEPTABLE CONFORMITY",
                    ifelse(mad_value <= 0.015, "MARGINALLY ACCEPTABLE",
                           "NONCONFORMING")))
  
  return(list(
    mad            = mad_value,
    classification = classification
  ))
}

# ---------- Per-Doctor Benford Analysis ----------

analyze_doctor_benford <- function(doctor_id, amounts) {
  # Analyze a single doctor's billing amounts against Benford's Law
  
  digits   <- extract_leading_digit(amounts)
  obs      <- compute_digit_distribution(digits)
  expected <- benford_expected()
  
  if (obs$n < 10) {
    # Not enough data for meaningful analysis
    return(list(
      doctor_id = doctor_id,
      n_transactions = obs$n,
      chi_sq = NA,
      p_value = NA,
      mad = NA,
      classification = "INSUFFICIENT DATA",
      benford_score = 0.5,
      observed_proportions = obs$proportions,
      conforming = NA
    ))
  }
  
  chi_test <- benford_chi_squared(obs$counts, expected)
  mad_test <- benford_mad(obs$proportions, expected)
  
  # Convert to anomaly score [0, 1]
  # Higher MAD = more suspicious
  # Scale: MAD of 0 = score 0, MAD of 0.05+ = score 1
  benford_score <- min(1, mad_test$mad / 0.05)
  
  return(list(
    doctor_id = doctor_id,
    n_transactions = obs$n,
    chi_sq = chi_test$chi_squared,
    p_value = chi_test$p_value,
    mad = mad_test$mad,
    classification = mad_test$classification,
    benford_score = benford_score,
    observed_proportions = obs$proportions,
    conforming = !chi_test$significant
  ))
}

# ---------- Master Function ----------

run_benford_analysis <- function(doctors, transactions) {
  
  section_header("BENFORD'S LAW ANALYSIS")
  
  cat(sprintf("  Transactions: %d | Doctors: %d\n",
              nrow(transactions), length(unique(transactions$doctor_id))))
  
  # Global Benford analysis (all transactions)
  cat("  [1/3] Global leading-digit analysis...\n")
  all_digits <- extract_leading_digit(transactions$amount)
  global_obs <- compute_digit_distribution(all_digits)
  expected   <- benford_expected()
  global_chi <- benford_chi_squared(global_obs$counts, expected)
  global_mad <- benford_mad(global_obs$proportions, expected)
  
  cat(sprintf("  [✓] Global χ² = %.2f (p = %s) | MAD = %.4f (%s)\n",
              global_chi$chi_squared,
              format_probability(global_chi$p_value),
              global_mad$mad,
              global_mad$classification))
  
  # Per-doctor Benford analysis
  cat("  [2/3] Per-doctor Benford analysis...\n")
  doctor_ids <- unique(transactions$doctor_id)
  n_docs <- length(doctor_ids)
  
  benford_results <- vector("list", n_docs)
  
  for (i in seq_along(doctor_ids)) {
    doc_id <- doctor_ids[i]
    doc_amounts <- transactions$amount[transactions$doctor_id == doc_id]
    benford_results[[i]] <- analyze_doctor_benford(doc_id, doc_amounts)
    
    if (i %% 100 == 0) {
      progress_bar(i, n_docs, "  Analyzing doctors")
    }
  }
  progress_bar(n_docs, n_docs, "  Analyzing doctors")
  
  # Compile into dataframe
  cat("  [3/3] Compiling results...\n")
  results <- data.frame(
    doctor_id        = sapply(benford_results, `[[`, "doctor_id"),
    benford_chi_sq   = sapply(benford_results, function(x) ifelse(is.na(x$chi_sq), NA, x$chi_sq)),
    benford_p_value  = sapply(benford_results, function(x) ifelse(is.na(x$p_value), NA, x$p_value)),
    benford_mad      = sapply(benford_results, function(x) ifelse(is.na(x$mad), NA, x$mad)),
    benford_class    = sapply(benford_results, `[[`, "classification"),
    benford_score    = sapply(benford_results, `[[`, "benford_score"),
    benford_conforming = sapply(benford_results, function(x) ifelse(is.na(x$conforming), NA, x$conforming)),
    stringsAsFactors = FALSE
  )
  
  n_nonconforming <- sum(results$benford_class == "NONCONFORMING", na.rm = TRUE)
  cat(sprintf("  [✓] Found %d nonconforming doctors (%.1f%%)\n",
              n_nonconforming, 100 * n_nonconforming / nrow(results)))
  
  return(list(
    results       = results,
    global_chi    = global_chi,
    global_mad    = global_mad,
    global_observed = global_obs,
    expected      = expected,
    per_doctor    = benford_results
  ))
}

cat("[✓] Benford's Law analysis engine loaded.\n")
