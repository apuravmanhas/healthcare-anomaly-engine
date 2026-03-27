# =============================================================================
# 01_generate_data.R — Synthetic Healthcare Provider Data Generator
# Zero-Dependency Healthcare Provider Fraud Detection
# =============================================================================
# Generates realistic mock data for ~500 doctors, with ~10% injected anomalies,
# referral network edges, and billing transaction logs.
# =============================================================================

generate_doctor_profiles <- function(n_doctors = 500, anomaly_rate = 0.10, seed = 42) {
  set.seed(seed)
  
  section_header("GENERATING SYNTHETIC HEALTHCARE PROVIDER DATA")
  cat(sprintf("  Doctors: %d | Anomaly injection rate: %.0f%%\n", n_doctors, anomaly_rate * 100))
  
  n_anomalies <- round(n_doctors * anomaly_rate)
  n_normal    <- n_doctors - n_anomalies
  
  # --- Generate NORMAL doctor profiles ---
  # Realistic distributions for legitimate practitioners
  normal_data <- data.frame(
    doctor_id               = paste0("DR-", sprintf("%04d", 1:n_normal)),
    years_experience        = pmax(1, round(rnorm(n_normal, mean = 15, sd = 8))),
    consultations_per_month = pmax(10, round(rnorm(n_normal, mean = 120, sd = 40))),
    avg_billing_amount      = pmax(200, round(rnorm(n_normal, mean = 2500, sd = 800), 2)),
    patient_complaints      = pmax(0, rpois(n_normal, lambda = 2)),
    prescriptions_per_patient = pmax(1, round(rnorm(n_normal, mean = 3.5, sd = 1.2), 1)),
    online_reviews_count    = pmax(0, round(rnorm(n_normal, mean = 45, sd = 25))),
    review_score            = pmin(5, pmax(1, round(rnorm(n_normal, mean = 4.1, sd = 0.5), 2))),
    is_anomaly              = FALSE,
    stringsAsFactors        = FALSE
  )
  
  # --- Generate ANOMALY doctor profiles ---
  # These are injected with statistically impossible or suspicious patterns
  anomaly_ids <- paste0("DR-", sprintf("%04d", (n_normal + 1):n_doctors))
  
  # Create different types of anomalies for realism
  n_type1 <- round(n_anomalies * 0.35)  # Billing fraud (inflated amounts)
  n_type2 <- round(n_anomalies * 0.25)  # Volume fraud (impossible consultation rates)
  n_type3 <- round(n_anomalies * 0.20)  # Fake reviews (too perfect + too many reviews)
  n_type4 <- n_anomalies - n_type1 - n_type2 - n_type3  # Mixed anomalies
  
  anomaly_data <- data.frame(
    doctor_id               = anomaly_ids,
    years_experience        = c(
      pmax(1, round(rnorm(n_type1, mean = 3, sd = 2))),     # Billing fraudsters: suspiciously junior
      pmax(1, round(rnorm(n_type2, mean = 5, sd = 3))),     # Volume fraud: junior doctors with impossible patient loads
      pmax(1, round(rnorm(n_type3, mean = 8, sd = 4))),     # Fake reviews
      pmax(1, round(rnorm(n_type4, mean = 4, sd = 3)))      # Mixed
    ),
    consultations_per_month = c(
      pmax(10, round(rnorm(n_type1, mean = 90, sd = 30))),   # Normal volume
      pmax(200, round(rnorm(n_type2, mean = 450, sd = 100))),# Impossibly high volume
      pmax(10, round(rnorm(n_type3, mean = 80, sd = 25))),   # Normal volume
      pmax(250, round(rnorm(n_type4, mean = 380, sd = 90)))  # High volume
    ),
    avg_billing_amount = c(
      pmax(5000, round(rnorm(n_type1, mean = 12000, sd = 3000), 2)),  # Massively inflated
      pmax(500, round(rnorm(n_type2, mean = 3000, sd = 800), 2)),     # Slightly high
      pmax(200, round(rnorm(n_type3, mean = 2200, sd = 600), 2)),     # Normal
      pmax(6000, round(rnorm(n_type4, mean = 9000, sd = 2500), 2))    # Inflated
    ),
    patient_complaints = c(
      pmax(0, rpois(n_type1, lambda = 8)),   # Higher complaints
      pmax(0, rpois(n_type2, lambda = 12)),  # Very high complaints
      pmax(0, rpois(n_type3, lambda = 0.5)), # Suspiciously low (fake reviews)
      pmax(0, rpois(n_type4, lambda = 10))   # High complaints
    ),
    prescriptions_per_patient = c(
      pmax(1, round(rnorm(n_type1, mean = 7.5, sd = 2), 1)),   # Over-prescribing
      pmax(1, round(rnorm(n_type2, mean = 8.0, sd = 1.5), 1)), # Over-prescribing
      pmax(1, round(rnorm(n_type3, mean = 3.0, sd = 0.8), 1)), # Normal
      pmax(1, round(rnorm(n_type4, mean = 9.0, sd = 2.0), 1))  # Extreme over-prescribing
    ),
    online_reviews_count = c(
      pmax(0, round(rnorm(n_type1, mean = 15, sd = 10))),     # Few reviews
      pmax(0, round(rnorm(n_type2, mean = 10, sd = 8))),      # Few reviews
      pmax(100, round(rnorm(n_type3, mean = 250, sd = 60))),  # Suspiciously many reviews
      pmax(0, round(rnorm(n_type4, mean = 12, sd = 8)))       # Few reviews
    ),
    review_score = c(
      pmin(5, pmax(1, round(rnorm(n_type1, mean = 3.2, sd = 0.8), 2))), # Below average
      pmin(5, pmax(1, round(rnorm(n_type2, mean = 2.8, sd = 0.9), 2))), # Poor reviews
      pmin(5, pmax(1, round(rnorm(n_type3, mean = 4.95, sd = 0.05), 2))),# Suspiciously perfect
      pmin(5, pmax(1, round(rnorm(n_type4, mean = 3.0, sd = 0.7), 2)))  # Below average
    ),
    is_anomaly = TRUE,
    stringsAsFactors = FALSE
  )
  
  # Combine and shuffle
  doctors <- rbind(normal_data, anomaly_data)
  doctors <- doctors[sample(nrow(doctors)), ]
  rownames(doctors) <- NULL
  
  # Add specialty and clinic assignment
  specialties <- c("Cardiology", "Dermatology", "Neurology", "Orthopedics",
                   "Pediatrics", "Oncology", "Psychiatry", "Radiology",
                   "General Practice", "Emergency Medicine", "Ophthalmology",
                   "ENT", "Gastroenterology", "Urology", "Pulmonology")
  clinics <- paste0("CLINIC-", sprintf("%03d", 1:50))
  
  doctors$specialty <- sample(specialties, nrow(doctors), replace = TRUE)
  doctors$clinic_id <- sample(clinics, nrow(doctors), replace = TRUE)
  
  cat(sprintf("  [✓] Generated %d doctor profiles (%d normal, %d anomalies)\n",
              nrow(doctors), n_normal, n_anomalies))
  
  return(doctors)
}


generate_referral_network <- function(doctors, n_edges = 1500, n_collusion_rings = 3, 
                                       ring_size = 5, seed = 42) {
  set.seed(seed + 1)
  
  section_header("GENERATING REFERRAL NETWORK")
  
  all_ids    <- doctors$doctor_id
  anomaly_ids <- doctors$doctor_id[doctors$is_anomaly]
  clinic_ids <- unique(doctors$clinic_id)
  n <- length(all_ids)
  
  # --- Generate normal referral edges ---
  # Doctors refer patients to other doctors (especially within same specialty clusters)
  from_ids <- sample(all_ids, n_edges, replace = TRUE)
  to_ids   <- sample(all_ids, n_edges, replace = TRUE)
  
  # Remove self-loops
  valid <- from_ids != to_ids
  from_ids <- from_ids[valid]
  to_ids   <- to_ids[valid]
  
  edges <- data.frame(
    from = from_ids,
    to   = to_ids,
    weight = round(runif(length(from_ids), 1, 10)),
    is_suspicious = FALSE,
    stringsAsFactors = FALSE
  )
  
  # --- Inject collusion rings ---
  # A small group of anomaly doctors refer exclusively among themselves
  collusion_doctors <- c()
  for (ring in 1:n_collusion_rings) {
    ring_members <- sample(anomaly_ids, min(ring_size, length(anomaly_ids)), replace = FALSE)
    collusion_doctors <- c(collusion_doctors, ring_members)
    
    # Create dense interconnections within the ring
    for (i in 1:length(ring_members)) {
      for (j in 1:length(ring_members)) {
        if (i != j) {
          # Multiple referrals between ring members (high weight = suspicious)
          ring_edge <- data.frame(
            from = ring_members[i],
            to   = ring_members[j],
            weight = round(runif(1, 15, 50)),  # Abnormally high referral weight
            is_suspicious = TRUE,
            stringsAsFactors = FALSE
          )
          edges <- rbind(edges, ring_edge)
        }
      }
    }
  }
  
  # Remove duplicate edges (keep the one with highest weight)
  edges <- edges[order(-edges$weight), ]
  edges <- edges[!duplicated(paste(edges$from, edges$to)), ]
  
  cat(sprintf("  [✓] Generated %d referral edges (%d suspicious collusion edges)\n",
              nrow(edges), sum(edges$is_suspicious)))
  cat(sprintf("  [✓] Injected %d collusion rings with %d unique members\n",
              n_collusion_rings, length(unique(collusion_doctors))))
  
  return(edges)
}


generate_billing_transactions <- function(doctors, n_per_doctor = 50, seed = 42) {
  set.seed(seed + 2)
  
  section_header("GENERATING BILLING TRANSACTIONS")
  
  transactions <- data.frame(
    doctor_id = character(),
    amount    = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:nrow(doctors)) {
    doc <- doctors[i, ]
    n_txn <- sample(30:n_per_doctor, 1)
    
    if (doc$is_anomaly) {
      # Anomaly doctors: fabricated billing amounts
      # They tend to use "round" numbers or repeat certain patterns
      # This will violate Benford's Law
      amounts <- sample(c(
        round(runif(round(n_txn * 0.4), 1000, 1000), -2),  # Lots of round hundreds
        round(runif(round(n_txn * 0.3), 5000, 5000), -3),  # Round thousands
        round(runif(round(n_txn * 0.3), 100, 9999), 2)     # Some real-looking
      ))
    } else {
      # Normal doctors: naturally occurring billing amounts (Benford-compliant)
      # Use log-normal distribution which naturally follows Benford's Law
      amounts <- round(rlnorm(n_txn, meanlog = log(doc$avg_billing_amount), sdlog = 0.6), 2)
      amounts <- pmax(50, amounts)  # minimum billing amount
    }
    
    doc_txn <- data.frame(
      doctor_id = rep(doc$doctor_id, length(amounts)),
      amount    = amounts,
      stringsAsFactors = FALSE
    )
    transactions <- rbind(transactions, doc_txn)
    
    if (i %% 100 == 0) {
      progress_bar(i, nrow(doctors), "  Generating transactions")
    }
  }
  
  progress_bar(nrow(doctors), nrow(doctors), "  Generating transactions")
  cat(sprintf("  [✓] Generated %d billing transactions across %d doctors\n",
              nrow(transactions), nrow(doctors)))
  
  return(transactions)
}


generate_all_data <- function(n_doctors = 500, seed = 42) {
  print_banner()
  
  doctors      <- generate_doctor_profiles(n_doctors = n_doctors, seed = seed)
  network      <- generate_referral_network(doctors, seed = seed)
  transactions <- generate_billing_transactions(doctors, seed = seed)
  
  section_header("DATA GENERATION COMPLETE")
  cat(sprintf("  Total doctors:       %d\n", nrow(doctors)))
  cat(sprintf("  Total anomalies:     %d\n", sum(doctors$is_anomaly)))
  cat(sprintf("  Network edges:       %d\n", nrow(network)))
  cat(sprintf("  Billing transactions: %d\n", nrow(transactions)))
  cat("\n")
  
  return(list(
    doctors      = doctors,
    network      = network,
    transactions = transactions
  ))
}

cat("[✓] Data generator loaded.\n")
