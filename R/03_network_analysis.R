# =============================================================================
# 03_network_analysis.R — Eigenvector Centrality & Collusion Detection
# Zero-Dependency Healthcare Provider Fraud Detection
# =============================================================================
# Builds adjacency matrices from referral data, computes eigenvector centrality
# via power iteration, detects collusion clusters via spectral methods.
# All from scratch — no igraph, no network packages.
# =============================================================================

# ---------- Adjacency Matrix Construction ----------

build_adjacency_matrix <- function(edges, nodes) {
  # Construct a square weighted adjacency matrix from edge list
  #
  # edges: data.frame with columns (from, to, weight)
  # nodes: character vector of all node IDs
  #
  # Returns: n x n matrix where A[i,j] = weight of edge from node i to node j
  
  n <- length(nodes)
  A <- matrix(0, nrow = n, ncol = n)
  rownames(A) <- nodes
  colnames(A) <- nodes
  
  for (k in 1:nrow(edges)) {
    i <- which(nodes == edges$from[k])
    j <- which(nodes == edges$to[k])
    if (length(i) == 1 && length(j) == 1) {
      A[i, j] <- A[i, j] + edges$weight[k]
      A[j, i] <- A[j, i] + edges$weight[k]  # undirected
    }
  }
  
  return(A)
}

# ---------- Eigenvector Centrality via Power Iteration ----------

eigenvector_centrality <- function(A, tol = 1e-8, max_iter = 1000) {
  # Compute eigenvector centrality using the power iteration method
  #
  # The dominant eigenvector of A gives the centrality scores.
  #
  # Algorithm:
  #   v_0 = uniform vector
  #   Repeat:
  #     v_{k+1} = A %*% v_k            (matrix-vector multiplication)
  #     v_{k+1} = v_{k+1} / ||v_{k+1}||  (normalize)
  #   Until ||v_{k+1} - v_k|| < tol
  #
  # A: n x n adjacency matrix
  # tol: convergence tolerance
  # max_iter: maximum iterations
  #
  # Returns: list(centrality, eigenvalue, iterations, converged)
  
  n <- nrow(A)
  
  # Initialize with uniform vector
  v <- rep(1 / sqrt(n), n)
  
  eigenvalue <- 0
  converged  <- FALSE
  
  for (iter in 1:max_iter) {
    # Matrix-vector multiplication: A %*% v
    v_new <- as.numeric(A %*% v)
    
    # Compute the norm (eigenvalue estimate)
    norm_v <- sqrt(sum(v_new^2))
    
    if (norm_v < .Machine$double.eps) {
      # Graph is disconnected or trivial
      warning("Power iteration: zero vector encountered. Graph may be disconnected.")
      break
    }
    
    # Normalize
    v_new <- v_new / norm_v
    
    # Check convergence: ||v_new - v|| < tol
    delta <- sqrt(sum((v_new - v)^2))
    
    if (delta < tol) {
      converged <- TRUE
      eigenvalue <- norm_v
      v <- v_new
      break
    }
    
    v <- v_new
    eigenvalue <- norm_v
  }
  
  # Normalize centrality to [0, 1]
  v_min <- min(v)
  v_max <- max(v)
  if (v_max > v_min) {
    centrality <- (v - v_min) / (v_max - v_min)
  } else {
    centrality <- rep(0.5, n)
  }
  
  names(centrality) <- rownames(A)
  
  return(list(
    centrality  = centrality,
    raw_eigenvector = v,
    eigenvalue  = eigenvalue,
    iterations  = iter,
    converged   = converged
  ))
}

# ---------- Degree Centrality (for comparison) ----------

degree_centrality <- function(A) {
  # Simple degree centrality: sum of edge weights per node
  degrees <- rowSums(A)
  # Normalize to [0, 1]
  if (max(degrees) > 0) {
    degrees <- degrees / max(degrees)
  }
  names(degrees) <- rownames(A)
  return(degrees)
}

# ---------- Graph Laplacian ----------

compute_laplacian <- function(A) {
  # Compute the graph Laplacian: L = D - A
  # D = diagonal matrix of node degrees
  D <- diag(rowSums(A))
  L <- D - A
  return(L)
}

# ---------- Spectral Community Detection ----------

spectral_communities <- function(A, k = 3) {
  # Detect k communities using spectral clustering on the graph Laplacian
  #
  # Algorithm:
  #   1. Compute Laplacian L = D - A
  #   2. Find the k smallest eigenvalues/eigenvectors of L
  #   3. Use the Fiedler vector (2nd smallest eigenvector) for bipartition
  #   4. Extend to k clusters using k-means on the eigenvector embedding
  #
  # We implement k-means from scratch too.
  
  n <- nrow(A)
  L <- compute_laplacian(A)
  
  # Eigendecomposition of Laplacian
  eig <- eigen(L, symmetric = TRUE)
  
  # Sort by ascending eigenvalue (smallest first)
  ord <- order(eig$values)
  eigenvalues  <- eig$values[ord]
  eigenvectors <- eig$vectors[, ord]
  
  # Take the k smallest non-trivial eigenvectors (skip the first which is constant)
  # These form the spectral embedding
  embedding <- eigenvectors[, 2:min(k + 1, ncol(eigenvectors)), drop = FALSE]
  
  # Scratch k-means on the embedding
  clusters <- scratch_kmeans(embedding, k = k, max_iter = 100)
  
  names(clusters) <- rownames(A)
  
  return(list(
    clusters     = clusters,
    eigenvalues  = eigenvalues[1:min(k + 1, length(eigenvalues))],
    embedding    = embedding
  ))
}

# ---------- Scratch K-Means ----------

scratch_kmeans <- function(X, k, max_iter = 100, seed = 42) {
  # K-means clustering from scratch
  # X: n x p data matrix
  # k: number of clusters
  
  set.seed(seed)
  X <- as.matrix(X)
  n <- nrow(X)
  p <- ncol(X)
  
  # Initialize centroids by random selection
  centroid_idx <- sample(1:n, k)
  centroids <- X[centroid_idx, , drop = FALSE]
  
  assignments <- rep(0, n)
  
  for (iter in 1:max_iter) {
    # Assignment step: assign each point to nearest centroid
    new_assignments <- rep(0, n)
    for (i in 1:n) {
      dists <- rowSums((centroids - matrix(X[i, ], nrow = k, ncol = p, byrow = TRUE))^2)
      new_assignments[i] <- which.min(dists)
    }
    
    # Check convergence
    if (all(new_assignments == assignments)) break
    assignments <- new_assignments
    
    # Update step: recompute centroids
    for (j in 1:k) {
      members <- which(assignments == j)
      if (length(members) > 0) {
        centroids[j, ] <- colMeans(X[members, , drop = FALSE])
      }
    }
  }
  
  return(assignments)
}

# ---------- Collusion Cluster Detection ----------

detect_collusion_clusters <- function(centrality_scores, A, threshold_quantile = 0.90) {
  # Flag high-centrality nodes as potential collusion hubs
  # Then identify their connected neighborhoods
  
  threshold <- quantile(centrality_scores, probs = threshold_quantile)
  hub_nodes <- names(centrality_scores[centrality_scores >= threshold])
  
  # For each hub, find its direct neighbors with strong connections
  clusters <- list()
  for (hub in hub_nodes) {
    hub_idx <- which(rownames(A) == hub)
    neighbors <- names(which(A[hub_idx, ] > 0))
    # Filter to only strongly connected neighbors
    strong_neighbors <- names(which(A[hub_idx, ] > median(A[A > 0])))
    clusters[[hub]] <- list(
      hub = hub,
      neighbors = strong_neighbors,
      total_weight = sum(A[hub_idx, ])
    )
  }
  
  return(list(
    hub_nodes  = hub_nodes,
    threshold  = threshold,
    clusters   = clusters
  ))
}

# ---------- Master Function ----------

run_network_analysis <- function(doctors, network_edges) {
  
  section_header("NETWORK ANALYSIS: COLLUSION DETECTION")
  
  # Get all unique doctor IDs
  all_nodes <- unique(c(network_edges$from, network_edges$to))
  # Only keep nodes that are doctors
  doctor_nodes <- intersect(all_nodes, doctors$doctor_id)
  
  cat(sprintf("  Nodes: %d | Edges: %d\n", length(doctor_nodes), nrow(network_edges)))
  
  # Step 1: Build adjacency matrix
  cat("  [1/4] Building adjacency matrix...\n")
  A <- build_adjacency_matrix(network_edges, doctor_nodes)
  cat(sprintf("  [✓] Adjacency matrix: %d x %d (density: %.4f)\n",
              nrow(A), ncol(A), sum(A > 0) / (nrow(A)^2)))
  
  # Step 2: Eigenvector centrality via power iteration
  cat("  [2/4] Computing eigenvector centrality (power iteration)...\n")
  eig_result <- eigenvector_centrality(A)
  cat(sprintf("  [✓] Converged in %d iterations (λ = %.4f)\n",
              eig_result$iterations, eig_result$eigenvalue))
  
  # Step 3: Detect collusion clusters
  cat("  [3/4] Detecting collusion clusters...\n")
  collusion <- detect_collusion_clusters(eig_result$centrality, A)
  cat(sprintf("  [✓] Found %d high-centrality hub nodes\n", length(collusion$hub_nodes)))
  
  # Step 4: Spectral community detection
  cat("  [4/4] Spectral community detection (Laplacian eigendecomposition)...\n")
  # Use a subset if the matrix is large
  if (length(doctor_nodes) > 200) {
    # Work on the top-connected subgraph for spectral analysis
    top_nodes <- names(sort(rowSums(A), decreasing = TRUE))[1:min(200, length(doctor_nodes))]
    A_sub <- A[top_nodes, top_nodes]
    communities <- spectral_communities(A_sub, k = 4)
    # Map back to full node set
    full_clusters <- rep(0, length(doctor_nodes))
    names(full_clusters) <- doctor_nodes
    full_clusters[top_nodes] <- communities$clusters
    communities$clusters <- full_clusters
  } else {
    communities <- spectral_communities(A, k = 4)
  }
  cat(sprintf("  [✓] Identified %d spectral communities\n", max(communities$clusters)))
  
  # Build results dataframe
  results <- data.frame(
    doctor_id            = doctor_nodes,
    eigenvector_centrality = eig_result$centrality[doctor_nodes],
    degree_centrality     = degree_centrality(A)[doctor_nodes],
    community            = communities$clusters[doctor_nodes],
    is_hub               = doctor_nodes %in% collusion$hub_nodes,
    stringsAsFactors     = FALSE
  )
  
  # Normalize centrality to anomaly score [0, 1]
  results$network_anomaly_score <- results$eigenvector_centrality
  
  return(list(
    results       = results,
    adjacency     = A,
    eig_result    = eig_result,
    collusion     = collusion,
    communities   = communities,
    laplacian_eigenvalues = communities$eigenvalues
  ))
}

cat("[✓] Network analysis engine loaded.\n")
