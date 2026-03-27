# Healthcare Provider Anomaly Detection Engine

[![View Live Report](https://img.shields.io/badge/View_Live-HTML_Dossier-8b5cf6?style=for-the-badge)](https://htmlpreview.github.io/?https://github.com/apuravmanhas/healthcare-anomaly-engine/blob/main/Healthcare_Anomaly_Detection_Report.html)

![R](https://img.shields.io/badge/Language-Pure_R-blue)
![Dependencies](https://img.shields.io/badge/Dependencies-Zero-brightgreen)
![Status](https://img.shields.io/badge/Build-Passing-success)

A zero-dependency statistical fraud detection engine built in pure R. This project mathematically flags suspicious healthcare providers (inflated billing, statistically impossible consultation volumes, fake review patterns, and network collusion rings) **using scratch-built linear algebra without any pre-built Machine Learning packages.**

## 🧠 The Math Behind the Engine

To prove an understanding of the algorithms beneath standard data science libraries, this engine implements three orthogonal detection methods entirely from scratch:

### 1. Multivariate Gaussian Probability Density
Each provider is modeled as a point in multidimensional feature space.
* **Covariance Matrix:** $\Sigma = \frac{1}{m-1}(X-\mu)^T(X-\mu)$ computed via raw matrix multiplication `%*%` (the `cov()` function is explicitly not used).
* **Probability Density:** Calculates the exact probability $p(x)$ of observing a provider's feature combination under the estimated normal distribution.
* **Mahalanobis Distance:** $D_M = \sqrt{(x-\mu)^T\Sigma^{-1}(x-\mu)}$ calculates anomaly magnitude.

### 2. Eigenvector Centrality (Network Collusion)
Fraudulent providers and fake review rings often operate in connected hubs.
* Constructs adjacency matrices natively from referral edge lists.
* Computes Eigenvector Centrality using **Power Iteration**: $v_{k+1} = \frac{Av_k}{||Av_k||}$ until convergence (< 1e-8 tolerance).
* Flags high-centrality nodes as potential collusion hubs and detects communities via spectral eigendecomposition.

### 3. Benford's Law (Digital Forensics)
Analyzes the leading-digit distribution of billing transaction logs to detect fabricated financial data.
* Implements leading-digit extraction logic.
* Computes expected logarithmic distribution $P(d) = \log_{10}(1 + 1/d)$.
* Calculates Pearson's $\chi^2$ Goodness-of-Fit test and Mean Absolute Deviation (MAD) from scratch to flag non-conforming practitioners.

## 🏗️ Project Architecture

```text
├── R/
│   ├── 01_generate_data.R        # Injects 4 types of synthetic anomalies & collusion rings
│   ├── 02_mvg_engine.R           # Scratch-built Covariance & Multivariate Gaussian PDF
│   ├── 03_network_analysis.R     # Power iteration centrality & spectral community detection
│   ├── 04_benford_law.R          # Forensics: Leading-digit extraction & Chi-Squared test
│   └── 05_utils.R                # Composite risk scoring & matrix regularization helpers
├── run.R                         # One-click pipeline launcher
└── report.Rmd                    # Generates the automated HTML dossier
```

## 🚀 How to Run

1. Clone this repository.
2. Ensure you have the required reporting packages installed (`ggplot2`, `rmarkdown`, `knitr`).
3. Set your working directory to the project folder and run:
```R
Rscript run.R
```
4. Or, open RStudio, open `run.R`, and click **Source**.

The script will synthesize a dataset of 500 mock doctors, perform the matrix algebra real-time, rank the providers by composite fraud risk, and generate a jaw-dropping dark-themed `Healthcare_Anomaly_Detection_Report.html` dossier.

## 📊 The Output Dossier

The engine automatically generates a professional, multi-page HTML report containing:
* **Composite Risk Scoring:** Weighted fusion of all three methods.
* **Visual Diagnostics:** High-density `ggplot2` visualizations including Mahalanobis distance scatter plots, feature correlation heatmaps, network centrality histograms, and Benford digit distribution comparisons.
* **Performance Metrics:** Precision, Recall, F1-Score, and a confusion matrix validating the engine's capability to natively detect the injected anomalies.

---
*Disclaimer: This project uses entirely synthetic mock data generated via statistical distributions. No real patient, clinical, or provider data is utilized.*
