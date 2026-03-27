# =============================================================================
# run.R — One-Click Launcher for the Anomaly Detection Engine
# =============================================================================
# Usage: Open RStudio, set working directory to this project folder, then:
#   source("run.R")
#
# Or from terminal:
#   Rscript run.R
# =============================================================================

# Set working directory to the script's location
if (interactive()) {
  # In RStudio
  tryCatch({
    setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  }, error = function(e) {
    message("Note: Could not auto-set working directory. Please set it manually to the project folder.")
  })
}

# ── Load all engine modules ──
cat("Loading engine modules...\n")
source("R/05_utils.R")
source("R/01_generate_data.R")
source("R/02_mvg_engine.R")
source("R/03_network_analysis.R")
source("R/04_benford_law.R")
cat("\n")

# ── Check for required packages ──
required_packages <- c("ggplot2", "knitr", "rmarkdown", "gridExtra", "scales")
missing <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

if (length(missing) > 0) {
  cat("Installing required packages:", paste(missing, collapse = ", "), "\n")
  install.packages(missing, repos = "https://cloud.r-project.org")
}

# ══════════════════════════════════════════════════════════════════
# RENDER THE COMPLETE REPORT
# ══════════════════════════════════════════════════════════════════

# Tell rmarkdown where to find the newly installed Pandoc
Sys.setenv(RSTUDIO_PANDOC="C:/Users/asus/AppData/Local/Pandoc")

print_banner()
cat("Rendering the Anomaly Detection Dossier...\n")
cat("This will generate data, run all engines, and produce a full HTML report.\n\n")

output_file <- rmarkdown::render(
  input       = "report.Rmd",
  output_file = "Healthcare_Anomaly_Detection_Report.html",
  output_dir  = ".",
  quiet       = FALSE
)

section_header("REPORT GENERATED SUCCESSFULLY")
cat(sprintf("  Output: %s\n", normalizePath(output_file)))
cat("  Open the HTML file in your browser to view the full dossier.\n\n")

# Auto-open in browser
if (interactive()) {
  browseURL(output_file)
}

cat("╔══════════════════════════════════════════════════════════════════╗\n")
cat("║                    ENGINE RUN COMPLETE                         ║\n")
cat("╚══════════════════════════════════════════════════════════════════╝\n")
