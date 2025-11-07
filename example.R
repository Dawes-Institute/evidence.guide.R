#!/usr/bin/env Rscript
# Example: Process multiple PDFs with Evidence Guide API
# This script demonstrates how to extract study data from multiple PDF files
# and combine them into a single data frame for analysis.

library(evidenceguide)

# Example 1: Process multiple PDFs at once
# -----------------------------------------
# The API will process all files in parallel and wait for all to complete

pdf_files <- c(
  "paper1.pdf",
  "paper2.pdf",
  "paper3.pdf"
)

# Process all PDFs (make sure files exist first)
existing_files <- pdf_files[file.exists(pdf_files)]

if (length(existing_files) > 0) {
  result <- eg_process(
    files = existing_files,
    mode = "text",           # or "vision" for image-based extraction
    interval = 2,            # check status every 2 seconds
    timeout = 600,           # wait up to 10 minutes
    progress = TRUE          # show progress bar
  )
  
  # The result contains:
  # - jobs: status for each file
  # - studies: combined data frame with all extracted studies
  
  cat("Processed", nrow(result$jobs), "files\n")
  cat("Extracted", nrow(result$studies), "total studies\n\n")
  
  # View the combined studies data frame
  print(result$studies)
  
  # Save to CSV for further analysis
  write.csv(result$studies, "extracted_studies.csv", row.names = FALSE)
}


# Example 2: Process files sequentially (useful for large batches)
# -----------------------------------------------------------------
# For very large sets of PDFs, you might want to process in batches

batch_process <- function(files, batch_size = 5) {
  all_studies <- list()
  
  for (i in seq(1, length(files), by = batch_size)) {
    batch <- files[i:min(i + batch_size - 1, length(files))]
    
    cat("Processing batch", ceiling(i / batch_size), "...\n")
    result <- eg_process(files = batch, mode = "text", progress = TRUE)
    
    all_studies[[length(all_studies) + 1]] <- result$studies
  }
  
  # Combine all batches into single data frame
  dplyr::bind_rows(all_studies)
}

# Usage:
# all_data <- batch_process(list_of_pdf_files, batch_size = 5)


# Example 3: Work with the data
# ------------------------------
# The studies data frame contains these columns:
# - job_id: internal job identifier
# - doi: paper DOI
# - title: paper title
# - year: publication year
# - journal: journal name
# - study_number: study identifier within paper
# - design: experimental design description
# - hypothesis: study hypothesis
# - test: test statistic type (F, t, Chi-square, etc.)
# - stat_value: test statistic value
# - df: degrees of freedom
# - n: sample size
# - p: p-value

# Example analysis:
if (exists("result") && nrow(result$studies) > 0) {
  # Filter significant results
  significant <- subset(result$studies, p < 0.05)
  
  # Summary by test type
  table(result$studies$test)
  
  # View papers with most studies
  study_counts <- aggregate(
    study_number ~ doi + title, 
    data = result$studies, 
    FUN = length
  )
  study_counts[order(-study_counts$study_number), ]
}
