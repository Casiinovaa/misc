# Load libraries
library(data.table)
library(dplyr)
library(tidyr)
library(stats)
library(pracma)    # for normalization
library(ggplot2)

# Read your dataset (adjust path as needed)
df <- fread("first_25000_rows.csv")

# Ensure it is sorted by timestamp
df <- df[order(ts_event)]

### 1. BEST-LEVEL OFI (LEVEL 0) -----------------------------------------

df[, ofi_best := shift(bid_sz_00, type = "lag") - shift(ask_sz_00, type = "lag")]

### 2. MULTI-LEVEL OFI (LEVELS 0–9) -------------------------------------

# Compute OFI at each level
ofi_matrix <- data.table()
for (i in 0:9) {
  bid_col <- paste0("bid_sz_0", i)
  ask_col <- paste0("ask_sz_0", i)
  ofi_col <- paste0("ofi_l", i)
  ofi_matrix[[ofi_col]] <- shift(df[[bid_col]], type = "lag") - shift(df[[ask_col]], type = "lag")
}

# Combine into original data
df <- cbind(df, ofi_matrix)

### 3. INTEGRATED OFI (PCA-BASED) ---------------------------------------

# Remove NAs and run PCA
valid_rows <- complete.cases(ofi_matrix)
pca_result <- prcomp(ofi_matrix[valid_rows], center = TRUE, scale. = TRUE)

# Use first principal component and normalize it
weights <- pca_result$rotation[, 1]
weights <- weights / sum(abs(weights))  # normalize weights

# Apply weights to entire matrix (including NAs will result in NA)
ofi_integrated <- as.matrix(ofi_matrix) %*% weights
df$ofi_integrated <- NA
df$ofi_integrated[valid_rows] <- ofi_integrated

### 4. OPTIONAL: CROSS-ASSET OFI FORMAT -------------------------------

# If your dataset has multiple symbols, pivot for cross-impact estimation
if (length(unique(df$symbol)) > 1) {
  ofi_cross <- df[, .(ts_event, symbol, ofi_integrated)]
  wide_ofi <- pivot_wider(ofi_cross, names_from = symbol, values_from = ofi_integrated)
  # Save or use this wide format for LASSO modeling
}

### Export result
fwrite(df[, .(ts_event, ofi_best, ofi_integrated)], "ofi_features_output.csv")
