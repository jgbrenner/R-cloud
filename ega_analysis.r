renv::init()
renv::status()

###############################################################################
# 0. MINIMAL INSTALLATION SECTION
###############################################################################

# Define packages you need:
packages_needed <- c(
  "lavaan",       # For CFA
  "readr",        # For reading CSV
  "psych",        # EFA + psychometrics
  "qgraph",       # Graphs for EFA/EGA
  "GPArotation",  # Factor rotations (varimax, etc.)
  "EGAnet",       # EGA + bootEGA
  "semPlot",      # For semPaths (CFA diagrams)
  "ggplot2",      # Optional for extra visuals
  "dplyr",        # Data manipulation
  "reshape2"      # Data reshaping
)

# 1. Check what's already installed to avoid re-installs
installed_pkgs <- installed.packages()[, "Package"]
to_install <- setdiff(packages_needed, installed_pkgs)

if (length(to_install) > 0) {
  message("\nInstalling only minimal dependencies on Windows:\n")
  print(to_install)
  # Use minimal dependencies (Depends, Imports):
  install.packages(to_install, dependencies = c("Depends","Imports"))
} else {
  message("All required packages already installed.")
}

# Load everything
invisible(lapply(packages_needed, require, character.only = TRUE))

###############################################################################
# 1. LOAD AND PREPARE DATA
###############################################################################
# CSV import from GitHub
github_url <- "https://raw.githubusercontent.com/jgbrenner/EGA/refs/heads/main/cleaned_perfectionism_data.csv"
data_raw <- read_csv(github_url)

# Re-label columns for convenience
question_columns <- c(
  "1. Czuję, że muszę być doskonały we wszystkim, co robię.",
  "2. Gdy nie osiągam swoich wysokich standardów, czuję się zawiedziony/a.",
  "3. Nawet drobne błędy wywołują u mnie silny dyskomfort.",
  "4. Czuję, że muszę być lepszy od innych, aby móc się czuć spełniony.",
  "5. Porównuję się z innymi, aby ocenić swoje osiągnięcia.",
  "6. Uważam, że nie mogę sobie pozwolić na porażkę.",
  "7. Oczekuję, że inni będą wykonywać swoje zadania perfekcyjnie.",
  "8. Krytycznie oceniam osiągnięcia innych ludzi.",
  "9. Czuję, że inni mogą zrobić coś lepiej niż to zrobili.",
  "10. Nie toleruję błędów popełnianych przez innych.",
  "11. Czuję się sfrustrowany, gdy inni nie spełniają moich oczekiwań.",
  "12. Wyrażam swoje niezadowolenie z pracy innych.",
  "13. Czuję, że inni oczekują ode mnie doskonałości.",
  "14. Mam wrażenie, że muszę spełniać wysokie oczekiwania innych.",
  "15. Czuję presję, aby być idealnym w oczach innych.",
  "16. Obawiam się, że zawiodę oczekiwania innych.",
  "17. Czuję, że muszę udowodnić swoją wartość innym.",
  "18. Myślę o tym, co inni sądzą o moich osiągnięciach."
)

data_cfa <- data_raw[, question_columns]
colnames(data_cfa) <- paste0("Q", 1:18)

###############################################################################
# 2. FULL ANALYSIS (WITH Q5)
###############################################################################
cat("\n=========================\nFULL ANALYSIS (with Q5)\n=========================\n")

# Updated EFA (Promax) Analysis and Circular Plot

cat("\n--- Parallel Analysis (with Q5) ---\n")
fa.parallel(data_cfa, fa = "fa", fm = "ml", n.iter = 100)

cat("\n--- Scree Plot (with Q5) ---\n")
scree(data_cfa, factors = TRUE, pc = FALSE, main = "Scree Plot (with Q5)")

cat("\n--- EFA (Promax) with Q5 ---\n")
efa_promax <- fa(data_cfa, nfactors = 3, rotate = "promax", fm = "ml")
efa_loadings_matrix <- as.matrix(efa_promax$loadings)

# Replace NA with 0
efa_loadings_matrix[is.na(efa_loadings_matrix)] <- 0

# Filter loadings below 0.3 for clarity
filtered_loadings <- efa_loadings_matrix
filtered_loadings[abs(filtered_loadings) < 0.3] <- 0

# Define item labels and groups
item_labels <- paste0("Q", 1:18)
groups <- list("ML1" = 1:6, "ML2" = 7:12, "ML3" = 13:18)

# Set plot parameters
options(repr.plot.width = 22, repr.plot.height = 15)  # Adjust canvas size

# Plot EFA loadings using the preferred style
qgraph(
  filtered_loadings,
  layout = "circle",                 # Use the circle layout
  vsize = 6,                         # Node size
  labels = item_labels,              # Use Q1–Q18 as labels
  edge.width = abs(filtered_loadings) * 1.5,  # Scale edge widths
  edge.color = "black",              # Edge color
  edge.labels = TRUE,                # Show edge labels
  label.cex = 1.5,                   # Adjust label size
  repulsion = TRUE,                  # Enable repulsion for readability
  borders = TRUE,                    # Add borders for clarity
  groups = groups,                   # Define groups for factors
  color = c("lightblue", "lightgreen", "lightpink")  # Different colors for factors
)


## 2C. CFA (with Q5)
cat("\n--- CFA (with Q5) ---\n")
cfa_model <- '
  PSS =~ Q1 + Q2 + Q3 + Q4 + Q5 + Q6
  PSI =~ Q7 + Q8 + Q9 + Q10 + Q11 + Q12
  PSP =~ Q13 + Q14 + Q15 + Q16 + Q17 + Q18
'
fit <- cfa(cfa_model, data = data_cfa)
summary(fit, fit.measures = TRUE, standardized = TRUE)

# CFA path diagram
semPaths(
  fit,
  what = "std",
  layout = "circle2",
  residuals = TRUE,
  edge.color = "black",
  edge.label.cex = 0.8,
  label.cex = 1.2,
  style = "ram",
  exoCov = TRUE,
  rotation = 3
)

## 2D. EGA (with Q5, walktrap)
cat("\n--- EGA (with Q5, walktrap) ---\n")
ega_result <- EGA(
  data = data_cfa,
  model = "glasso",
  algorithm = "walktrap", 
  plot.EGA = FALSE
)
print(ega_result)
plot(ega_result, plot.type = "qgraph", title = "EGA Network (with Q5)", legend = TRUE)

## 2E. bootEGA (with Q5, walktrap)
cat("\n--- bootEGA (with Q5, walktrap) ---\n")
set.seed(123)
boot_ega_result <- bootEGA(
  data = data_cfa,
  iter = 1000,
  type = "parametric",
  ncores = 1,
  seed = 123,
  typicalStructure = TRUE,
  algorithm = "walktrap"
)
print(boot_ega_result)
cat("\n--- bootEGA Dimensions (with Q5) ---\n")
print(boot_ega_result$boot.ndim)
cat("\n--- bootEGA Summary Table (with Q5) ---\n")
print(boot_ega_result$summary.table)
cat("\n--- bootEGA Dimension Frequency (with Q5) ---\n")
print(boot_ega_result$frequency)

if(!is.null(boot_ega_result$typicalStructure)) {
  plot(boot_ega_result, type = "typical", title = "Typical Structure (with Q5)")
}

###############################################################################
# 3. ANALYSIS WITHOUT Q5
###############################################################################
cat("\n===========================\nANALYSIS WITHOUT Q5\n===========================\n")

## 3A. Remove Q5
data_cfa_noQ5 <- data_cfa[, -5]

## 3B. EFA (Promax, No Q5)
cat("\n--- Parallel Analysis (No Q5) ---\n")
fa.parallel(data_cfa_noQ5, fa = "fa", fm = "ml", n.iter = 100)

cat("\n--- Scree Plot (No Q5) ---\n")
scree(data_cfa_noQ5, factors = TRUE, pc = FALSE, main = "Scree Plot (No Q5)")

cat("\n--- EFA (Promax) without Q5 ---\n")
efa_promax_noQ5 <- fa(data_cfa_noQ5, nfactors = 3, rotate = "promax", fm = "ml")
print(efa_promax_noQ5$loadings, cutoff = 0.3)

## Plot EFA Loadings in a Circle for No Q5
cat("\n--- EFA Circular Plot (No Q5) ---\n")

# Convert EFA loadings to a matrix
efa_noQ5_mat <- as.matrix(efa_promax_noQ5$loadings)

# Replace NA with 0 for better visualization
efa_noQ5_mat[is.na(efa_noQ5_mat)] <- 0

# Filter out loadings below 0.3 for clarity
efa_noQ5_filtered <- efa_noQ5_mat
efa_noQ5_filtered[abs(efa_noQ5_filtered) < 0.3] <- 0

# Define item labels for better visualization
item_labels_noQ5 <- paste0("Q", c(1:4, 6:18))  # Excluding Q5

# Adjusted plotting parameters
options(repr.plot.width = 22, repr.plot.height = 15)  # Adjust canvas size

qgraph(
  efa_noQ5_filtered,
  layout = "circle",                  # Circle layout for clean visuals
  vsize = 6,                          # Node size
  labels = item_labels_noQ5,          # Updated labels excluding Q5
  edge.width = abs(efa_noQ5_filtered) * 1.5,  # Scaled edge widths
  edge.color = "black",               # Standard edge color
  edge.labels = TRUE,                 # Show edge labels
  label.cex = 1.5,                    # Adjust label size
  repulsion = TRUE,                   # Add repulsion for better spacing
  borders = TRUE,                     # Add node borders for clarity
  groups = list("ML1" = 1:5, "ML2" = 6:11, "ML3" = 12:17),  # Define factor groups
  color = c("lightblue", "lightgreen", "lightpink"),  # Factor group colors
  asize = 18                          # Adjust as needed
)


## 3C. CFA (No Q5)
cat("\n--- CFA (No Q5) ---\n")
cfa_model_noQ5 <- '
  PSS =~ Q1 + Q2 + Q3 + Q4 + Q6
  PSI =~ Q7 + Q8 + Q9 + Q10 + Q11 + Q12
  PSP =~ Q13 + Q14 + Q15 + Q16 + Q17 + Q18
'
fit_noQ5 <- cfa(cfa_model_noQ5, data = data_cfa_noQ5)
summary(fit_noQ5, fit.measures = TRUE, standardized = TRUE)

semPaths(
  fit_noQ5,
  what = "std",
  layout = "circle2",
  residuals = TRUE,
  edge.color = "black",
  edge.label.cex = 0.8,
  label.cex = 1.2,
  style = "ram",
  exoCov = TRUE,
  rotation = 3
)

## 3D. EGA (No Q5, walktrap)
cat("\n--- EGA (No Q5, walktrap) ---\n")
ega_result_noQ5 <- EGA(
  data = data_cfa_noQ5,
  model = "glasso",
  algorithm = "walktrap",
  plot.EGA = FALSE
)
print(ega_result_noQ5)
plot(ega_result_noQ5, plot.type = "qgraph", title = "EGA Network (No Q5)", legend = TRUE)

## 3E. bootEGA (No Q5, walktrap)
cat("\n--- bootEGA (No Q5, walktrap) ---\n")
set.seed(123)
boot_ega_result_noQ5 <- bootEGA(
  data = data_cfa_noQ5,
  iter = 1000,
  type = "parametric",
  ncores = 1,
  seed = 123,
  typicalStructure = TRUE,
  algorithm = "walktrap"
)
print(boot_ega_result_noQ5)
cat("\n--- bootEGA Dimensions (No Q5) ---\n")
print(boot_ega_result_noQ5$boot.ndim)
cat("\n--- bootEGA Summary Table (No Q5) ---\n")
print(boot_ega_result_noQ5$summary.table)
cat("\n--- bootEGA Dimension Frequency (No Q5) ---\n")
print(boot_ega_result_noQ5$frequency)

if(!is.null(boot_ega_result_noQ5$typicalStructure)) {
  plot(boot_ega_result_noQ5, type = "typical", title = "Typical Structure (No Q5)")
}

cat("\n--- END OF SCRIPT ---\n")
