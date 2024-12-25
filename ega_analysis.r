# --------------------------------------------------
# 1. Install and Load Required Packages
# --------------------------------------------------
install.packages(
  c("semPlot", "lavaan", "readr", 
    "psych", "qgraph", "GPArotation", "EGAnet"),
  dependencies = TRUE
)

library(readr)
library(lavaan)
library(semPlot)
library(psych)
library(qgraph)
library(GPArotation)
library(EGAnet)

# --------------------------------------------------
# 2. Import and Prepare Data
# --------------------------------------------------
github_url <- "https://raw.githubusercontent.com/jgbrenner/EGA/refs/heads/main/cleaned_perfectionism_data.csv"
data <- read_csv(github_url)

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

data_cfa <- data[, question_columns]
colnames(data_cfa) <- paste0("Q", 1:18)

# --------------------------------------------------
# 3. Confirmatory Factor Analysis (CFA)
# --------------------------------------------------
cfa_model <- '
  PSS =~ Q1 + Q2 + Q3 + Q4 + Q5 + Q6
  PSI =~ Q7 + Q8 + Q9 + Q10 + Q11 + Q12
  PSP =~ Q13 + Q14 + Q15 + Q16 + Q17 + Q18
'

fit <- cfa(cfa_model, data = data_cfa)
summary(fit, fit.measures = TRUE, standardized = TRUE)

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

# --------------------------------------------------
# 4. Exploratory Factor Analysis (EFA)
# --------------------------------------------------
# (a) EFA with oblimin rotation
efa_result_oblimin <- fa(data_cfa, nfactors = 3, rotate = "oblimin", fm = "ml")
print(efa_result_oblimin$loadings, cutoff = 0.3)

# (b) EFA with promax rotation
efa_result_promax <- fa(data_cfa, nfactors = 3, rotate = "promax", fm = "ml")
print(efa_result_promax$loadings, cutoff = 0.3)

# Scree plot (parallel analysis)
fa.parallel(data_cfa, fa = "fa", fm = "ml", n.iter = 100)

# --------------------------------------------------
# 5. Bootstrapped Exploratory Graph Analysis (bootEGA)
# --------------------------------------------------
set.seed(123)  # For reproducibility
boot_result <- bootEGA(
  data = data_cfa,
  iter = 1000,
  type = "parametric",
  ncores = 1,  
  seed = 123
)
print(boot_result)
print(boot_result$boot.ndim)      
print(boot_result$summary.table)  
print(boot_result$frequency)      

# Re-run with typicalStructure enabled for median structure
set.seed(123)
boot_result_with_structure <- bootEGA(
  data = data_cfa,
  iter = 1000,
  type = "parametric",
  ncores = 1,
  typicalStructure = TRUE,
  seed = 123
)

print(boot_result_with_structure$typicalStructure)
# Optional: plot of the typical structure
if (!is.null(boot_result_with_structure$typicalStructure)) {
  plot(boot_result_with_structure, type = "typical")
}

# --------------------------------------------------
# 6. Q5-Specific Analyses
# --------------------------------------------------
# Extract Q5 loadings from EFA (promax)
efa_loadings <- efa_result_promax$loadings
q5_loadings <- efa_loadings["Q5", ]
cat("\nPromax Loadings for Q5:\n")
print(q5_loadings)

# Bootstrap membership for Q5 ("V05" in bootEGA)
q5_membership <- boot_result_with_structure$boot.wc[, "V05"]
cat("\nBootstrap Factor Assignments for Q5:\n")
print(table(q5_membership))

# (a) Item-rest correlation
total_minus_q5 <- rowSums(data_cfa[, -5])
q5_item_rest_corr <- cor(data_cfa$Q5, total_minus_q5, use = "pairwise.complete.obs")
cat("\nItem-Rest Correlation for Q5:\n", q5_item_rest_corr, "\n")

# (b) Cronbach’s alpha for PSP with/without Q5 
psp_items <- data_cfa[, c("Q13", "Q14", "Q15", "Q16", "Q17", "Q18")]
alpha_with_q5 <- alpha(psp_items)
alpha_without_q5 <- alpha(psp_items[, -1])  # Example removing Q13
cat("\nCronbach’s Alpha for PSP items:\n")
cat("With Q13–Q18: ", alpha_with_q5$total$std.alpha, "\n")
cat("Without Q13: ", alpha_without_q5$total$std.alpha, "\n")

# (c) Dimensional Stability
dim_stability <- dimensionStability(boot_result_with_structure)
cat("\nDimension Stability:\n")
print(dim_stability)

# (d) Correlation of Q5 with items in the PSP factor
q5_factor_corr <- cor(
  data_cfa[, c("Q5", "Q13", "Q14", "Q15", "Q16", "Q17", "Q18")],
  use = "pairwise.complete.obs"
)
cat("\nCorrelations Between Q5 and PSP Items:\n")
print(q5_factor_corr)

# --------------------------------------------------
# 7. Optional: Visualize EFA Loadings (Oblimin) in qgraph
# --------------------------------------------------
loadings_matrix <- as.matrix(efa_result_oblimin$loadings)
loadings_matrix[is.na(loadings_matrix)] <- 0

# Zero out loadings < 0.3 for clarity
filtered_loadings <- loadings_matrix
filtered_loadings[abs(filtered_loadings) < 0.3] <- 0

qgraph(
  filtered_loadings,
  layout = "circle",
  vsize = 6,
  labels = paste0("Q", 1:18),
  edge.width = abs(filtered_loadings) * 1.5,
  edge.color = "black",
  edge.labels = TRUE,
  label.cex = 1.2,
  borders = TRUE,
  groups = list("ML1" = 1:6, "ML2" = 7:12, "ML3" = 13:18),
  color = c("lightblue", "lightgreen", "lightpink")
)

# Identify any cross-loadings above 0.3
cross_loadings <- apply(abs(loadings_matrix) > 0.3, 1, sum) > 1
if (any(cross_loadings)) {
  cat("\nItems with Cross-Loadings (Loadings > 0.3 on Multiple Factors):\n")
  print(which(cross_loadings))
} else {
  cat("\nNo cross-loadings detected.\n")
}

cat("\n--- End of Essential Analysis Script ---\n")

# 1. Run EGA to estimate the number of dimensions and the network
ega_result <- EGA(
  data_cfa,
  model = "glasso",     # or "TMFG"
  plot.EGA = FALSE      # We'll plot after capturing the result
)

# 2. Plot the network found by EGA
plot(ega_result, plot.type = "qgraph")

# (Optional) Print EGA summary
print(ega_result)
