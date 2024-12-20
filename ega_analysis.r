install.packages("semPlot")
install.packages("lavaan")
install.packages("readr")

# Load required library
library(readr)

# Define the GitHub raw file URL
github_url <- "https://raw.githubusercontent.com/jgbrenner/EGA/refs/heads/main/cleaned_perfectionism_data.csv"

# Read the CSV file directly from GitHub
data <- read_csv(github_url)

# Display the first few rows
print(head(data))

# Rename the columns for the 18 test questions
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

# Select only the relevant columns and rename them
data_cfa <- data[, question_columns]
colnames(data_cfa) <- paste0("Q", 1:18)

# Display the first few rows of the renamed dataset
print(head(data_cfa))

library(lavaan)

# Define the CFA model
model <- '
  PSS =~ Q1 + Q2 + Q3 + Q4 + Q5 + Q6
  PSI =~ Q7 + Q8 + Q9 + Q10 + Q11 + Q12
  PSP =~ Q13 + Q14 + Q15 + Q16 + Q17 + Q18
'

# Fit the CFA model
fit <- cfa(model, data = data_cfa)

# Display the summary with fit indices and standardized estimates
summary(fit, fit.measures = TRUE, standardized = TRUE)

install.packages("semPlot")
library(semPlot)

# Set margins (bottom, left, top, right)
par(mar = c(5, 5, 5, 5))  # Adjust as needed

# Optionally set canvas size (for R Markdown or interactive sessions)
options(repr.plot.width = 12, repr.plot.height = 12)  # Set width and height

# Create your SEM path diagram with adjusted parameters
semPaths(
  fit,
  what = "std",
  layout = "circle2",
  residuals = TRUE,
  edge.color = c("black"),
  edge.label.cex = 0.8,
  label.cex = 1.2,
  style = "ram",
  exoCov = TRUE,
  rotation = 3
)

library(psych)
library(qgraph)
library(GPArotation)

# Perform EFA with 3 factors (adjust the number of factors as needed)
efa_result <- fa(data_cfa, nfactors = 3, rotate = "oblimin", fm = "ml")

# Print factor loadings
print(efa_result$loadings, cutoff = 0.3)

# Extract the loadings matrix and convert to a matrix
loadings_matrix <- as.matrix(efa_result$loadings)

# Replace any NA values with 0 to ensure compatibility with qgraph
loadings_matrix[is.na(loadings_matrix)] <- 0

# Print the prepared loadings matrix
print(loadings_matrix)

# Prepare a custom layout to spread nodes
n_factors <- 3
n_items <- nrow(loadings_matrix)

layout_matrix <- matrix(NA, nrow = n_items + n_factors, ncol = 2)

# Factors at the top
layout_matrix[1:n_factors, ] <- cbind(1:n_factors, 5)

# Items spaced below each factor
layout_matrix[(n_factors + 1):nrow(layout_matrix), ] <- cbind(
  rep(1:n_factors, each = n_items / n_factors),
  seq(4, 1, length.out = n_items / n_factors)
)

# Verify the layout
print(layout_matrix)

cat("\nFiltered Loadings Matrix (Line by Line):\n")
apply(filtered_loadings, 1, function(row) {
  cat(row, "\n")
})


# Define item labels only (Q1–Q18)
item_labels <- paste0("Q", 1:18)

# Define a clean graph with adjusted parameters
options(repr.plot.width = 22, repr.plot.height = 15)  # Adjust canvas size

# Original loadings matrix
cat("\nOriginal Loadings Matrix:\n")
print(loadings_matrix, digits = 3)

# Filtered loadings matrix (values below the threshold set to zero)
cat("\nFiltered Loadings Matrix (Threshold = 0.3):\n")
print(filtered_loadings, digits = 3)

# Define item labels (Q1–Q18)
item_labels <- paste0("Q", 1:18)

# Improved edge scaling: Linear scaling with a minimum width
scaled_widths <- abs(filtered_loadings) * 2  # Linear scaling
scaled_widths[scaled_widths < 0.5] <- 0.5   # Set a minimum width for visibility

# Plot the EFA graph with improved scaling
options(repr.plot.width = 22, repr.plot.height = 15)  # Adjust canvas size

qgraph(
  filtered_loadings,
  layout = "circle",                  # Circle layout
  vsize = 6,                          # Node size
  labels = item_labels,               # Correct item labels Q1-Q18 only
  edge.width = scaled_widths,         # Apply improved scaling
  edge.color = "black",               # Edge color
  edge.labels = TRUE,                 # Show edge labels
  label.cex = 1.2,                    # Label size
  borders = TRUE,                     # Add borders for clarity
  groups = list("ML1" = 1:6, "ML2" = 7:12, "ML3" = 13:18), # Define factor groups
  color = c("lightblue", "lightgreen", "lightpink")  # Colors for ML1, ML2, ML3
)

# Add a legend for clarity
legend("bottom",
       legend = c("ML1 = Items 1–6", "ML2 = Items 7–12", "ML3 = Items 13–18"),
       fill = c("lightblue", "lightgreen", "lightpink"),
       title = "Factors",
       cex = 1.2,
       bty = "n",
       horiz = TRUE)

# Identify cross-loadings: Items with significant loadings on multiple factors
cross_loadings <- apply(abs(loadings_matrix) > 0.3, 1, sum) > 1
if (any(cross_loadings)) {
  cat("\nItems with Cross-Loadings (Loadings > 0.3 on Multiple Factors):\n")
  print(which(cross_loadings))
} else {
  cat("\nNo cross-loadings detected.\n")
}


qgraph(
  filtered_loadings,
  layout = "circle",                 # Use the spring layout
  vsize = 6,                         # Node size
  labels = item_labels,              # Use only Q1–Q18 as labels
  edge.width = abs(filtered_loadings) * 1.5,  # Scale edge widths
  edge.color = "black",              # Edge color
  edge.labels = TRUE,                # Show edge labels
  label.cex = 1.5,
  repulsion = TRUE,                  # Adjust label size
  borders = TRUE,                    # Add borders for clarity
  groups = list("ML1" = 1:6, "ML2" = 7:12, "ML3" = 13:18), # Define groups
  color = c("lightblue", "lightgreen", "lightpink"),  # Different colors for factors
  asize = 18                     # Set as NULL or omit if not needed
)

  
