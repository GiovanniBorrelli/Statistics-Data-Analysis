# In questo notebook sono stati creati gli istogrammi, i botplox e i botplox a intaglio, e i diagrammi di Pareto

hist_dataset <- dataset[dataset$Age == "Total" & dataset$Sex == "All persons" & dataset$Series == "Unemployment", c("Country", "Time", "Value")]
hist_dataset <- subset(hist_dataset, !grepl("OECD countries", Country))
dataset <- subset(dataset, !grepl("OECD countries", Country))


install.packages("tidyr")
library(ggplot2)
library(tidyr)

directory_path <- "C:/Users/giova/Documents/R PROJECT/IstogrammaTime"

# Crea l'istogramma per ogni valore unico di "Time"
for (anno in unique(hist_dataset$Time)) {

  subset_data <- hist_dataset[hist_dataset$Time == anno, ]
  
  grafico_istogramma <- ggplot(subset_data, aes(x = Value)) +
    geom_histogram(binwidth = 500, fill = "skyblue", color = "black", alpha = 0.7) +
    labs(title = sprintf("Distribuzione di Frequenza di Disoccupati per paese, Anno %s", anno),
         x = "Numero di Disoccupati in migliaia",
         y = "Frequenza") +
    theme_minimal()
  
  ggsave(file.path(directory_path, sprintf("Histogram_%s.pdf", anno)), grafico_istogramma, width = 8, height = 6)
}

directory_path <- "C:/Users/giova/Documents/R PROJECT/Boxplot"

for (anno in unique(hist_dataset$Time)) {

  subset_data <- hist_dataset[hist_dataset$Time == anno, ]
  
  grafico_boxplot <- ggplot(subset_data, aes(x = as.factor(1), y = Value)) +
    geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
    labs(title = sprintf("Boxplot a Intaglio della Distribuzione di Disoccupati, Anno %s", anno),
         x = "Anno",
         y = "Numero di Disoccupati in migliaia") +
    theme_minimal()
  
  ggsave(file.path(directory_path, sprintf("Boxplot_%s.pdf", anno)), grafico_boxplot, width = 12, height = 8)
}

# Crea un unico boxplot a intaglio per ogni valore unico di "Time"
for (anno in unique(hist_dataset$Time)) {

  subset_data <- hist_dataset[hist_dataset$Time == anno, ]
  
  grafico_boxplot <- ggplot(subset_data, aes(x = as.factor(1), y = Value)) +
    geom_boxplot(notch = TRUE, outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
    labs(title = sprintf("Boxplot a Intaglio della Distribuzione di Disoccupati, Anno %s", anno),
         x = "Anno",
         y = "Numero di Disoccupati in migliaia") +
    theme_minimal()
  
  ggsave(file.path(directory_path, sprintf("BoxplotIntaglio_%s.pdf", anno)), grafico_boxplot, width = 12, height = 8)
}

directory_path <- "C:/Users/giova/Documents/R PROJECT/Pareto"
install.packages("dplyr")
library(dplyr)
library(qcc)
graphics.off() 
par("mar") 
par(mar=c(1,1,1,1))
pareto.chart(pareto_data$Value, main = "Diagramma di Pareto", ylab = "Frequenza", col = "steelblue")

library(ggplot2)

for (year in 2010:2022) {

  filtered_data <- hist_dataset %>% filter(Time == year)
  
  ordered_data <- filtered_data %>% arrange(desc(Value))
  ordered_data$Country[ordered_data$Country == "United Kingdom"] <- "UK"
  
  Countries <- ordered_data$Country
  Values <- ordered_data$Value
  
  pdf_path <- file.path(directory_path, paste0("pareto_chart_", year, ".pdf"))
  pdf(pdf_path)
  pareto.chart(Values, 
               ylab = "Frequenza di Disoccupati",
               names.arg = Countries,
               main = paste("Pareto Chart -", year))
  dev.off()
  
  system(paste("start", shQuote(pdf_path)))
}

newhist_dataset <- hist_dataset
newhist_dataset$ValDis <- newhist_dataset$Value
newhist_dataset$Value <- NULL  # Rimuove la vecchia colonna "Value"
temp_dataset <- dataset[dataset$Age == "Total" & dataset$Sex == "All persons" & dataset$Series == "Population", c("Country","Value")]
temp_dataset$ValPop <- temp_dataset$Value
temp_dataset$Value <- NULL
final_dataset <- cbind(newhist_dataset, temp_dataset)
temp_dataset <- dataset[dataset$Age == "Total" & dataset$Sex == "All persons" & dataset$Series == "Labour Force", c("Country","Value")]
temp_dataset$ValFL <- temp_dataset$Value
temp_dataset$Value <- NULL
final_dataset <- cbind(final_dataset, temp_dataset)
final_dataset <- final_dataset[, !duplicated(names(final_dataset))]
temp_dataset <- dataset[dataset$Age == "Total" & dataset$Sex == "All persons" & dataset$Series == "Employment", c("Country","Value")]
temp_dataset$ValLav <- temp_dataset$Value
temp_dataset$Value <- NULL
final_dataset <- cbind(final_dataset, temp_dataset)
final_dataset <- final_dataset[, !duplicated(names(final_dataset))]

final_dataset$PercentualeDisoccupati <- (final_dataset$ValDis / final_dataset$ValFL) * 100
final_dataset$PercentualeOccupati <- (final_dataset$ValLav / final_dataset$ValFL) * 100
final_dataset$PercentualeFLrispettoallaPop <- (final_dataset$ValFL / final_dataset$ValPop) * 100

directory_path <- "C:/Users/giova/Documents/R PROJECT/IstogrammaPercentuale"

# Crea l'istogramma per ogni valore unico di "Time"
for (anno in unique(final_dataset$Time)) {

  subset_data <- final_dataset[final_dataset$Time == anno, ]
  
  n_bins <- 9
  bin_limits <- seq(0, 20, length.out = n_bins + 1)
  
  grafico_istogramma <- ggplot(subset_data, aes(x = PercentualeDisoccupati)) +
    geom_histogram(breaks = bin_limits, fill = "skyblue", color = "black", alpha = 0.7) +
    labs(title = sprintf("Distribuzione di Frequenza di Disoccupati per paese, Anno %s", anno),
         x = "Percentuale di Disoccupati",
         y = "Frequenza") +
    theme_minimal()
  
  ggsave(file.path(directory_path, sprintf("Histogram_%s.pdf", anno)), grafico_istogramma, width = 8, height = 6)
}

directory_path <- "C:/Users/giova/Documents/R PROJECT/BoxplotPercentuale"

for (anno in unique(final_dataset$Time)) {

  subset_data <- final_dataset[final_dataset$Time == anno, ]
  
  grafico_boxplot <- ggplot(subset_data, aes(x = as.factor(1), y = PercentualeDisoccupati)) +
    geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
    labs(title = sprintf("Boxplot della Distribuzione di Disoccupati, Anno %s", anno),
         x = "Anno",
         y = "Percentuale di Disoccupati") +
    theme_minimal()
  
  ggsave(file.path(directory_path, sprintf("Boxplot_%s.pdf", anno)), grafico_boxplot, width = 12, height = 8)
}

for (anno in unique(final_dataset$Time)) {

  subset_data <- final_dataset[final_dataset$Time == anno, ]
  
  grafico_boxplot <- ggplot(subset_data, aes(x = as.factor(1), y = PercentualeDisoccupati)) +
    geom_boxplot(notch = TRUE, outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
    labs(title = sprintf("Boxplot a Intaglio della Distribuzione di Disoccupati, Anno %s", anno),
         x = "Anno",
         y = "Percentuale di Disoccupati") +
    theme_minimal()
  
  ggsave(file.path(directory_path, sprintf("BoxplotIntaglio_%s.pdf", anno)), grafico_boxplot, width = 12, height = 8)
}

grafico_boxplot <- ggplot(final_dataset, aes(x = as.factor(Time), y = PercentualeDisoccupati, group = as.factor(Time))) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  labs(title = "Boxplot della Distribuzione di Disoccupati per Anno",
       x = "Anno",
       y = "Percentuale di Disoccupati") +
  theme_minimal()

grafico_boxplot <- ggplot(final_dataset, aes(x = as.factor(Time), y = PercentualeDisoccupati, group = as.factor(Time))) +
  geom_boxplot(notch = TRUE, outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  labs(title = "Boxplot della Distribuzione di Disoccupati per Anno",
       x = "Anno",
       y = "Percentuale di Disoccupati") +
  theme_minimal()

ggsave(file.path(directory_path, "BoxplotIntaglio_TuttiAnni.pdf"), grafico_boxplot, width = 12, height = 8)

col_palette <- rainbow(length(unique(final_dataset$Time)))
# Creazione del grafico con sovrapposizione degli istogrammi
n_bins <- 9
bin_limits <- seq(0, 20, length.out = n_bins + 1)

# Creazione del grafico a faccette
grafico_istogramma <- ggplot(final_dataset, aes(x = PercentualeDisoccupati, fill = as.factor(Time))) +
  geom_histogram(breaks = bin_limits, color = "black", alpha = 0.7, position = "identity") +
  labs(title = "Distribuzione di Frequenza di Disoccupati per paese",
       x = "Percentuale di Disoccupati",
       y = "Frequenza") +
  theme_minimal() +
  scale_fill_manual(values = col_palette, name = "Anno") +  
  facet_wrap(~Time, scales = "free", labeller = labeller(Anno = "Anno")) +  
  labs(fill = "Anno") 

ggsave(file.path(directory_path, "Histogram_Facets.pdf"), grafico_istogramma, width = 12, height = 6)

directory_path <- "C:/Users/giova/Documents/R PROJECT/ParetoPercentuale"
library(qcc)
library(dplyr)

for (year in 2010:2022) {

  filtered_data <- final_dataset %>% filter(Time == year)
  
  ordered_data <- filtered_data %>% arrange(desc(PercentualeDisoccupati))
  ordered_data$Country[ordered_data$Country == "United Kingdom"] <- "UK"
  
  Countries <- ordered_data$Country
  PercentualiDisoccupati <- ordered_data$PercentualeDisoccupati
  
  pdf_path <- file.path(directory_path, paste0("pareto_chart_", year, ".pdf"))
  pdf(pdf_path)
  pareto.chart(PercentualiDisoccupati, 
               ylab = "Frequenza di Disoccupati in percentuale",
               names.arg = Countries,
               main = paste("Pareto Chart -", year))
  dev.off()
  
  system(paste("start", shQuote(pdf_path)))
}

directory_path <- "C:/Users/giova/Documents/R PROJECT/VariabiliAleatorie"
library(ggplot2)

for (anno in unique(final_dataset$Time)) {

  subset_data <- final_dataset[final_dataset$Time == anno, ]
  
  n_bins <- 13
  bin_limits <- seq(0, 20, length.out = n_bins + 1)
  
  grafico_istogramma <- ggplot(subset_data, aes(x = PercentualeDisoccupati)) +
    geom_histogram(breaks = bin_limits, fill = "skyblue", color = "black", alpha = 0.7) +
    labs(title = sprintf("Distribuzione di Frequenza di Disoccupati per paese, Anno %s", anno),
         x = "Percentuale di Disoccupati",
         y = "Frequenza") +
    theme_minimal()
  
  ggsave(file.path(directory_path, sprintf("Histogram_%s_%s.pdf", n_bins, anno)), grafico_istogramma, width = 8, height = 6)
}

