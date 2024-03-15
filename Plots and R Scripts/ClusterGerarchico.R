directory_path <- "C:/Users/giova/Documents/R PROJECT/ClusterGerarchico2022"

# Estrai i dati per l'anno 2022
quasisubset_data <- final_dataset[final_dataset$Time == "2022", ]

# Seleziona un sottoinsieme di colonne per il clustering
subset_cols <- c("PercentualeDisoccupati", "PercentualeFLrispettoallaPop")
cluster_ger_analysis <- quasisubset_data[,c("Country", "PercentualeDisoccupati", "PercentualeFLrispettoallaPop")]
subset_data <- quasisubset_data[, subset_cols]

# Loop attraverso tutti i metodi di dissimilarità - dendogramma senza suddivisioni in cluster
for (metodo_dissimil in c("euclidean", "manhattan", "maximum", "canberra", "minkowski")) {
  
  dissimilarita <- dist(subset_data, method = metodo_dissimil)
  
  # Loop attraverso tutti i metodi di clustering
  for (metodo_cluster in c("single", "complete", "average", "centroid", "median")) {
    
    cluster <- hclust(dissimilarita, method = metodo_cluster)
    
    pdf_path <- file.path(directory_path, sprintf("dendrogramma_2022_noScreeplot_%s_%s.pdf", metodo_dissimil, metodo_cluster))
    
    pdf(pdf_path)
    plot(cluster, hang = -1, main = sprintf("Dendrogramma - 2010 - Distanza: %s, Metodo: %s", metodo_dissimil, metodo_cluster),
         labels = quasisubset_data$Country, xlab = "", sub = "")
    dev.off()
  }
}


colonne_interesse <- c("PercentualeDisoccupati", "PercentualeFLrispettoallaPop")
cov_m <- quasisubset_data[, colonne_interesse]

# Calcola la matrice delle covarianze
matrice_covarianze <- cov(cov_m)
print(matrice_covarianze)

x <- quasisubset_data$PercentualeDisoccupati
y <- quasisubset_data$PercentualeFLrispettoallaPop

# Genera lo scatterplot
plot(x, y, main = "Scatterplot tra PercentualeDisoccupati e PercentualeFLrispettoallaPop",
     xlab = "PercentualeDisoccupati", ylab = "PercentualeFLrispettoallaPop",
     pch = 16, col = "black")

# Aggiungi la linea di regressione
fit <- lm(y ~ x)
abline(fit, col = "red")

# Calcola il coefficiente di correlazione (r)
correlation_coefficient <- cor(x, y)

# Aggiungi il valore del coefficiente di correlazione al grafico
text(quantile(x, 0.9), quantile(y, 0.1), paste("r =", round(correlation_coefficient, 3)), pos = 4, col = "darkgreen")

# Esegui la regressione lineare
linear_model <- lm(y ~ x, data = data.frame(x = x, y = y))

# Estrai il p-value associato al coefficiente di correlazione
p_value <- summary(linear_model)$coefficients["x", "Pr(>|t|)"]
print(p_value)

directory_path <- "C:/Users/giova/Documents/R PROJECT/ClusterGerarchicoConDivisione2022"

graphics.off() 
par("mar") 
par(mar=c(1,1,1,1))

for (metodo_dissimil in c("euclidean", "manhattan", "maximum", "canberra", "minkowski")) {
  
  dissimilarita <- dist(subset_data, method = metodo_dissimil)
  
  for (metodo_cluster in c("single", "complete", "average", "centroid", "median")) {
    
    cluster <- hclust(dissimilarita, method = metodo_cluster)
    
    png_path <- file.path(directory_path, sprintf("dendrogramma_2022_k2_%s_%s.png", metodo_dissimil, metodo_cluster))
    
    png(png_path, width = 800, height = 600)  
    
    plot(cluster, hang = -1, main = sprintf("Dendrogramma - 2022 - Distanza: %s, Metodo: %s", metodo_dissimil, metodo_cluster),
         labels = quasisubset_data$Country, xlab = "", sub = "")
    
    rect.hclust(cluster, k = 2, border = "purple")  
    
    dev.off()
  }
}

if (!requireNamespace("cluster", quietly = TRUE)) {
  install.packages("cluster")
}
library(cluster)

if (!requireNamespace("stats", quietly = TRUE)) {
  install.packages("stats")
}


manhattan_dist <- dist(subset_data, method = "manhattan")
wcss_values <- vector("numeric", length = 9)

# Calcola WCSS per k da 2 a 10
for (k in 2:10) {

  hc <- hclust(manhattan_dist, method = "average")
  
  cluster_membership <- cutree(hc, k = k)
  
  wcss_values[k - 1] <- sum(hc$height[1:(length(hc$height) - (k - 1))])
}

plot(2:10, wcss_values, type = "b", pch = 19, col = "red",
     xlab = "Numero di Cluster (k)", ylab = "WCSS",
     main = "Metodo del Gomito per Clustering Gerarchico")

library(stats)

k <- 5
hc <- hclust(dist(subset_data))
cluster_membership <- cutree(hc, k)
stat_non_homogeneity_clusters <- numeric(k)

# Calcola la misura di non omogeneità statistica dei cluster
for (i in 1:k) {
  subset_cluster_i <- subset_data[cluster_membership == i, ]
  stat_non_homogeneity_clusters[i] <- chisq.test(subset_cluster_i)$statistic
}

# Stampa le misure di non omogeneità per ciascun cluster
for (i in 1:k) {
  cat("Misure di non omogeneità statistica per il cluster", i, ":", stat_non_homogeneity_clusters[i], "\n")
}

# Calcola la misura di non omogeneità interna ai cluster (within)
within_non_homogeneity <- sum(hc$height[1:(length(hc$height) - (k - 1))])

# Calcola la misura di non omogeneità tra i cluster (between) considerando k
between_non_homogeneity <- sum(hc$height[(length(hc$height) - (k - 1)):length(hc$height)])

# Stampa i risultati
cat("Misura di non omogeneità interna ai cluster (within):", within_non_homogeneity, "\n")
cat("Misura di non omogeneità tra i cluster (between):", between_non_homogeneity, "\n")

medie_colonne <- mean(subset_cols)
print(medie_colonne)

directory_path <- "C:/Users/giova/Documents/R PROJECT/ClusterGerarchico2022"

# Screeplot
for (metodo_dissimil in c("euclidean", "manhattan", "maximum", "canberra", "minkowski")) {
  
  dissimilarita <- dist(subset_data, method = metodo_dissimil)
  
  for (metodo_cluster in c("single", "complete", "average", "centroid", "median")) {
    
    cluster <- hclust(dissimilarita, method = metodo_cluster)
    
    png_path <- file.path(directory_path, sprintf("dendrogramma_2010_%s_%s.png", metodo_dissimil, metodo_cluster))
    
    png(png_path, width = 800, height = 600, res = 100)  
    plot(cluster, hang = -1, main = sprintf("Dendrogramma - 2010 - Distanza: %s, Metodo: %s", metodo_dissimil, metodo_cluster),
         labels = quasisubset_data$Country, xlab = "", sub = "")
    
    num_clusters <- 1:10  
    wss <- sapply(num_clusters, function(k) sum(kmeans(as.matrix(subset_data), k)$tot.withinss))
    
    plot(num_clusters, wss, type = "b", main = "Screeplot", xlab = "Numero di Cluster", ylab = "Somma dei quadrati delle distanze intra-cluster")
    
    dev.off() 
  }
}

temp_dataset <- dataset[dataset$Age == "Total" & dataset$Sex == "Men" & dataset$Series == "Unemployment", c("Country","Value")]
temp_dataset$ValDisoccupatiMaschi <- temp_dataset$Value
temp_dataset$Value <- NULL
final_dataset <- cbind(final_dataset, temp_dataset)
final_dataset <- final_dataset[, !duplicated(names(final_dataset))]
temp_dataset <- dataset[dataset$Age == "Total" & dataset$Sex == "Women" & dataset$Series == "Unemployment", c("Country","Value")]
temp_dataset$ValDisoccupatiFemmine <- temp_dataset$Value
temp_dataset$Value <- NULL
final_dataset <- cbind(final_dataset, temp_dataset)
final_dataset <- final_dataset[, !duplicated(names(final_dataset))]

final_dataset$PercentualeDisoccupatiMaschi <- (final_dataset$ValDisoccupatiMaschi / final_dataset$ValFL) * 100
final_dataset$PercentualeDisoccupatiFemmine <- (final_dataset$ValDisoccupatiFemmine / final_dataset$ValFL) * 100

fm_subset_cols <- c("PercentualeDisoccupatiMaschi", "PercentualeDisoccupatiFemmine")

fm_subset_data <- quasisubset_data[, fm_subset_cols]

directory_path <- "C:/Users/giova/Documents/R PROJECT/ClusterKMEANS"

for (metodo_dissimil in c("euclidean", "manhattan", "maximum", "canberra", "minkowski")) {
  
  dissimilarita <- dist(fm_subset_data, method = metodo_dissimil)
  
  for (k in 2:6) {  # Modifica il range di k secondo le tue esigenze
    
    kmeans_result <- kmeans(fm_subset_data, centers = k, nstart = 10)
    
    png_path <- file.path(directory_path, sprintf("kmeans_2022_%s_%d.png", metodo_dissimil, k))
    
    png(png_path, width = 800, height = 600) 
    plot(fm_subset_data, col = kmeans_result$cluster, pch = 20,
         main = sprintf("K-Means - Distanza: %s, k: %d", metodo_dissimil, k),
         xlab = "", ylab = "")
    
    points(kmeans_result$centers, col = 1:k, pch = 8, cex = 2)
    
    dev.off()
  }
}

directory_path <- "C:/Users/giova/Documents/R PROJECT/ClusterKMEANSmisure"

for (metodo_dissimil in c("euclidean", "manhattan", "maximum", "canberra", "minkowski")) {
  
  dissimilarita <- dist(fm_subset_data, method = metodo_dissimil)
  
  for (k in 2:10) {
    
    kmeans_result <- kmeans(fm_subset_data, centers = k, nstart = 10)
    
    wcss <- sum(kmeans_result$withinss)  # Inerzia intra-cluster (Within-Cluster Sum of Squares)
    bcss <- sum(kmeans_result$betweenss)  # Inerzia tra i cluster (Between-Cluster Sum of Squares)
    
    wcss_cluster <- sapply(1:k, function(i) sum((fm_subset_data[kmeans_result$cluster == i,] - kmeans_result$centers[i,])^2))
    
    png_path <- file.path(directory_path, sprintf("kmeans_2022_%s_%d.png", metodo_dissimil, k))
    
    png(png_path, width = 800, height = 600)  # Imposta le dimensioni desiderate
    plot(fm_subset_data, col = kmeans_result$cluster, pch = 20,
         main = sprintf("K-Means - Distanza: %s, k: %d, WCSS: %.2f, BCSS: %.2f", metodo_dissimil, k, wcss, bcss),
         xlab = "", ylab = "")
    
    points(kmeans_result$centers, col = 1:k, pch = 8, cex = 2)
    
    text(1, max(fm_subset_data), sprintf("WCSS: %.2f", wcss), pos = 4)
    text(1, max(fm_subset_data) - 0.04 * diff(range(fm_subset_data)), sprintf("BCSS: %.2f", bcss), pos = 4)
    
    for (i in 1:k) {
      text(1, max(fm_subset_data) - i * 0.6, sprintf("WCSS Cluster %d: %.2f", i, wcss_cluster[i]), pos = 4)
    }
    
    dev.off()
  }
}

directory_path <- "C:/Users/giova/Documents/R PROJECT/ClusterKMEANSmisure"

for (metodo_dissimil in c("euclidean", "manhattan", "maximum", "canberra", "minkowski")) {
  
  dissimilarita <- dist(fm_subset_data, method = metodo_dissimil)
  
  for (k in 2:6) { 
    
    kmeans_result <- kmeans(fm_subset_data, centers = k, nstart = 10)
    
    wcss <- sum(kmeans_result$withinss)  
    bcss <- sum(kmeans_result$betweenss)  
    
    wcss_cluster <- sapply(1:k, function(i) sum((fm_subset_data[kmeans_result$cluster == i,] - kmeans_result$centers[i,])^2))
    
    png_path <- file.path(directory_path, sprintf("kmeans_2010_%s_%d.png", metodo_dissimil, k))
    
    png(png_path, width = 800, height = 600)
    plot(fm_subset_data, col = kmeans_result$cluster, pch = 20,
    main = sprintf("K-Means - Distanza: %s, k: %d, WCSS: %.2f, BCSS: %.2f, WCSS + BCSS: %.2f", metodo_dissimil, k, wcss, bcss, wcss + bcss),
    xlab = "", ylab = "")
    
    points(kmeans_result$centers, col = 1:k, pch = 8, cex = 2)
    
    text_x <- max(fm_subset_data[, 1]) + 0.05 * diff(range(fm_subset_data[, 1]))
    
    text_y <- mean(fm_subset_data[, 1]) - 0.02 * diff(range(fm_subset_data[, 1]))
    
    text(text_x, text_y, sprintf("WCSS: %.2f", wcss), pos = 4)
    text(text_x, text_y - 0.02 * diff(range(fm_subset_data[, 1])), sprintf("BCSS: %.2f", bcss), pos = 4)
    
    for (i in 1:k) {
      text(text_x, text_y - i * 0.1 * diff(range(fm_subset_data[, 1])), sprintf("WCSS Cluster %d: %.2f", i, wcss_cluster[i]), pos = 4)
    }
    
    # codice per aggiungere etichette dei paesi!!!
    #text(fm_subset_data, labels = quasisubset_data$Country, pos = 3, cex = 0.7)
    
    dev.off()
  }
}

plotbar_fm <- cbind(fm_subset_data, quasisubset_data[1])
plotbar_fm$Country <- gsub("United Kingdom", "UK", plotbar_fm$Country)

library(tidyr)
library(ggplot2)
plotbar_fm_long <- gather(plotbar_fm, key = "Variable", value = "Value", 
                          -Country)

barplot_horizontal <- ggplot(plotbar_fm_long, aes(x = Country, y = Value, fill = Variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Barplot orizzontale con Barre Affiancate",
       x = "Country",
       y = "Value") + 
  scale_fill_manual(values = c("PercentualeDisoccupatiMaschi" = "blue", 
                               "PercentualeDisoccupatiFemmine" = "red")) +
  theme_minimal() +
  coord_flip()  

print(barplot_horizontal)

ggsave("barplot_orizzontale.png", barplot_horizontal, width = 10, height = 40, units = "in")

install.packages("cluster")
library(cluster)
library(ggplot2)
wss <- numeric(length=10)

for(i in 1:10) {
  kmeans_model <- kmeans(fm_subset_data, centers = i, nstart = 10)
  wss[i] <- kmeans_model$tot.withinss
}

plot(1:10, wss, type="b", pch = 19, frame = false, xlab = "Numero di Cluster (k)", ylab = "Somma distanze quadrate intra-cluster")

title("Screeplot K-Means")

library(ggplot2)

directory_path <- "C:/Users/giova/Documents/R PROJECT/ClusterKMEANSmisureLabel"

install.packages("cluster")
install.packages("factoextra")
library(cluster)
library(factoextra)

fviz_nbclust(subset_data, kmeans, method = "wss")
fviz_nbclust(subset_data, kmeans, method = "silhouette")

hierarchical_result <- hclust(dist(subset_data))
fviz_nbclust(fm_subset_data, FUN = hcut, method = "silhouette")

silhouette_values <- numeric(length = 10)
for (k in 2:11) {  
  kmeans_result <- kmeans(fm_subset_data, centers = k, nstart = 10)
  silhouette_values[k-1] <- silhouette(kmeans_result$cluster, dist(fm_subset_data))
}

plot(2:11, silhouette_values, type = "b", pch = 19, frame = FALSE, 
     xlab = "Numero di Cluster (k)", ylab = "Silhouette",
     main = "Analisi della Silhouette")

pkgs <- c("factoextra",  "NbClust")
install.packages(pkgs)
library(factoextra)
set.seed(123)
fviz_nbclust(fm_subset_data, hcut, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")
fviz_nbclust(fm_subset_data, hcut, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(fm_subset_data, hcut, method = "silhouette")+
  labs(subtitle = "Silhouette method")
