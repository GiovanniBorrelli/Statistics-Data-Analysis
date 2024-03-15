#  In questo notebook sono state create le serie temporali

total_age_dataset <- dataset[dataset$Age == "Total" & dataset$Sex == "All persons" & dataset$Series == "Labour Force", c("Country", "Time", "Value")]

library(ggplot2)

# Percorso della directory di salvataggio
directory_path <- "C:/Users/giova/Documents/R PROJECT/SerieTemporali"

paesi <- unique(total_age_dataset$Country)

# Ciclo per creare e salvare grafici separati per ciascun paese
for (paese in paesi) {
  subset_data <- total_age_dataset[total_age_dataset$Country == paese, ]
  
  grafico <- ggplot(subset_data, aes(x = Time, y = Value)) +
    geom_line() +
    labs(title = sprintf("Variazione della forza lavoro in %s (2010-2022)", paese),
         x = "Anno",
         y = "Valore in migliaia") +
    theme_minimal() +
    scale_x_continuous(breaks = seq(2010, 2022, by = 1))   
  
  ggsave(file.path(directory_path, sprintf("%s_graph.pdf", paese)), grafico, width = 8, height = 6) 







