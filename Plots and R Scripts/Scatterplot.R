# In questo notebook sono stati creati gli scatterplot

total_emp_dataset <- dataset[dataset$Age == "Total" & dataset$Sex == "All persons" & dataset$Series == "Employment", c("Country", "Time", "Value")]

colnames(total_emp_dataset)[3] <- "ValueEmp"
total_emp_dataset$Country <- NULL
total_emp_dataset$Time <- NULL
risultato_unione <- cbind(total_age_dataset, total_emp_dataset) 
# valore di forza lavoro + valore di lavoratori

library(ggplot2)

directory_path <- "C:/Users/giova/Documents/R PROJECT/Scatterplot"

paesi <- unique(risultato_unione$Country)

# Ciclo per creare e salvare uno scatterplot per ciascun paese
for (paese in paesi) {
  subset_data <- risultato_unione[risultato_unione$Country == paese, ]
  
  grafico <- ggplot(subset_data, aes(x = Value, y = ValueEmp)) +
    geom_point() +
    labs(title = sprintf("Confronto Forza Lavoro vs Lavoratori in %s (2010-2022)", paese),
         y = "Lavoratori (in migliaia)",
         x = "Forza Lavoro (in migliaia)") +
    theme_minimal()

  ggsave(file.path(directory_path, sprintf("%s_scatterplot.pdf", paese)), grafico, width = 8, height = 6)  
}
