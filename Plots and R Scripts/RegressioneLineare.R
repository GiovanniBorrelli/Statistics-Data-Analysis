# In questo notebook sono stati creati gli scatterplot con regressione lineare

total_emp_dataset <- dataset[dataset$Age == "Total" & dataset$Sex == "All persons" & dataset$Series == "Employment", c("Country", "Time", "Value")]

colnames(total_emp_dataset)[3] <- "ValueEmp"
total_emp_dataset$Country <- NULL
total_emp_dataset$Time <- NULL
risultato_unione <- cbind(total_age_dataset, total_emp_dataset)

library(ggplot2)

directory_path <- "C:/Users/giova/Documents/R PROJECT/RegressioneLineare"

# Lista dei paesi unici nel dataset
paesi <- unique(risultato_unione$Country)

# Ciclo per creare e salvare uno scatterplot per ciascun paese
for (paese in paesi) {
  subset_data <- risultato_unione[risultato_unione$Country == paese, ]

  correlation_coefficient <- cor(subset_data$ValueEmp, subset_data$Value)
  
  grafico <- ggplot(subset_data, aes(x = Value, y = ValueEmp)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Aggiunge la regressione lineare
    labs(title = sprintf("Confronto Forza Lavoro vs Lavoratori in %s (2010-2022)\nCoeff. Correlazione: %.2f", paese, correlation_coefficient),
         x = "Forza Lavoro (in migliaia)",
         y = "Lavoratori (in migliaia)") +
    theme_minimal()
  
  ggsave(file.path(directory_path, sprintf("%s_scatterplotRegressioneLineare.pdf", paese)), grafico, width = 8, height = 6)  # Modifica le dimensioni a tuo piacimento
}
