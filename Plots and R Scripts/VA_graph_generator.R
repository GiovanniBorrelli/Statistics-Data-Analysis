directory_path <- "C:/Users/giova/Documents/R PROJECT/VariabiliAleatorie"
library(ggplot2)
{
# Seleziona solo i dati per l'anno 2022
subset_data <- final_dataset[final_dataset$Time == 2022, ]

n_bins <- 20
bin_limits <- seq(0, 20, length.out = n_bins + 1)

grafico_istogramma <- ggplot(subset_data, aes(x = PercentualeDisoccupati)) +
  geom_histogram(breaks = bin_limits, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_density(kernel = "gaussian", color = "red", fill = "transparent", bw = 0.5, adjust = 1.5) + 
  labs(title = "Distribuzione di Frequenza di Disoccupati per paese, Anno 2022",
       x = "Percentuale di Disoccupati",
       y = "Frequenza") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  )

ggsave(file.path(directory_path, "Histogram_2022_20_gaussian.png"), grafico_istogramma, width = 8, height = 6)
}

vect <- final_dataset[final_dataset$Time == 2022, c("PercentualeDisoccupati") ]
print(vect)

{
  # Calcola la densità con il kernel gaussiano
  d <- density(vect, kernel = "gaussian")
  
  y_max <- max(d$y)
  
  grafico <- ggplot(data.frame(x = d$x, y = d$y), aes(x, y)) +
    geom_line(lwd = 2) +
    labs(title = "Gaussian Kernel - Frequenza Percentuale Disoccupati 2022") +
    scale_y_continuous(breaks = seq(0, y_max, by = 1)) +  # Etichette intere sull'asse y
    theme(
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white")
    )
  
  ggsave(file.path(directory_path, "Density_Plot.png"), grafico, width = 8, height = 6)
  
}

library(gridExtra)
{
  istogramma <- ggplot(data.frame(x = vect), aes(x = x)) +
    geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
    labs(title = "Istogramma - Frequenza Percentuale Disoccupati 2022",
         x = "Percentuale di Disoccupati",
         y = "Frequenza") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white")
    )
  
  # Calcola la densità con il kernel gaussiano
  d <- density(vect, kernel = "gaussian")
  
  grafico_densita <- ggplot(data.frame(x = d$x, y = d$y), aes(x, y)) +
    geom_line(lwd = 2, color = "red") +
    labs(title = "Gaussian Kernel - Frequenza Percentuale Disoccupati 2022") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white")
    )
  
  ggsave(file.path(directory_path, "Histogram_Density_Plot.png"), arrangeGrob(istogramma, grafico_densita), width = 8, height = 6)
  
}





{
  subset_data <- final_dataset[final_dataset$Time == 2022, ]
  
  # Genera dati casuali da una distribuzione normale, del chi-quadrato e t di Student
  random_data_norm <- rnorm(10000, mean = mean(vect), sd = sd(vect))
  random_data_chisq <- rchisq(10000, df = 8)  
  random_data_t <- rt(10000, df = 5)  
  
  n_bins <- 50
  bin_limits <- seq(0, 20, length.out = n_bins + 1)
  
  # Genera un istogramma per la distribuzione normale
  grafico_norm <- ggplot(data.frame(x = random_data_norm), aes(x = x)) +
    geom_histogram(breaks = bin_limits, fill = "skyblue", color = "black", alpha = 0.7) +
    labs(title = "Distribuzione Normale, Anno 2022",
         x = "Valore Casuale",
         y = "Frequenza") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white")
    )
  
  # Genera un istogramma per la distribuzione del chi-quadrato
  grafico_chisq <- ggplot(data.frame(x = random_data_chisq), aes(x = x)) +
    geom_histogram(breaks = bin_limits, fill = "salmon", color = "black", alpha = 0.7) +
    labs(title = "Distribuzione Chi-Quadrato, Anno 2022",
         x = "Valore Casuale",
         y = "Frequenza") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white")
    )
  
  # Genera un istogramma per la distribuzione t di Student
  grafico_t <- ggplot(data.frame(x = random_data_t), aes(x = x)) +
    geom_histogram(breaks = bin_limits, fill = "lightgreen", color = "black", alpha = 0.7) +
    labs(title = "Distribuzione t di Student, Anno 2022",
         x = "Valore Casuale",
         y = "Frequenza") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white")
    )
  
  ggsave(file.path(directory_path, "RandomData_Normal_2022.png"), grafico_norm, width = 8, height = 6)
  ggsave(file.path(directory_path, "RandomData_ChiSq_2022.png"), grafico_chisq, width = 8, height = 6)
  ggsave(file.path(directory_path, "RandomData_StudentT_2022.png"), grafico_t, width = 8, height = 6)
  
}

directory_path <- "C:/Users/giova/Documents/R PROJECT/ChiQuadrato"

df_chi <-  5.93817  

densita_chi <- data.frame(x = seq(0, 20, length.out = 1000),
                          y = dchisq(seq(0, 20, length.out = 1000), df = df_chi))

distribuzione_chi <- data.frame(x = seq(0, 20, length.out = 1000),
                                y = pchisq(seq(0, 20, length.out = 1000), df = df_chi))

grafico_densita_chi <- ggplot(densita_chi, aes(x, y)) +
  geom_line(lwd = 2, color = "red") +
  labs(title = sprintf("Densità Chi-Quadrato, Anno 2022, grado di libertà = %s", df_chi)) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  )

ggsave(file.path(directory_path, sprintf("Density_ChiSq_2022_n%s.png", df_chi)), grafico_densita_chi, width = 8, height = 6)

grafico_distribuzione_chi <- ggplot(distribuzione_chi, aes(x, y)) +
  geom_line(lwd = 2, color = "blue") +
  labs(title = "Distribuzione Chi-Quadrato, Anno 2022") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  )

ggsave(file.path(directory_path, "Distribution_ChiSq_2022.png"), grafico_distribuzione_chi, width = 8, height = 6)

quantili_chi <- data.frame(x = seq(0, 1, length.out = 1000),
                           y = qchisq(seq(0, 1, length.out = 1000), df = df_chi))

grafico_quantili_chi <- ggplot(quantili_chi, aes(x, y)) +
  geom_line(lwd = 2, color = "green") +
  labs(title = "Quantili Chi-Quadrato, Anno 2022") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  )

ggsave(file.path(directory_path, "Quantiles_ChiSq_2022.png"), grafico_quantili_chi, width = 8, height = 6)


media_normale <- 5.577043  
deviazione_standard_normale <- 2.893177 

densita_normale <- data.frame(x = seq(0, 20, length.out = 1000),
                              y = dnorm(seq(0, 20, length.out = 1000), mean = media_normale, sd = deviazione_standard_normale))

grafico_densita_normale <- ggplot(densita_normale, aes(x, y)) +
  geom_line(lwd = 2, color = "blue") +
  labs(title = sprintf("Densità Distribuzione Normale, Anno 2022, Media = %s, Dev. Standard = %s",
                       media_normale, deviazione_standard_normale)) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  )

ggsave(file.path(directory_path, sprintf("Density_NormalDistribution.png")), grafico_densita_normale, width = 8, height = 6)

