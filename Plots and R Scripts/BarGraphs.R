# In questo dataset sono stati creati i grafici a barre. In particolare:
# 1° grafico: Confronto della Disoccupazione tra Uomini e Donne in %s (2010-2022)", paese
# 2° grafico: Confronto tra Popolazione Maschile e Femminile in %s (2010-2022), paese
# 3° grafico: Confronto tra le diverse età in %s", paese

total_men_dataset <- dataset[dataset$Age == "Total" & dataset$Sex == "Men" & dataset$Series == "Unemployment", c("Country", "Time", "Value")]
total_women_dataset <- dataset[dataset$Age == "Total" & dataset$Sex == "Women" & dataset$Series == "Unemployment", c("Value", "Time")]
total_popwomen_dataset <- dataset[dataset$Age == "Total" & dataset$Sex == "Women" & dataset$Series == "Population", c("Value", "Time")]
total_popmen_dataset <- dataset[dataset$Age == "Total" & dataset$Sex == "Men" & dataset$Series == "Population", c("Value", "Time")]

colnames(total_women_dataset)[1] <- "ValueWomenUnemployed"
colnames(total_men_dataset)[3] <- "ValueMenUnemployed"
colnames(total_popmen_dataset)[1] <- "ValueMenPopulation"
colnames(total_popwomen_dataset)[1] <- "ValueWomenPopulation"

combined_dataset <- cbind(total_popmen_dataset,total_popwomen_dataset,total_men_dataset,total_women_dataset)
combined_dataset <- combined_dataset[, !duplicated(names(combined_dataset))]
combined_dataset <- combined_dataset[,c(1,3,2,4,5,6)]

install.packages("tidyverse")
library(tidyverse)
library(ggplot2)

# Percorso della directory di salvataggio
directory_path <- "C:/Users/giova/Documents/R PROJECT/GraficiBarreDisoccupazioneSesso"

# Lista dei paesi unici nel dataset
paesi <- unique(combined_dataset$Country)
print(paesi)

colnames(combined_dataset)[5] <- "Uomini Disoccupati"
colnames(combined_dataset)[6] <- "Donne Disoccupate"

for (paese in paesi) {
  subset_data <- combined_dataset[combined_dataset$Country == paese, ]
  
  subset_data_long <- tidyr::gather(subset_data, key = "Gender", value = "Value", `Donne Disoccupate`, `Uomini Disoccupati`)
  
  grafico <- ggplot(subset_data_long, aes(x = as.factor(Time), y = Value, fill = Gender)) +
    geom_bar(position = "dodge", stat = "identity", width = 0.8) +
    labs(title = sprintf("Confronto della Disoccupazione tra Uomini e Donne in %s (2010-2022)", paese),
         x = "Anno",
         y = "Valore in migliaia") +
    scale_fill_manual(values = c("Donne Disoccupate" = "darkorchid2", "Uomini Disoccupati" = "chartreuse3")) +
    theme_minimal()
  
  grafico <- grafico + labs(fill = "Distribuzione")
  
  ggsave(file.path(directory_path, sprintf("%s_grouped_bar_chart.pdf", paese)), grafico, width = 12, height = 6)
}

colnames(combined_dataset)[1] <- "Uomini"
colnames(combined_dataset)[2] <- "Donne"

directory_path <- "C:/Users/giova/Documents/R PROJECT/GraficiBarrePopolazioneSesso"

for (paese in paesi) {
  subset_data <- combined_dataset[combined_dataset$Country == paese, ]
  
  subset_data_long <- tidyr::gather(subset_data, key = "Gender", value = "Value", `Donne`, `Uomini`)
  
  grafico <- ggplot(subset_data_long, aes(x = as.factor(Time), y = Value, fill = Gender)) +
    geom_bar(position = "dodge", stat = "identity", width = 0.8) +
    labs(title = sprintf("Confronto tra Popolazione Maschile e Femminile in %s (2010-2022)", paese),
         x = "Anno",
         y = "Valore in migliaia") +
    scale_fill_manual(values = c("Donne" = "lightpink", "Uomini" = "cadetblue2")) +
    theme_minimal()
  
  grafico <- grafico + labs(fill = "Distribuzione")
  
  ggsave(file.path(directory_path, sprintf("%s_grouped_bar_chart.pdf", paese)), grafico, width = 12, height = 6)
}

directory_path <- "C:/Users/giova/Documents/R PROJECT/GraficiBarreEtà"
age_dataset <- LFS_D_28022024235215584[LFS_D_28022024235215584$Sex == "All persons" & LFS_D_28022024235215584$Series == "Labour Force", c("Country", "Age", "Time", "Value")]

for (paese in paesi) {
  subset_data <- age_dataset[age_dataset$Country == paese, ]
  
  subset_data <- subset_data[subset_data$Time %in% c(2010, 2022), ]
  
  grafico <- ggplot(subset_data, aes(x = as.factor(Time), y = Value, fill = Age)) +
    geom_bar(position = "dodge", stat = "identity", width = 0.8) +
    labs(title = sprintf("Confronto tra le diverse età in %s", paese),
         x = "Anno",
         y = "Valore in migliaia") +
    scale_fill_manual(values = c("15 to 19" = "lightpink", "20 to 24" = "cadetblue2", "25 to 29" = "darkorange2", "30 to 34" = "darkgreen",
                                 "35 to 39" = "coral", "40 to 44" = "darkcyan", "45 to 49" = "orchid3", "50 to 54" = "lightsteelblue",
                                 "55 to 59" = "gold2", "60 to 64" = "darkolivegreen3", "65 to 69" = "darkseagreen3", 
                                 "70 to 74" = "lightcoral", "75 to 79" = "darkslategray3", "80+" = "dodgerblue3")) +
    theme_minimal() +
    labs(fill = "Distribuzione")
  
  ggsave(file.path(directory_path, sprintf("%s_grouped_bar_chart.pdf", paese)), grafico, width = 12, height = 6)
}

