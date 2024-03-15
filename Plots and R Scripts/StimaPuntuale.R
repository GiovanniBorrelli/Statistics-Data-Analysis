directory_path <- "C:/Users/giova/Documents/R PROJECT/StimaPuntuale"
library(stats4)
library(stats)

# Funzione di verosimiglianza per la distribuzione chi-quadrato
likelihood_chi <- function(df) {
  -sum(dchisq(vect, df = df, log = TRUE))
}

mle_result <- mle(likelihood_chi, start = list(df = 1))  # Start con un valore iniziale arbitrario

summary(mle_result)

# Calcola l'intervallo di confidenza per la media utilizzando il metodo pivotale
intervallo_confidenza_media_pivotale <- t.test(vect, conf.level = 0.95)$conf.int
print(intervallo_confidenza_media_pivotale)

df_chi <- 5.93817
quantile_inf <- qchisq(0.05, df = df_chi)
quantile_sup <- qchisq(0.95, df = df_chi)
cat("Intervallo di confidenza al 90%:", quantile_inf, "-", quantile_sup, "\n")

media_chi_quadro <- mean(vect, df = 5.93817)
print(media_chi_quadro)

media <- mean(vect)
deviazione_std <- sd(vect)
livello_confidenza <- 0.95
n <- length(vect)
interv_confidenza <- qchisq(c((1 - livello_confidenza) / 2, 1 - (1 - livello_confidenza) / 2), n - 1)
interv_confidenza <- sqrt(interv_confidenza)
errore_std_media <- deviazione_std / sqrt(n)
intervallo_confidenza_media <- media + interv_confidenza * errore_std_media
print(intervallo_confidenza_media)

df_sperimentale <- 5.93817
df_ipotesi <- 6  

test_stat <- (df_sperimentale - df_ipotesi)^2 / df_ipotesi
livello_significativita <- 0.05
p_value <- 1 - pchisq(test_stat, df = 1)  # Per un test bilaterale

if (p_value < livello_significativita) {
  cat("Rifiutiamo l'ipotesi nulla a", livello_significativita, "di significatività.\n")
} else {
  cat("Non rifiutiamo l'ipotesi nulla a", livello_significativita, "di significatività.\n")
}

library(ggplot2)

# Dati osservati
observed_data <- data.frame(x = seq(0, 15, length.out = 1000), 
                            y = dchisq(seq(0, 15, length.out = 1000), df = df_sperimentale))

# Dati sotto l'ipotesi nulla
null_hypothesis_data <- data.frame(x = seq(0, 15, length.out = 1000), 
                                   y = dchisq(seq(0, 15, length.out = 1000), df = df_ipotesi))

grafico_osservati <- ggplot(observed_data, aes(x = x, y = y)) +
  geom_line(color = "blue", size = 1.5) +
  labs(title = "Dati Osservati", x = "Valore X", y = "Valore Y")

grafico_ipotesi_nulla <- ggplot(null_hypothesis_data, aes(x = x, y = y)) +
  geom_line(color = "red", size = 1.5) +
  labs(title = "Ipotesi Nulla", x = "Valore X", y = "Valore Y")

ggsave(file.path(directory_path, "grafico_osservati.png"), plot = grafico_osservati, width = 8, height = 6)
ggsave(file.path(directory_path, "grafico_ipotesi_nulla.png"), plot = grafico_ipotesi_nulla, width = 8, height = 6)

# Funzione di verosimiglianza per la distribuzione normale
likelihood_norm <- function(mu, sigma) {
  log_likelihood <- sum(dnorm(vect, mean = mu, sd = sigma, log = TRUE))
  return(-log_likelihood)  

mle_norm_result <- mle(likelihood_norm, start = list(mu = mean(vect), sigma = sd(vect)))
summary(mle_norm_result)


mean(vect)
deviazione_standard <- sd(vect)
deviazione_standard <- 2.93201339770631
media_osservata <- 5.577043
media_aspettata <- 5
df_migliore <- 5.93817
alpha <- 0.001
numero_campioni <- 38

valore_t <- (media_osservata - media_aspettata) / (deviazione_standard / sqrt(numero_campioni))
gradi_liberta <- numero_campioni - 1
p_value <- 2 * (1 - pt(abs(valore_t), df = gradi_liberta))

if (p_value < alpha) {
  cat("Rifiutiamo l'ipotesi nulla al livello di significatività", alpha, "\n")
} else {
  cat("Non rifiutiamo l'ipotesi nulla al livello di significatività", alpha, "\n")
}

cat("Valore t:", valore_t, "\n")
cat("P-value:", p_value, "\n")

probabilities <- c(0, 0.25, 0.5, 0.75, 1)
quantiles <- quantile(vect, probabilities)
intervalli <- cut(vect, breaks = quantiles, include.lowest = TRUE)
table(intervalli)

osservati <- c(10, 9, 9, 10)
lambda <- mean(osservati)
prob_attesa <- dpois(0:(length(osservati)-1), lambda) * sum(osservati)

# Calcola il test del chi-quadrato
chi_quad_stat <- sum((osservati - prob_attesa)^2 / prob_attesa)
gradi_liberta <- length(osservati) - 1
p_value <- 2 * (1 - pchisq(chi_quad_stat, df = gradi_liberta))
livello_significativita <- 0.05
intervallo_accettazione <- qchisq(c(0.005, 0.995), df = gradi_liberta)

if (p_value < livello_significativita) {
  cat("Rifiutiamo l'ipotesi nulla al livello di significatività", livello_significativita, "\n")
  cat(sprintf("Non essendo %.3f < 𝜒^2 < %.3f, l’ipotesi nulla di popolazione di Poisson non può essere accettata\n", intervallo_accettazione[1], intervallo_accettazione[2]))
} else {
  cat("Non rifiutiamo l'ipotesi nulla al livello di significatività", livello_significativita, "\n")
  cat(sprintf("Essendo %.3f < 𝜒^2 < %.3f, l’ipotesi nulla di popolazione di Poisson può essere accettata\n", intervallo_accettazione[1], intervallo_accettazione[2]))
}

cat("Statistica del test del chi-quadrato:", chi_quad_stat, "\n")
cat("P-value:", p_value, "\n")

osservati <- c(10, 9, 9, 10)
gradi_liberta_ipotesi <- 5.93817  

# Calcola il test del chi-quadrato
chi_quad_stat <- sum((osservati - gradi_liberta_ipotesi)^2 / gradi_liberta_ipotesi)

# Calcola il p-value
p_value <- 1 - pchisq(chi_quad_stat, df = length(osservati) - 1)

livello_significativita <- 0.05

intervallo_accettazione <- qchisq(c(0.005, 0.995), df = length(osservati) - 1)

if (p_value < livello_significativita) {
  cat("Rifiutiamo l'ipotesi nulla al livello di significatività", livello_significativita, "\n")
  cat(sprintf("Non essendo %.3f < 𝜒^2 < %.3f, l’ipotesi nulla di distribuzione chi-quadrato con %.1f gradi di libertà non può essere accettata\n", intervallo_accettazione[1], intervallo_accettazione[2], gradi_liberta_ipotesi))
} else {
  cat("Non rifiutiamo l'ipotesi nulla al livello di significatività", livello_significativita, "\n")
  cat(sprintf("Essendo %.3f < 𝜒^2 < %.3f, l’ipotesi nulla di distribuzione chi-quadrato con %.1f gradi di libertà può essere accettata\n", intervallo_accettazione[1], intervallo_accettazione[2], gradi_liberta_ipotesi))
}

cat("Statistica del test del chi-quadrato:", chi_quad_stat, "\n")
cat("P-value:", p_value, "\n")

# Calcola i quantili della distribuzione normale standard
quantili_normale <- quantile(vect, probs = seq(0, 1, length.out = 5))
intervalli <- cut(vect, breaks = quantili_normale, include.lowest = TRUE)
frequenze_osservate <- table(intervalli)
print(frequenze_osservate)

# Calcolare il numero totale di osservazioni
n_totale <- length(vect)
num_intervalli <- 4
frequenze_attese <- rep(n_totale / num_intervalli, num_intervalli)
frequenze_attese_normalizzate <- frequenze_attese / sum(frequenze_attese)
intervalli <- cut(vect, breaks = quantili_normale, include.lowest = TRUE)
test_chi_quadro <- chisq.test(table(intervalli), p = frequenze_attese_normalizzate, correct = FALSE)
print(test_chi_quadro)

livello_significativita <- 0.05
valore_critico_sinistro <- qchisq(livello_significativita / 2, df = test_chi_quadro$parameter)
valore_critico_destra <- qchisq(1 - livello_significativita / 2, df = test_chi_quadro$parameter)
regione_accettazione <- c(valore_critico_sinistro, valore_critico_destra)
cat("Regioni di accettazione:", regione_accettazione, "\n")
intervalli <- cut(vect, breaks = quantili_normale, include.lowest = TRUE)
cat("Intervalli:")
levels(intervalli)

n <-length(vect)
m <- mean(vect)
d <-sd(vect)
a <- numeric (4)
for (i in 1:4)
  a[ i] <- qnorm (0.2 *i , mean =m , sd = d)

r <-5
nint <- numeric (r)
nint [1] <- length ( which ( vect < a [1]) )
nint [2] <- length ( which (( vect >= a [1]) &( vect <a[2]) ))
nint [3] <- length ( which (( vect >= a [2]) &( vect <a[3]) ))
nint [4] <- length ( which (( vect >= a [3]) &( vect <a[4]) ))
nint [5] <- length ( which ( vect >= a [4]) )
nint

chi2 <- sum ((( nint - n* 0.2) / sqrt ( n* 0.2) ) ^2)
chi2

k <-2
alpha <- 0.05
qchisq ( alpha /2, df =r -k -1)
qchisq (1 - alpha /2, df =r -k -1)

media_vect <- mean(vect)
deviazione_std_vect <- sd(vect)
densita_normale <- dnorm(vect, mean = media_vect, sd = deviazione_std_vect)
hist_densita_normale <- hist(densita_normale, breaks = seq(min(densita_normale), max(densita_normale), length.out = 20), plot = FALSE)
frequenze_densita_normale <- hist_densita_normale$counts
frequenze_densita_normale_normalizzate <- frequenze_densita_normale / sum(frequenze_densita_normale)

test_chi_quadro <- chisq.test(table(cut(vect, breaks = seq(min(vect), max(vect), length.out = 20))), p = frequenze_densita_normale_normalizzate)
print(test_chi_quadro)

n <-length(vect)
m <- mean(vect)
d <-sd(vect)
print(d)
vect_corto <- vect[1:25]
d <-sd(vect_corto)
print(d)

alpha <- 0.05
sigma02 <- 8.596
n = 25
varcamp <- 7.695
qchisq(alpha/2, df=n-1)
qchisq(1-alpha/2, df=n-1)
(n-1)*varcamp/sigma02
