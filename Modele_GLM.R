options(scipen=99)

install.packages("C:/Users/admin/Desktop/MGR/CASdatasets_1.2-0.tar.gz",
                 repos = NULL, type = "source")


library(CASdatasets)

library(dplyr)
library(ggplot2)
library(MASS)
library(tweedie)
library(statmod)


## Wybór zmiennych
data(freMPL5)
freMPL5 <- unique(freMPL5)
freMPL5$PastClaims <- freMPL5$ClaimNbResp + freMPL5$ClaimNbNonResp + freMPL5$ClaimNbFireTheft + 
  freMPL5$ClaimNbParking + freMPL5$ClaimNbWindscreen


data_set <- data.frame(
  ClaimAmount = freMPL5$ClaimAmount,
  Gender = freMPL5$Gender,
  LicAge = as.numeric(freMPL5$LicAge),
  DrivAge = as.numeric(freMPL5$DrivAge),
  RiskArea = as.factor(freMPL5$RiskArea),
  PastClaims = freMPL5$PastClaims
)


data_set <- data_set[data_set$ClaimAmount > 0 ,]

## Analiza Wstępna
par(mfrow = c(2,3)) 

# ZMIENNE CIĄGŁE

# Histogram Claim Ammount
hist(data_set$ClaimAmount, 
     nclass = 100, 
     col = "firebrick",
     border = "white",
     main = "Rozkład Claim Ammount", 
     xlab = "Claim Ammount", 
     ylab = "Liczebność",
     xlim = c(0, 10000)) #Pokazujemy tylko szkody do 10k, żeby było  widać

# Histogram Stażu Prawa Jazdy
hist(data_set$LicAge, 
     nclass = 20, 
     col = "mediumseagreen",
     border = "white",
     main = "Staż Prawa Jazdy", 
     xlab = "Miesiące", 
     ylab = "Liczebność")

# Histogram Wieku Kierowcy
hist(data_set$DrivAge, 
     nclass = 20, 
     col = "steelblue",
     border = "white",
     main = "Wiek Kierowcy", 
     xlab = "Lata", 
     ylab = "Liczebność")

## ZMIENNE KATEGORYCZNE

# Płeć (Gender)
barplot(table(data_set$Gender), 
        col = c("pink", "lightblue"), 
        main = "Płeć Kierowców",
        ylab = "Liczba kierowców")

# Strefa Ryzyka (RiskArea)
barplot(table(data_set$RiskArea), 
        col = "orange", 
        main = "Strefa Ryzyka", 
        ylab = "Liczba szkód",
        las = 2,
        cex.names = 0.8)

# Historia Szkód (PastClaims)
barplot(table(data_set$PastClaims), 
        col = "tomato", 
        main = "Historia Szkód", 
        xlab = "Liczba szkód",
        ylab = "Liczebność")


par(mfrow = c(1,2))
# Płeć vs Wysokość Szkody
boxplot(ClaimAmount ~ Gender, 
        data = data_set, 
        log = "y", #Skala logarytmiczna do wizualizacji
        col = c("pink", "lightblue"),
        main = "Rozrzut szkód wg Płci",
        ylab = "Kwota (Skala Log)",
        xlab = "Płeć")

# 2. Strefa Ryzyka vs Wysokość Szkody
boxplot(ClaimAmount ~ RiskArea, 
        data = data_set, 
        log = "y",
        col = "orange",
        main = "Rozrzut szkód wg Strefy",
        ylab = "Kwota (Skala Log)",
        las = 2)

par(mfrow = c(1,1))


#Korelacja
zmienne_num <- data_set[, c("ClaimAmount", "DrivAge", "LicAge")]
print(cor(zmienne_num))

#Zaleznosc między zmiennymi DrivAge i LicAge
plot(data_set$DrivAge, data_set$LicAge,
     main = "Korelacja: Wiek vs Prawo Jazdy",
     xlab = "Wiek Kierowcy",
     ylab = "Staż Prawa Jazdy",
     pch = 19,
     col = rgb(0,0,0, 0.1),
     cex = 0.5)



# Szkody w zależności od wieku
analiza_wieku <- aggregate(ClaimAmount ~ DrivAge, data = data_set, FUN = mean)

# Rysowanie wykresu
plot(analiza_wieku$DrivAge, analiza_wieku$ClaimAmount,
     type = "p",
     pch = 19, col = "blue",
     main = "Średnia wartość szkody w zależności od wieku",
     xlab = "Wiek Kierowcy",
     ylab = "Średnia szkoda")

# Dodanie linii trendu
lines(lowess(analiza_wieku$DrivAge, analiza_wieku$ClaimAmount, f = 0.2), 
      col = "red", lwd = 3)

legend("topright", legend = c("Średnia z danych", "Trend"), 
       col = c("blue", "red"), pch = c(19, NA), lty = c(NA, 1))




# liczności w kategoriach
lapply(data_set[, c("Gender", "RiskArea", "PastClaims")], table)

levels(data_set$RiskArea)[levels(data_set$RiskArea) %in% c("11","12", "13")] <- "11+" #połączenie kategorii

data_set <- droplevels(data_set) #usunięcie pustej kategorii w RiskArea


## Model GAMMA GLM
model = glm(ClaimAmount ~ Gender + LicAge + DrivAge + RiskArea + PastClaims,
            family = Gamma(link = "log"), data_set)

## Model inverse gaussian GLM
model_gaussian = glm(ClaimAmount ~ Gender + LicAge + DrivAge + RiskArea + PastClaims,
            family = inverse.gaussian(link = "log"), data_set, start = coef(model))

## Prognozy punktowe i przedziałowe wartości oczekiwanej zmiennej objaśnianej ??

id_osoba1 <- 10
id_osoba2 <- 200

# Parametry modelu
p_beta <- coef(model)        
cov_beta <- vcov(model)      
X_all <- model.matrix(model)
p_beta_gaus <- coef(model_gaussian)        
cov_beta_gaus<- vcov(model_gaussian)      
X_all_gaus <- model.matrix(model_gaussian)

# Wyciągamy wiersze dla os1 i os2 z macierzy modelu
p_vector_1 <- X_all[id_osoba1, ]
p_vector_2 <- X_all[id_osoba2, ]                                                                                                                                                                                                                                                                                    
p_vector_gaus_1 <- X_all_gaus[id_osoba1, ]
p_vector_gaus_2 <- X_all_gaus[id_osoba2, ]                                                                                                                                                                                                                                                                                    


# Predykcja liniowa (eta)
p_predict_linear_1 <- sum(p_vector_1 * p_beta)
p_predict_linear_2 <- sum(p_vector_2 * p_beta)
print(paste("Predyktor liniowy (rozkład gamma) dla osoby 1:", p_predict_linear_1))
print(paste("Predyktor liniowy (rozkład gamma) dla osoby 2:", p_predict_linear_2))
p_predict_linear_gaus_1 <- sum(p_vector_gaus_1 * p_beta_gaus)
p_predict_linear_gaus_2 <- sum(p_vector_gaus_2 * p_beta_gaus)
print(paste("Predyktor liniowy (rozkład inverse gaussian) dla osoby 1:", p_predict_linear_gaus_1))
print(paste("Predyktor liniowy (rozkład inverse gaussian) dla osoby 2:", p_predict_linear_gaus_2))

# Wariancja predyktora liniowego
var_p_linear_1 <- t(p_vector_1) %*% cov_beta %*% p_vector_1
var_p_linear_2 <- t(p_vector_2) %*% cov_beta %*% p_vector_2
print(paste("Wariancja predyktora liniowego dla osoby 1:", var_p_linear_1))
print(paste("Wariancja predyktora liniowego dla osoby 2:", var_p_linear_2))
var_p_linear_gaus_1 <- t(p_vector_gaus_1) %*% cov_beta_gaus %*% p_vector_gaus_1
var_p_linear_gaus_2 <- t(p_vector_gaus_2) %*% cov_beta_gaus %*% p_vector_gaus_2
print(paste("Wariancja predyktora liniowego dla osoby 1:", var_p_linear_gaus_1))
print(paste("Wariancja predyktora liniowego dla osoby 2:", var_p_linear_gaus_2))

# Wartość oczekiwana (link logarytmiczny)
p_predict_1 <- exp(p_predict_linear_1)
p_predict_2 <- exp(p_predict_linear_2)
p_predict_gaus_1 <- exp(p_predict_linear_gaus_1)
p_predict_gaus_2 <- exp(p_predict_linear_gaus_2)

# Wariancja predyktora wartości oczekiwanej
var_p_predict_1 <- (exp(p_predict_linear_1))^2 * var_p_linear_1
var_p_predict_2 <- (exp(p_predict_linear_2))^2 * var_p_linear_2
var_p_predict_gaus_1 <- (exp(p_predict_linear_gaus_1))^2 * var_p_linear_gaus_1
var_p_predict_gaus_2 <- (exp(p_predict_linear_gaus_2))^2 * var_p_linear_gaus_2

# Przedziały ufności (95%)
dol_1 <- p_predict_1 - 1.96 * sqrt(var_p_predict_1)
gora_1 <- p_predict_1 + 1.96 * sqrt(var_p_predict_1)
dol_2 <- p_predict_2 - 1.96 * sqrt(var_p_predict_2)
gora_2 <- p_predict_2 + 1.96 * sqrt(var_p_predict_2)

dol_gaus_1 <- p_predict_gaus_1 - 1.96 * sqrt(var_p_predict_gaus_1)
gora_gaus_1 <- p_predict_gaus_1 + 1.96 * sqrt(var_p_predict_gaus_1)
dol_gaus_2 <- p_predict_gaus_2 - 1.96 * sqrt(var_p_predict_gaus_2)
gora_gaus_2 <- p_predict_gaus_2 + 1.96 * sqrt(var_p_predict_gaus_2)


wyniki_df <- data.frame(
  Osoba = c("Osoba 1", "Osoba 2"),
  # Model Gamma
  Gamma_Dol = round(c(dol_1, dol_2), 2),
  Gamma_Pred = round(c(p_predict_1, p_predict_2), 2),
  Gamma_Gora = round(c(gora_1, gora_2), 2),
  
  # Model Inverse Gaussian
  InvGauss_Dol = round(c(dol_gaus_1, dol_gaus_2), 2),
  InvGauss_Pred = round(c(p_predict_gaus_1, p_predict_gaus_2), 2),
  InvGauss_Gora = round(c(gora_gaus_1, gora_gaus_2), 2)
)

print(wyniki_df)

## Eliminacja zmiennych - test F
drop1(model, test = "F" )#używam drop1 do redukcji zmiennych, zmienna gender nieistotna

model_reduced = glm(ClaimAmount ~  LicAge + DrivAge + RiskArea + PastClaims,
            family = Gamma(link = "log"), data_set) #model bez zmiennej gender

drop1(model_reduced, test = "F") #wszystkie zmienne są istotne

model_prediction = predict(model_reduced, type = "response")


#Funkcja drop1 nie działa dla inverse gaussian, używam AIC do porównywania modeli
summary(model_gaussian) #zmienna gender nie istotna
AIC(model_gaussian) # AIC = 25217.01

model_gaussian_2 = glm(ClaimAmount ~ LicAge + DrivAge + RiskArea + PastClaims,
                   family = inverse.gaussian(link = "log"), data_set, start = coef(model_reduced))

AIC(model_gaussian_2) # AIC = 25215.42 - poprawa
summary(model_gaussian_2) #zmienna LicAge nieistotna

model_pomocniczy = glm(ClaimAmount ~  DrivAge + RiskArea + PastClaims,
                                        family = Gamma(link = "log"), data_set)

model_gaussian_3 = glm(ClaimAmount ~ DrivAge + RiskArea + PastClaims,
                       family = inverse.gaussian(link = "log"), data_set, start = coef(model_pomocniczy))

AIC(model_gaussian_3) # AIC = 25214.51 - poprawa
summary(model_gaussian_3) # zmienna DrivAge nieistotna

model_pomocniczy_2 = glm(ClaimAmount ~ RiskArea + PastClaims,
                         family = Gamma(link = "log"), data_set)

model_gaussian_reduced = glm(ClaimAmount ~ RiskArea + PastClaims,
                       family = inverse.gaussian(link = "log"), data_set, start = coef(model_pomocniczy_2))


AIC(model_gaussian_reduced) # AIC = 25212.7 - poprawa
summary(model_gaussian_reduced) #reszta zmiennych jest istotna

model_prediction_gaus = predict(model_gaussian_reduced, type = "response")

## Jakość dopasowania modelu

# Model GAMMA
par(mfrow = c(1,2))

#Wykres reszt dewiancyjnych
plot(residuals(model_reduced, type = "deviance") ~ predict(model_reduced, type = "link"),
     col = "blue", pch = 20, xlab = "Predyktor liniowy", ylab = "Reszty dewiancyjne", 
     main = "Wykres reszt dewiancyjnych")
abline(h = 0, col = "red", lwd = 2)
add_loc_reg(residuals(model_reduced, type = "deviance"), predict(model_reduced, type = "link"), 
            sp = 0.2, d = 0)

mean(residuals(model_reduced, type = "deviance"))
2/6*sqrt(summary(model_reduced)$dispersion)
skewness(residuals(model_reduced, type = "deviance"))

#Wykres reszt Pearsona
plot(residuals(model_reduced, type = "pearson") ~ predict(model_reduced, type = "link"),
     col = "blue", pch = 20, xlab = "Predyktor liniowy", ylab = "Reszty preasona", 
     main = "Wykres reszt Peasona", ylim = c(-2,10))
abline(h = 0, col = "red", lwd = 2)
add_loc_reg(residuals(model_reduced, type = "pearson"), predict(model_reduced, type = "link"), 
            sp = 0.2, d = 0)

mean(residuals(model_reduced, type = "pearson"))
skewness(residuals(model_reduced, type = "pearson"))

par(mfrow = c(1,1))

wghts = rep(1, length(data_set$ClaimAmount)) #brak wag, zmienna potrzebna do funkcji qqplot

gamma_qq_plot(data_set$ClaimAmount, 
              model_prediction, 
              wghts,
              summary(model_reduced)$dispersion , "Gamma GLM")

# Analiza wsp. dyspersji i ocena wyboru funkcji wariancji

par(mfrow = c(1,2))
gamma.dispersion(model_reduced)


sq_residuals_1 = (data_set$ClaimAmount-model_prediction)^2/(1-hatvalues(model_reduced))
sq_residuals_2 = sq_residuals_1/model_prediction^2

# Ocena wyboru funkcji wariancji
plot(sq_residuals_2 ~ log(model_prediction),
     main = "Ocena funkcji wariancji",
     xlab = "Predyktor liniowy", ylab = "Kwadrat reszt Pearsona", 
     col = "blue", pch = 20, ylim = c(0,30))
abline(h = mean(sq_residuals_2), col = "red", lwd = 2)
add_loc_reg(sq_residuals_2, log(model_prediction), sp = 0.20, d = 0)

plot(log(sq_residuals_1) ~ log(model_prediction), col = "blue", pch = 20,
     xlab = "Logarytm predykcji",ylab = "Logarytm estymatora wariancji", main = "Zależność wariancji od wartości oczekiwanej")
abline(log(summary(model_reduced)$dispersion), 2, col = "red", lwd = 2)




## Model Inverse Gaussian
plot(residuals(model_gaussian_reduced, type = "deviance") ~ predict(model_gaussian_reduced, type = "link"),
     col = "blue", pch = 20, xlab = "Predyktor liniowy", ylab = "Reszty dewiancyjne", 
     main = "Wykres reszt dewiancyjnych")
abline(h = 0, col = "red", lwd = 2)
add_loc_reg(residuals(model_gaussian_reduced, type = "deviance"), predict(model_gaussian_reduced, type = "link"), 
            sp = 0.2, d = 0)

mean(residuals(model_gaussian_reduced, type = "deviance"))
2/6*sqrt(summary(model_gaussian_reduced)$dispersion)
skewness(residuals(model_gaussian_reduced, type = "deviance"))

plot(residuals(model_gaussian_reduced, type = "pearson") ~ predict(model_gaussian_reduced, type = "link"),
     col = "blue", pch = 20, xlab = "Predyktor liniowy", ylab = "Reszty preasona", 
     main = "Wykres reszt Peasona")
abline(h = 0, col = "red", lwd = 2)
add_loc_reg(residuals(model_gaussian_reduced, type = "pearson"), predict(model_gaussian_reduced, type = "link"), 
            sp = 0.2, d = 0)

mean(residuals(model_gaussian_reduced, type = "pearson"))
skewness(residuals(model_gaussian_reduced, type = "pearson"))


# Wykes kwantylowy
par(mfrow = c(1,1))

n_obs <- length(data_set$ClaimAmount)

u_probabilities <- pinvgauss(data_set$ClaimAmount, mean = model_prediction_gaus,
                             dispersion = summary(model_gaussian_reduced)$dispersion)
z_scores <- qnorm(u_probabilities, mean = 0, sd = 1)
sorted_residuals <- sort(z_scores)
theoretical_quantiles <- qnorm((1:n_obs) / (n_obs + 1), mean = 0, sd = 1)


plot(theoretical_quantiles, sorted_residuals,
     main = "QQ Plot: Inverse Gaussian GLM",
     xlab = "Sample normal quantile",
     ylab = "Theoretical normal quantile",
     col = "blue", 
     pch = 20, 
     cex = 0.8,
     xlim = c(-5, 5), 
     ylim = c(-5, 5))
abline(0, 1, col = "red", lwd = 2)

# Analiza wsp dyspersji
par(mfrow = c(1,2))
summary(model_gaussian_reduced)$dispersion


sq_residuals_gaus_1 = (data_set$ClaimAmount-model_prediction_gaus)^2/(1-hatvalues(model_gaussian_reduced))
sq_residuals_gaus_2 = sq_residuals_gaus_1/model_prediction_gaus^2

plot(sq_residuals_gaus_2 ~ log(model_prediction_gaus),
     main = "Ocena funkcji wariancji",
     xlab = "Predyktor liniowy", ylab = "Kwadrat reszt Pearsona", 
     col = "blue", pch = 20, ylim = c(0,10))
abline(h = mean(sq_residuals_gaus_2), col = "red", lwd = 2)
add_loc_reg(sq_residuals_gaus_2, log(model_prediction_gaus), sp = 0.20, d = 0)

plot(log(sq_residuals_gaus_1) ~ log(model_prediction_gaus), col = "blue", pch = 20,
     xlab = "Logarytm predykcji",ylab = "Logarytm estymatora wariancji", main = "Zależność wariancji od wartości oczekiwanej")
abline(log(summary(model_gaussian_reduced)$dispersion), 3, col = "red", lwd = 2)

# Porównanie gamma GLM i inverse gaussian GLM
exp_vs_obs_plot(data_set$ClaimAmount, model_prediction, wghts, n = 10)
mtext("Model Gamma GLM", side = 3, line = 3, font = 2)
exp_vs_obs_plot(data_set$ClaimAmount, model_prediction_gaus, wghts, n = 10)
mtext("Model Inverse Gaussian GLM", side = 3, line = 3, font = 2)

AIC(model_reduced)
AIC(model_gaussian_reduced)

## Inny model GLM
data_set$PastClaims <- as.factor(data_set$PastClaims) #Past claims traktowane jako kategorie
levels(data_set$PastClaims)[levels(data_set$PastClaims) %in% c("5", "6", "7")] <- "5+" #połączenie kategorii



model_2 = glm(ClaimAmount ~ LicAge + DrivAge + LicAge:DrivAge + RiskArea + PastClaims,
            family = Gamma(link = "log"), data_set)

model_prediction_2 = predict(model_2, type = "response")
# Lift plot
par(mfrow = c(1,2))

exp_vs_obs_plot(data_set$ClaimAmount, model_prediction, wghts, n = 10)
mtext("Model oryginalny", side = 3, line = 3, font = 2)
exp_vs_obs_plot(data_set$ClaimAmount, model_prediction_2, wghts, n = 10)
mtext("Model z interakcją i ze zdyskretyzowaną zmienną", side = 3, line = 3, font = 2)

# Dewiancja
pearson_1 <- sum((residuals(model_reduced,type = "pearson"))^2)
deviance_1 <- sum((residuals(model_reduced,type = "deviance"))^2)

pearson_2 <- sum((residuals(model_2,type = "pearson"))^2)
deviance_2 <- sum((residuals(model_2,type = "deviance"))^2)

tabela_dyspersji <- data.frame(
  Metryka = c("Pearson Dispersion", "Deviance Dispersion"),
  Model_oryginalny   = round(c(pearson_1, deviance_1), 4),
  Model_zmodyfikowany = round(c(pearson_2, deviance_2), 4)
)

print(tabela_dyspersji)

AIC(model_reduced)
AIC(model_2)

par(mfrow = c(1,1))

# GLM Tweediego
model_tweedie = model_2

p_seq = seq(2, 4, 0.1)

aic_tweedie = c()

# Znalezienie najlepszego p
for (p in p_seq){
  
  model_tweedie = glm(ClaimAmount ~ LicAge + DrivAge + LicAge:DrivAge + RiskArea + PastClaims,
                      data = data_set,
                      weights = wghts,
                      family = tweedie(var.power = p, link.power = 0),
                      start = coef(model_tweedie))
  
  model_tweedie_prediction = predict(model_tweedie, type = "response")
  
  q = tweedie.profile(data_set$ClaimAmount ~ offset(log(model_tweedie_prediction)) - 1, 
                      method = "series",
                      p.vec = p, 
                      link.power = 0, 
                      weights = wghts,
                      do.smooth = FALSE, 
                      do.plot = FALSE,
                      phi.method = "mle")
  
  aic_tweedie = c(aic_tweedie, AICtweedie(model_tweedie, dispersion = q$phi.max)+2)
  
}

plot(aic_tweedie ~ p_seq, type = "l", col = "blue", lwd = 2,
     main = "AIC Tweedie", ylab = "", xlab = "p")

plot(aic_tweedie[1:10] ~ p_seq[1:10], type = "l", col = "blue", lwd = 2,
     main = "AIC Tweedie", ylab = "", xlab = "p") #p dla którego AIC jest minimalne to 2.5

model_tweedie = glm(ClaimAmount ~ LicAge + DrivAge + LicAge:DrivAge + RiskArea + PastClaims,
                    data = data_set,
                    weights = wghts,
                    family = tweedie(var.power = 2.6, link.power = 0),
                    start = coef(model_2))

model_tweedie_prediction = predict(model_tweedie, type = "response")


# Model Double GLM
glm_formula = paste("~ Gender + LicAge + DrivAge + LicAge:DrivAge + RiskArea + PastClaims")

model_double_gamma = fit_double_gamma_glm(glm_formula, 
                                          glm_formula,
                                          data_set$ClaimAmount, 
                                          wghts, 
                                          data_set, 10)

model_double_gamma_severity = model_double_gamma$model_double_gamma
model_double_gamma_dispersion = model_double_gamma$model_double_gamma_phi

summary(model_double_gamma_severity)
summary(model_double_gamma_dispersion)
model_double_gamma$optimization

model_double_gamma_prediction = predict(model_double_gamma_severity, type = "response")/predict(model_double_gamma_dispersion, type = "response")
model_phi_prediction = predict(model_double_gamma_dispersion, type = "response")


## Porównanie modeli GLM, GLM Tweediego i Double GLM

# wartosc funkcji wiarygodnosci
log_lik <- tail(model_double_gamma$optimization, 1)

# liczba parametrow
k_srednia <- length(coef(model_double_gamma$model_double_gamma))
k_dyspersja <- length(coef(model_double_gamma$model_double_gamma_phi))
k <- k_srednia + k_dyspersja

aic_double_glm <- -2 * log_lik + 2 * k #aic dla double glm
aic_gamma <- AIC(model_2) #aic modelu podstawowego
aic_tweedie <- min(aic_tweedie) #aic dla tweedie dla najlepszego p

#porównanie aic modeli
tabela_aic <- data.frame(
  Metryka = "AIC",
  Model_oryginalny = aic_gamma,
  Model_tweediego = aic_tweedie,
  Model_double_glm = aic_double_glm
)
tabela_aic

#Lift plot dla 3 modeli
par(mfrow = c(1,3))
exp_vs_obs_plot(data_set$ClaimAmount, model_prediction_2, wghts, n = 10)
mtext("Model oryginalny", side = 3, line = 3, font = 2)

exp_vs_obs_plot(data_set$ClaimAmount, model_tweedie_prediction, wghts, n = 10)
mtext("Model Tweediego", side = 3, line = 3, font = 2)

exp_vs_obs_plot(data_set$ClaimAmount, model_double_gamma_prediction, wghts, n = 10)
mtext("Model dobule GLM", side = 3, line = 3, font = 2)

AIC(model)
