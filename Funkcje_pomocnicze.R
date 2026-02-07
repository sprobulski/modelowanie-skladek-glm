#Biblioteki

library(MASS)
library(lsr)
library(zoo)
library(xts)
library(sp)
library(dplyr)
library(locfit)
library(moments)
library(monotone)
library(tweedie)
library(statmod)

#install.packages("CASdatasets", repos = "http://cas.uqam.ca/pub/", type = "source")
library(CASdatasets)

#Funkcje pomocnicze

discretize_var <- function(x,e,n){
  
  x_m = cbind(x, e)
  x_m = x_m[order(x),]
  x_e = cumsum(x_m[,2])
  
  x_e_cut = x_e[length(x_e)]*seq(1/n,1,1/n)
  
  cut_range = min(x)-1
  
  for (j in x_e_cut){
    
    cut_range = c(cut_range, x_m[max(which(x_e<=j)),1])  
  }
  
  cut_range = unique(cut_range)
  
  x_cut = cut(x, breaks = cut_range, labels = cut_range[-1])
  
  return(x_cut)
}

add_loc_reg <- function(y, x, sp, d){
  
  model_auto = locfit(y~x,
                      alpha = sp, deg = d, kern = "rect")
  smooth_fit = predict(model_auto, newdata = sort(x))
  
  lines(sort(x), smooth_fit, col = "green", lwd = 2)
  
}

auto_calibrate <- function (y, x, w) {
  
  data_iso = data.frame(x = x, y = y, w = w)
  data_iso = data_iso[order(x),]
  y_predict_monotone = monotone(x = data_iso$y, w = data_iso$w)
  
  fit_calibrate = stepfun(sort(x), c(0,y_predict_monotone))
  
  return(fit_calibrate)
}

#Estymacja empiryczna

one_way_plot <- function (y, x, e, x_name, n, bp) { 
  
par(mfrow= c(1, ifelse(bp=="TRUE", 3, 2)))
  
  if (n_distinct (x) < 20) {
    
    response = c()
    exposure = c()
    categories = sort(unlist(unique(x)))
    
    for (i in categories){

    exposure = c(exposure,
                 sum(e[which(x == i)])) 
    response = c(response,
                 sum(y[which(x == i)]*e[which(x == i)])/sum(e[which(x == i)]))
    }
    
    barplot(exposure, names.arg = categories, col = "blue",
            xlab = x_name,
            ylab = "", main = "Exposure", las = 2)
                                        
    plot(response, xaxt = "n", type = "b", pch = 16, lwd = 2,
         xlab = x_name, ylab = "", main = "Mean estimate of response", col = "blue")
    axis(1, at = c(1:length(categories)), labels = categories, las = 2)

    if (bp=="TRUE") {
    
      boxplot(y ~ unlist(x), outline = FALSE,
              xlab = x_name, ylab = "", main = "Distribution of response", col = "blue")
      
      abline (h = sum (y*e)/sum (e), col="orange")
    }
    
    }else{
      
    x_cut = discretize_var(x,e,n)
    x_cut_n = as.numeric(paste(x_cut))
      
    response = c()
    exposure = c()
      
    for (i in sort(unique(x_cut))){
        
      exposure = c(exposure,
                   sum(e[which(x_cut == i)])) 
      response = c(response,
                   sum(y[which(x_cut == i)]*e[which(x_cut == i)])/sum(e[which(x_cut == i)]))
    }

    barplot(exposure, names.arg = round(sort(unique(x_cut_n)),2),
              xlab = x_name, ylab = "", main = "Exposure", las = 2, col = "blue")
      
    plot(response, xaxt = "n", type = "b", pch = 16, lwd = 2,
         xlab = x_name, ylab = "", main = "Mean estimate of response", col = "blue")
    
    axis(1, at = c(1:length(sort(unique(x_cut_n)))), labels = round(sort(unique(x_cut_n)),2), las = 2)
    
    if (bp=="TRUE") {
      
      boxplot(y ~ unlist(x_cut), outline = FALSE,
              xlab = x_name, ylab = "", main = "Distribution of response", col = "blue")
      
      abline (h = sum (y*e)/sum (e), col="orange")
    }
    }
  
  par(mfrow = c(1,1))
}  

pareto_plots <- function(y_var, u){
  
  par(mfrow= c(1,3))
  
  pareto_claims = sort (y_var)
  
  q = c(1:length(pareto_claims))/(length(pareto_claims)+1)
  
  plot(log(1-q[which(q>u)]) ~ log(pareto_claims[which(q>u)]), pch = 20,
       main = "Log tail of distribution", xlab = "Log severity", ylab = "", cex = 1.25, col = "blue")
  
  theta_index = log(length(pareto_claims)+1-c(2:length (pareto_claims)))/log(length(pareto_claims)) 
  hill_claims = sapply(round(u*length(pareto_claims)):length(pareto_claims),
                       function(i){
                        length(pareto_claims[i:length(pareto_claims)])/sum(log(pareto_claims[i:length (pareto_claims)]/pareto_claims [i-1]))})
                         
  plot(hill_claims ~ theta_index[round(u*length(pareto_claims)):length(pareto_claims)], pch = 20, 
      main = "Hill estimator", xlab = "Theta", ylab = "", cex = 1.25, col = "blue")
  
  excess_claims = sapply(round(u*length(pareto_claims)):length (pareto_claims),
                       function(i){
                       mean(pareto_claims[i:length(pareto_claims)])-pareto_claims[i-1]})

  plot(excess_claims ~ pareto_claims[(round(u*length (pareto_claims))-1):(length (pareto_claims)-1)], pch = 20,
       main = "Mean excess plot", xlab = "Threshold", ylab = "", cex = 1.25, col = "blue")

par (mfrow= c(1,1))

}

#Funkcje wiarogodności i dewiancji

tweedie_deviance_function <- function(p,x_var){
  
  if (is.vector(x_var)==TRUE){
    y = x_var[1]+10^(-6)
    mu = x_var[2]
    w = x_var[3]
  }else{
    y = x_var[,1]+10^(-6)
    mu = x_var[,2]
    w = x_var[,3]
  }
  
  z = 2*mean((y*(y^(1-p)-mu^(1-p))/(1-p)-(y^(2-p)-mu^(2-p))/(2-p))*w)
  
  return(z)
}

poisson_deviance_function <- function(x_var){
  
  if (is.vector(x_var)==TRUE){
    y = x_var[1]+10^(-6)
    mu = x_var[2]
    w = x_var[3]
  }else{
    y = x_var[,1]+10^(-6)
    mu = x_var[,2]
    w = x_var[,3]
  }
  
  z = 2*mean((y*log(y/mu)-(y-mu))*w)
  
  return(z)
}

gamma_deviance_function <- function(x_var){
  
  if (is.vector(x_var)==TRUE){
    y = x_var[1]+10^(-6)
    mu = x_var[2]
    w = x_var[3]
  }else{
    y = x_var[,1]+10^(-6)
    mu = x_var[,2]
    w = x_var[,3]
  }
  
  z = 2*mean((-log(y/mu)+(y-mu)/mu)*w)
  
  return(z)
}

negbin_deviance_function<-function(alpha,x_var){
  
  if (is.vector(x_var)==TRUE){
    y = x_var[1]+10^(-6)
    mu = x_var[2]
    w = x_var[3]
  }else{
    y = x_var[,1]+10^(-6)
    mu = x_var[,2]
    w = x_var[,3]
  }
  
  z = 2*mean((y*log(y/mu)-(y+alpha)*log((y+alpha)/(mu+alpha)))*w)
    
  return(z)
} 

poisson_loglik_function<-function(x_var){
  
  y = x_var[,1]
  mu = x_var[,2]
  v = x_var[,3]
  
  z = sum(-v*mu+v*y*log(v*mu)-log(factorial(v*y)))
  
  return(z)
}

negbin_loglik_function<-function(alpha,x_var){
  
  y = x_var[,1]
  mu = x_var[,2]
  v = x_var[,3]
  
  z = sum(log(gamma(v*y+v*alpha))-log(factorial(v*y))-log(gamma(v*alpha))
          +y*v*log(mu/(mu+alpha))+alpha*v*log(1-mu/(mu+alpha)))
  
  return(z)
} 

gamma_loglik_function <- function(phi,x_var){
  
  if (is.vector(x_var)==TRUE){
    y = x_var[1]
    mu = x_var[2]
    w = x_var[3]
  }else{
    y = x_var[,1]
    mu = x_var[,2]
    w = x_var[,3]
  }
  
  a = w/phi
  b = w/phi/mu
  
  z = sum(log(dgamma(y,shape=a,rate=b)))
  
  return(z)
}

#Wykresy kwantylowe

normal_qq_plot <- function(y, m, e, phi, n){
  
  v_2 = phi/e
  quantile_residual = (y-m)/sqrt(v_2)
  sorted_std_residuals = sort(quantile_residual)
    
  normal_quantiles = qnorm(c(1:length(y))/(length(y)+1),0,1)
    
  plot(sorted_std_residuals~normal_quantiles, col = "blue", pch = 20, cex = 1,
        xlim = c(-5,5), ylim =c(-5,5),
        ylab = "Theoretical normal quantile", xlab = "Sample normal quantile", 
        main = paste("QQ plot:", n), cex.lab = 1)
  abline(0,1,col = "red", lwd = 2)

}

gamma_qq_plot <- function(y, m, e, phi, n){
  
  beta = e/m/phi
  alpha = e/phi

  quantile_residual = pgamma(y, shape = alpha, rate = beta)
  quantile_residual = qnorm(quantile_residual,0,1)
  sorted_std_residuals = sort(quantile_residual)
    
  normal_quantiles = qnorm(c(1:length(y))/(length(y)+1),0,1)
    
  plot(sorted_std_residuals ~ normal_quantiles, col = "blue", pch = 20, cex = 1,
        xlim = c(-5,5), ylim =c(-5,5),
        ylab = "Theoretical normal quantile", xlab = "Sample normal quantile", 
        main = paste("QQ plot:", n), cex.lab = 1)
  abline(0,1,col = "red", lwd = 2)

}

poisson_prob_table <- function(y,m,e){
  
  n = round(y*e,0)
  n_max = max(n)
  m = m*e
  
  obs_no_claims = c()
  exp_no_claims = c()
  
  for (i in c(0:n_max)){
  
    obs_no_claims = c(obs_no_claims,
                      length(which(n == i)))
    exp_no_claims = c(exp_no_claims,
                      sum(dpois(i,m)))
  }

  z = data.frame(cbind(no_claims = c(0:n_max), 
                       observed = obs_no_claims, 
                       expected = round(exp_no_claims,1)))

  print(z)
}

negbin_prob_table <- function(y,m,e,alpha){
  
  n = round(y*e,0)
  n_max = max(n)
  m = m*e
  
  p_negbin = 1-m/(m+alpha*e)
  
  obs_no_claims = c()
  exp_no_claims = c()
  
  for (i in c(0:n_max)){
    
    obs_no_claims = c(obs_no_claims,
                    length(which(n == i)))
    exp_no_claims = c(exp_no_claims,
                      sum(dnbinom(i, alpha*e, p_negbin)))
                    
  }
  
  z = data.frame(cbind(no_claims = c(0:n_max), 
                       observed = obs_no_claims, 
                       expected = round(exp_no_claims,1)))
  
  print(z)
}

#Metody estymacji

fit_negbin_glm <- function(glm_formula, y, e, dd, n){
  
  nb_loglik=c()
  alpha_opt=c(1)

  for (iteration_step in c(1:n)){
    
    model_nb = glm(as.formula(glm_formula),
                   family = negative.binomial(alpha_opt[length(alpha_opt)],
                                              link = "log"), 
                   weights = e, data = dd)
    
    data_loglik = cbind(y, predict(model_nb,type = "response"), e)
    
    nb_loglik = c(nb_loglik, negbin_loglik_function(alpha_opt[length(alpha_opt)],data_loglik))
    
    alpha_optimization = optimize(negbin_loglik_function, 
                                  interval = c(0,10),
                                  x = data_loglik, maximum = TRUE)
    
    alpha_opt = c(alpha_opt,alpha_optimization$maximum)
  }
  
  z = list("model_negbin" = model_nb,
           "optimization" = nb_loglik,
           "alpha" = alpha_opt[-1])
  
  return(z)
}

fit_double_gamma_glm <- function (glm_formula_1, 
                                  glm_formula_2,
                                  y, e, dd, n){
  
  y_response = y
  
  model_double_gamma = glm(as.formula(paste("y_response", glm_formula_1)),
                           weights = e,
                           data = dd,
                           family = Gamma(link = "log"))
  
  model_double_gamma_predict = predict(model_double_gamma, type = "response")
  
  model_double_gamma_phi_predict = gamma.dispersion(model_double_gamma)
  
  gamma_loglik = gamma_loglik_function(model_double_gamma_phi_predict, 
                                       cbind(y, model_double_gamma_predict, e))
  
  for (optimization_step in c(1:n)){
    
    y_response = y*model_double_gamma_phi_predict
    
    model_double_gamma = glm(as.formula(paste("y_response", glm_formula_1)),
                             weights = e/model_double_gamma_phi_predict,
                             data = dd,
                             family = Gamma(link = "log"))
    
    model_double_gamma_predict = predict(model_double_gamma, type = "response")
    
    deviance_response = ((residuals(model_double_gamma, type = "deviance"))^2/(1-hatvalues(model_double_gamma))
                         *model_double_gamma_phi_predict)
    
    model_double_gamma_phi = glm(as.formula(paste("deviance_response", glm_formula_2)),
                                 data = dd,
                                 family = Gamma(link = "log"))
    
    model_double_gamma_phi_predict = predict(model_double_gamma_phi, type = "response")
    
    gamma_loglik = c(gamma_loglik,
                     gamma_loglik_function(model_double_gamma_phi_predict, 
                                           cbind(y, model_double_gamma_predict/model_double_gamma_phi_predict, e)))
  }
  
  z = list("model_double_gamma" = model_double_gamma,
           "model_double_gamma_phi" = model_double_gamma_phi,
           "optimization" = gamma_loglik)
  
  return(z)
} 

#Walidacja wartości oczekiwanej

exp_vs_obs_plot <- function(y, m, e, n){

    m_cut = discretize_var(m,e,n)
  
    obs_estimate = c()
    exp_estimate = c()
    
    for (i in sort(unique(m_cut))){
      
      index = which(m_cut == i)
      
      obs_estimate = c(obs_estimate, sum(y[index]*e[index])/sum(e[index]))
      exp_estimate = c(exp_estimate, sum(m[index]*e[index])/sum(e[index]))
    }
    
    plot(obs_estimate ~ exp_estimate, col = "blue", 
         main = paste("Lift plot, lift ratio  = ", round(exp_estimate[length(exp_estimate)]/exp_estimate[1], 2)),
         type = "b", pch = 16, lwd = 2,
         xlim = c(min(exp_estimate, obs_estimate), max(exp_estimate, obs_estimate)),
         ylim = c(min(exp_estimate, obs_estimate), max(exp_estimate, obs_estimate)),
         xlab = "Mean prediction", ylab = "Mean response")
    abline(0,1, col="red", lwd = 2)
    
}

exp_vs_obs_var_plot <- function (y, m, x, e, x_name, n, bp) { 
  
  if (n_distinct (x) < 20) {
    
    response = c()
    predicted = c()
    exposure = c()
    categories = sort(unlist(unique(x)))
    
    for (i in categories){
      
      exposure = c(exposure,
                   sum(e[which(x == i)])) 
      response = c(response,
                   sum(y[which(x == i)]*e[which(x == i)])/sum(e[which(x == i)]))
      predicted = c(predicted,
                   sum(m[which(x == i)]*e[which(x == i)])/sum(e[which(x == i)]))
    }
    
    plot(response, xaxt = "n", 
         ylim = c(min(response, predicted), max(response, predicted)),
         type = "b", pch = 16, lwd = 2,
         xlab = x_name, ylab = "", main = "Mean response vs mean prediction", col = "blue")
    lines(predicted, col = "green", type = "b", pch = 16, lwd = 2)
    axis(1, at = c(1:length(categories)), labels = categories, las = 2)
    
  }else{
    
    x_cut = discretize_var(x,e,n)
    x_cut_n = as.numeric(paste(x_cut))
    
    response = c()
    predicted = c()
    exposure = c()
    
    for (i in sort(unique(x_cut))){
      
      exposure = c(exposure,
                   sum(e[which(x_cut == i)])) 
      response = c(response,
                   sum(y[which(x_cut == i)]*e[which(x_cut == i)])/sum(e[which(x_cut == i)]))
      predicted = c(predicted,
                   sum(m[which(x_cut == i)]*e[which(x_cut == i)])/sum(e[which(x_cut == i)]))
    }
    
    plot(response, xaxt = "n", 
         ylim = c(min(response, predicted), max(response, predicted)),
         type = "b", pch = 16, lwd = 2,
         xlab = x_name, ylab = "", main = "Mean response vs mean prediction", col = "blue")
    lines(predicted, col = "green", type = "b", pch = 16, lwd = 2)
    
    axis(1, at = c(1:length(sort(unique(x_cut_n)))), labels = round(sort(unique(x_cut_n)),2), las = 2)
    
  }
  
}  

#Krzywe Lorenza i koncentracji

concentration_curve <- function (y, m, e){
  
    quantile_points = quantile(m, seq(from = 0, to = 1, by = 0.01))
    
    curve_c = c(0, sapply(c(1:length (quantile_points)),
                          function(i){sum(y[which(m <= quantile_points[i])]*e[which(m <= quantile_points[i])])}/sum(y*e)))
    
    curve_l = c(0, sapply(c(1:length (quantile_points)),
                          function(i){sum(m[which(m <= quantile_points[i])]*e[which(m <= quantile_points[i])])}/sum(y*e)))
    
    quantile_points_y = quantile(y, seq(from = 0, to = 1, by = 0.01))
    
    curve_y = c(0, sapply(c(1:length (quantile_points_y)),
                          function(i){sum(y[which(y <= quantile_points_y[i])]*e[which(y <= quantile_points_y[i])])}/sum(y*e)))
    
    plot(curve_c ~ c(0, seq(from = 0, to = 1, by = 0.01)), type = "l", lwd = 2,
         xlab = "Podportfele", ylab = "Szkody vs prognozy", main = "Krzywe koncentracji i Lorenza", col = "blue", 
         ylim = c(0,1), xlim = c(0,1))
    
    lines(c(0, seq(from = 0, to = 1, by = 0.01)), curve_l, col = "green", lwd = 2)
    
    lines(c(0, seq(from = 0, to = 1, by = 0.01)), curve_y, col = "red", lwd = 2)
    
    abline(0,1,col = "black", lwd = 2)
                     
    legend("topleft", legend = c(paste("Concentration =", round(sum(curve_c)*0.01, 2)),
                                 paste("Lorenz =", round(sum(curve_l)*0.01, 2))),
          col = c("blue", "green"), lty = 1, cex = 1.5)
    
    z = list("concentration" = curve_c,
             "lorenz" = curve_l,
             "gini_eco" = (0.5-sum(curve_l)*0.01)*2, 
             "gini_ml" = (0.5 - sum(curve_c)*0.01)/(0.5-sum(curve_y)*0.01),
             "abc" = (sum(curve_c)-sum(curve_l))*0.01)
    
    return(z)
}  

#Dekompozycja Murphy'ego

score_decomposition <- function (y, x, e) {
  
  fit_autocalibrate = auto_calibrate(y, x, e)
  
  y_0 = y+10^(-6)
  y_1 = x+10^(-6)
  y_2 = fit_autocalibrate(x)+10^(-6)
  y_3 = sum(y*e)/sum(e)
  
  a = poisson_deviance_function(cbind(y_0, y_1, e)) - poisson_deviance_function(cbind(y_0, y_2, e))
  b = poisson_deviance_function(cbind(y_0, y_3, e)) - poisson_deviance_function(cbind(y_0, y_2, e)) 
  c = poisson_deviance_function(cbind(y_0, y_3, e))
  
  z = data.frame(Dewiancja = poisson_deviance_function(cbind(y_0,y_1, e)), 
                 Miscalibration = a, 
                 Resolution = b, 
                 Entropy = c,
                 Skill = (b-a)/c)
  
  return(z)
}  
