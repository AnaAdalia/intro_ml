#load libraries
library(rstan)
library(coda)
library(betareg)
library(dplyr)
library(ggplot2)
library(shinystan)


#################### Regresión bayesiana con una normal ############# 

set.seed(20151204)
#the explanatory variables
dat<-data.frame(x1=runif(100,-2,2),x2=runif(100,-2,2))
#the model
X<-model.matrix(~x1*x2,dat)
#the regression slopes
set.seed(12345)
betas<-runif(4,-1,1)
#the standard deviation for the simulated data
sigma<-1
#the simulated data
y_norm<-rnorm(100,X%*%betas,sigma)
#a matrix to get the predicted y values
new_X<-model.matrix(~x1*x2,expand.grid(x1=seq(min(dat$x1),
                                              max(dat$x1),length=20),
                                       x2=c(min(dat$x2),
                                            mean(dat$x2),max(dat$x2))))



#the location of the model files
setwd("C:/Users/Home/Documents/Laboral2020/Konrad Lorenz/MachineLearning/Clase 16")
dir()
#the model
m_norm<-stan(file="lm.stan",
             data = list(N=100,N2=60,K=4,y=y_norm, X=X, new_X = new_X),
             pars = c("beta","sigma","y_pred"))

shinystan::launch_shinystan(m_norm)


#plotting the posterior distribution for the parameters
post_beta<-As.mcmc.list(m_norm,pars="beta")
plot(post_beta)

#computing the posterior probability for the slopes to be bigger than 0
apply(extract(m_norm,pars="beta")$beta,2,
      function(x) length(which(x>0))/4000)

#plot the correlation between the parameters
pairs(m_norm,pars="beta")


#plotting credible intervals for the different betas
plot(m_norm,pars=c("beta","sigma"))




###################### Regresión beta ##############################
#https://www.rpubs.com/kaz_yos/stan_beta1
data(FoodExpenditure, package = "betareg")
FoodExpenditure <-
  FoodExpenditure %>%
  as_tibble() %>%
  mutate(p_food = food / income)
FoodExpenditure

X11()
ggplot(data = FoodExpenditure, mapping = aes(x = persons, y = p_food)) +
  geom_point() +
  scale_y_continuous(limits = c(0,1)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.key = element_blank(),
        plot.title = element_text(hjust = 0.5),
        strip.background = element_blank())


setwd("C:/Users/Home/Documents/Laboral2020/Konrad Lorenz/MachineLearning/Clase 16")
N <- nrow(FoodExpenditure)
y <- FoodExpenditure$p_food
##  Mean model part
X1 <- model.matrix(object = ~ income + persons, data = FoodExpenditure)
X1_dim <- ncol(X1)
beta_x1_mean <- rep(0, X1_dim)
beta_x1_sd <- c(10, rep(1, X1_dim-1))
##  Precision model part
X2 <- model.matrix(object = ~ income + persons, data = FoodExpenditure)
X2_dim <- ncol(X2)
beta_x2_mean <- rep(0, X2_dim)
beta_x2_sd <- c(10, rep(1, X2_dim-1))


stan_logit_log_beta_model_fit1 <-
  rstan::stan(file = "betareg.stan" ,
              data = list(N = N,
                          y = y,
                          X1 = X1,
                          X1_dim = X1_dim,
                          beta_x1_mean = beta_x1_mean,
                          beta_x1_sd = beta_x1_sd,
                          X2 = X2,
                          X2_dim = X2_dim,
                          beta_x2_mean = beta_x2_mean,
                          beta_x2_sd = beta_x2_sd),
              chains = 3,
              cores = 3,
              verbose = TRUE)

pairs(stan_logit_log_beta_model_fit1, pars = c("beta_x1","beta_x2","lp__"))


traceplot(stan_logit_log_beta_model_fit1, pars = c("beta_x1","beta_x2"), inc_warmup = TRUE)
shinystan::launch_shinystan(stan_logit_log_beta_model_fit1)
print(stan_logit_log_beta_model_fit1, pars = c("beta_x1","beta_x2","lp__"))
plot(stan_logit_log_beta_model_fit1, pars = c("beta_x1","beta_x2"))





# ################# Regresión logística #########################
# # https://medium.com/@alex.pavlakis/making-predictions-from-stan-models-in-r-3e349dfac1ed
# options(mc.cores = parallel::detectCores())
# rstan_options(auto_write = TRUE)
# # Create some fake data - logistic regression
# set.seed(56)
# N <- 500
# alpha <- 1
# beta <- 2
# x <- rnorm(N)
# prob <- 1/(1 + exp(-(alpha + beta*x)))
# y <- rbinom(N, 1, prob)
# # Distribution of y
# table(y)
# ## y
# ##  0   1 
# ##  171 329
# # Split into training and testing
# N_train <- N*0.8
# N_test <- N*0.2
# train_ind <- sample(c(1:N), size = N_train, replace = FALSE)
# x_train <- x[train_ind]
# x_test <- x[-train_ind]
# y_train <- y[train_ind]
# y_test <- y[-train_ind]
# 
# 
# setwd("C:/Users/Home/Documents/Laboral2020/Konrad Lorenz/MachineLearning/Clase 16")
# # Recover parameters with stan
# fit <- stan(file = "reglogistica.stan",
#             data = list(x_train, y_train, N_train,
#                         x_test, N_test),
#             chains = 3, iter = 1000)
# plot(fit, pars = c("alpha", "beta"))
# 
# traceplot(fit, pars = c("alpha", "beta"))
# 
# shinystan::launch_shinystan(stan_logit_log_beta_model_fit1)
# 
# 
# # Accuracy
# ext_fit <- extract(fit)
# mean(apply(ext_fit$y_test, 2, median) == y_test)
# 
# 
# # Predecir con nuevos datos:
# # Extract posteriod distributions
# alpha_post <- ext_fit$alpha
# beta_post <- ext_fit$beta
# # Function for simulating y based on new x
# gen_quant_r <- function(x) {
#   lin_comb <- sample(alpha_post, size = length(x)) + x*sample(beta_post, size = length(x))
#   prob <- 1/(1 + exp(-lin_comb))
#   out <- rbinom(length(x), 1, prob)
#   return(out)
# }
# # Run the function on x_test
# set.seed(56)
# y_pred_r <- gen_quant_r(x_test)
# # Accuracy
# mean(y_pred_r == y_test)
# 
# 
# # Predecir valores con la muestra test:
# 
# pred <- stan(file = "prediccionLogisticaStan.stan",
#              data = list(x_test = x_test, N = N_test,
#                          N_samples = length(alpha_post),
#                          alpha = alpha_post,
#                          beta = beta_post),
#              chains = 1, iter = 100,
#              algorithm = "Fixed_param")
# 
# # Extract and format output
# ext_pred <- extract(pred)
# out_mat <- matrix(NA, nrow = dim(ext_pred$y_test)[2],
#                   ncol = dim(ext_pred$y_test)[3])
# for(i in 1:dim(ext_pred$y_test)[2]) {
#   for(j in 1:dim(ext_pred$y_test)[3]) {
#     out_mat[i, j] <- mean(ext_pred$y_test[, i, j])
#   }
# }
# # Accuracy
# (apply(out_mat, 2, median) %>% round(0) == y_test) %>% mean()
# 

