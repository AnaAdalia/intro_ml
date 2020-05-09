setwd("C:/Users/Home/Documents/Laboral2020/Konrad Lorenz/MachineLearning/Clase13")
################## Modelos lineales generalizados ##################
library(betareg)

nhanes <- readRDS("nhanes.rds")
# Regresión Poisson 
p_fit <- glm(sed ~ asthma + race + famsize,
             data = nhanes,
             family = "poisson")
summary(p_fit)


# Regresión Binomial Negativa
library(MASS)
fit_nb <- glm.nb(sed_zero ~ asthma + race + famsize,
                 data = nhanes)
summary(fit_nb)

# Modelar el número de integrantes del hogar, en términos de variables
# como el IPM, el ingreso percapita del hogar (o ingreso)
# Regresión gamma
nhanes$sed_gamma <- nhanes$sed + .01
g_fit <- glm(sed_gamma ~ asthma + race + famsize,
             data = nhanes,
             family = "Gamma")
summary(g_fit)
# Modelar el ingreso percapita de la EMB: en términos del IPM, el estrato,
# hacinamiento, la propiedad de la vivienda, y puntaje.

# Regresión beta
## Variable bound between 0 and 1
nhanes$beta_var <- sample(seq(.05, .99, by = .01), 
                      size = length(nhanes$asthma),
                      replace = TRUE)
library(betareg)
fit_beta <- betareg(beta_var ~ asthma + race + famsize,
                    data = nhanes)
summary(fit_beta)

# Ejercicio en clase: modelar el índice pobreza multidimensional
# ¿Qué tan relacionado está con el puntaje socioeconómico, el ingreso y el estrato,
# hacinamiento?
