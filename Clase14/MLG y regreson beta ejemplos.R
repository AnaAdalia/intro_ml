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
hist(nhanes$sed_gamma)

nhanes2 <- nhanes[,c("sed_gamma", "asthma", "race", 
                     "famsize")]
nhanes2 <- na.omit(nhanes2)
modelo_lineal <- lm(sed_gamma ~ asthma + race + famsize,
                    data = nhanes2)
summary(modelo_lineal)
MSE_ml <- mean((modelo_lineal$res^2))

g_fit <- glm(sed_gamma ~ asthma + race + famsize,
             data = nhanes2,
             family = Gamma(link = "log")
)
summary(g_fit)
exp(predict(g_fit))
MSE_gamma <- mean((exp(predict(g_fit)) - nhanes2$sed_gamma)^2)


g_fit <- glm(sed_gamma ~ asthma + race + famsize,
             data = nhanes,
             family = "Gamma"
)

1/predict(g_fit)


# Modelar el ingreso percapita de la EMB: en términos del IPM, el estrato,
# hacinamiento, la propiedad de la vivienda, y puntaje.

# Regresión beta
## Variable bound between 0 and 1
nhanes$beta_var <- sample(seq(.05, .99, by = .01), 
                          size = length(nhanes$asthma),
                          replace = TRUE)
summary(nhanes$beta_var)
library(betareg)
fit_beta <- betareg(beta_var ~ asthma + race + famsize,
                    data = nhanes)
summary(fit_beta)

# Ejercicio en clase: modelar el índice pobreza multidimensional / 100
# ¿Qué tan relacionado está con el puntaje socioeconómico, el ingreso y el estrato,
# hacinamiento?
#Google :  encuesta multiproposito planeacion
# Variables adicionales hogar

# Ejercicio 15 min
setwd("C:/Users/Home/Documents/Laboral2020/Konrad Lorenz/MachineLearning/Clase 14")
library(readr)
emb <- read_delim("variables_adicionales_hogar_v3.txt", delim = ";")
hogares <- read_delim("Hogares_2017_V2_03092018.txt", delim = ",")
hogares <- hogares[c("DIRECTORIO_HOG", "NHCCPCTRL2", "NHCCP20")]
hogares$Hacinamiento <- hogares$NHCCPCTRL2 / hogares$NHCCP20
names(hogares)
library(dplyr)
emb <- left_join(emb, hogares, by = "DIRECTORIO_HOG")
#Estrato viv, hacinamiento  ingr  eso  TOTAL_PUNTAJE
# pronosticar ipm 
#IPM ~ hacinamiento ´+ factor(ESTRATO_VIV) + 
     
# Calcular el MSE, compararlo con de la regresión lineal
  # Calcular el R2 y compararlo de la regresión lineal
datos <- emb[,c("IPM", "Hacinamiento", "TOTAL_PUNTAJE", 
                "INGRESOS_PER_CAPITA", "ESTRATO_VIV")]
summary(datos)
table(datos$IPM == 0)
datos$IPM[datos$IPM == 0] <- 0.001
modelobeta <- betareg(IPM ~ ., data = datos)
datos$pred <- predict(modelobeta)
