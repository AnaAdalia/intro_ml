setwd("~/Laboral2020/Konrad Lorenz/MachineLearning/Clase2")
dir()
insurance <- read.csv("insurance.csv")
# Modelo explicativo (entrena con todos los datos)1
# Matriz de correlaciones entre charges: bmi, age
cor(insurance[c("bmi", "age", "charges")])
plot(cor(insurance[c("bmi", "age", "charges")]))
boxplot(charges ~ sex, data = insurance)
aggregate(charges ~ sex, data = insurance, FUN = mean)
library(ggplot2)
ggplot(data = insurance, 
       aes(x = charges, color = sex)) +
  geom_density()
boxplot(charges ~ smoker, data = insurance)
boxplot(charges ~ region, data = insurance)
boxplot(charges ~ children, data = insurance)

modelo <- lm(charges ~ ., data = insurance)
summary(modelo)
step(modelo) # Procedimiento de selección de variables
modelo2 <- lm(charges ~ . - sex, 
              data = insurance)
summary(modelo2)

n <- nrow(insurance)
n_train <- round(0.8 * n)
set.seed(12345)
indica_train <- sample(n, n_train) 
training <- insurance[indica_train,]
test <- insurance[setdiff(1:n, indica_train),]
modelo_train <- lm(charges ~ age + bmi +
                     children + smoker +
                     region, 
              data = training)
RMSE_train <- sqrt(mean((training$charges - 
               modelo_train$fitted.values)^2))
R2_train  <- summary(modelo_train)$r.squared

# Predicciones del modelo en test
yhat_test <- predict(modelo_train, 
                     test[-c(2, 7)])
yhat_test <- predict(modelo_train, 
                     test[c("age" ,"bmi",
                            "children",
                            "smoker",
                            "region")])
RMSE_test <- sqrt(mean((test$charges - 
                          yhat_test)^2))

plot(insurance$charges ~ insurance$age)
plot(insurance$charges ~ insurance$bmi)
mod <- lm(charges ~ bmi + I(bmi^2), 
          data = insurance)
summary(mod)

mod <- lm(charges ~ age + I(children^2), 
          data = insurance)
summary(mod)


##################### Modelo 2 (Cúbico en bmi) ############
modelo2_train <- lm(charges ~ age + 
                     bmi + I(bmi^2) + I(bmi^3)+  
                     children + smoker +
                     region, 
                   data = training)

yhat_test_m2 <- predict(modelo2_train, 
                     test[-c(2, 7)])
RMSE_test_m2 <- sqrt(mean((test$charges - 
                             yhat_test_m2)^2))

library(caret)
plot(test$charges, yhat_test_m2)
abline(a = 1, b = 1)
