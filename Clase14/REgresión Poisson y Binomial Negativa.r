options(scipen=111)
library(faraway)
summary(gala$Species)
hist(gala$Species)

modelo_reg <- lm(Species~ Area + Elevation + Nearest + Scruz + 
                   Adjacent, gala)
summary(modelo_reg)
hist(modelo_reg$residuals)
predicciones <- predict(modelo_reg)
summary(predicciones)
table(predicciones < 0)
plot(gala$Species, predict(modelo_reg), pch = 20)
summary(modelo_reg)

modp <- glm(Species~ Area + Elevation + Nearest + Scruz + Adjacent,
            family=poisson, gala)
summary(modp)
MSE_lm <- mean((gala$Species - predict(modelo_reg))^2)
MSE_poisson <- mean((gala$Species - exp(predict(modp)))^2)
MSE_lm
MSE_poisson
# SCM / SCT
SCM <- var(exp(predict(modp))) * 29
SCT <- var(gala$Species) * 29
SCM / SCT

plot(gala$Endemics, gala$Species )
halfnorm(residuals(modp))

################### Binomial negativa ############
# Regresión Binomial Negativa
library(MASS)
fit_nb <- glm.nb(Species~ Area + Elevation + Nearest + Scruz + Adjacent,
                 data = gala)
summary(fit_nb)
predict(fit_nb)
MSE_nb <- mean((gala$Species - exp(predict(fit_nb)))^2)
MSE_nb



# Análisis de sobredipersion
plot(log(fitted(modp)),log((gala$Species- fitted(modp))^2),
     xlab=expression(hat(mu)),ylab=expression((y-hat(mu))^2))
abline(0,1)


(dp <-  sum(residuals(modp,type="pearson")^2)/modp$df.res)

summary (modp, dispersion=dp)


dropl(modp,test="F")


# Dosis de radiación en anormalidad de cromosonas
# ca: número de anormalidades en los cromosonas
# cell: número de ceulas en millones
# doseamt: cantidad de dosis en grays (unidad de radición)
# doserate: tasa de radiación por hora

data(dicentric)
round(xtabs(ca/cells ~ doseamt + doserate, dicentric),2)

with(dicentric,interaction.plot(doseamt,doserate,ca/cel ls))

summary(dicentric$ca / dicentric$cells)

lmod <- lm(ca/cells ~ log(doserate)*factor(doseamt),  dicentric)
summary(lmod)

plot(residuals(lmod) ~
       fitted(lmod),xlab="Fitted",ylab="Residuals")
abline(h=0)

dicentric$dosef <- factor(dicentric$doseamt)
pmod <- glm(ca ~ log(cells)+log(doserate)*dosef,
            family=poisson,dicentric)
summary(pmod)