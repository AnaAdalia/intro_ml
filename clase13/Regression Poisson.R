options(scipen=111)
library(faraway)
modp <- glm(Species~ Area+ Elevation+ Nearest+ Scruz +Adjacent ,family=poisson, gala)
summary(modp)

halfnorm(residuals(modp))

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


