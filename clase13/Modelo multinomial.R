#Distribución multinomial

setwd("C:/Users/Home/Documents/Laboral2020/Konrad Lorenz/MachineLearning/Clase13")

library(haven)
#ml <- read.dta("http://www.ats.ucla.edu/stat/data/hsbdemo.dta")
ml = read.dta("hsbdemo.dta")

#Example 1. People's occupational choices might be influenced by
#their parents' occupations and their own education level. We can study the relationship of one's occupation choice with education level and father's occupation. The occupational choices will be the outcome variable which 
#consists of categories of occupations.

#Tipo de programa
#id: identificador, female:sexo  ses: estatus socieconómico,
#schtyp: tipo de colegio  prog: tipo de programa,
#read: puntaje de lectura   write: puntaje escritura   math: puntaje matemática   
#science: puntaje en ciencias socst: puntaje en ciencias sociales  
#honor:   awards: premios:    cid:cluster


#Objetivo predecir la vocación de los estudiantes

with(ml, table(ses, prog))
with(ml, do.call(rbind, tapply(write, prog, function(x)
  c(M = mean(x), SD = sd(x)))))

library(nnet)
ml$prog2 <- relevel(ml$prog, ref = "academic")
modelo1 <- multinom(prog2 ~ ses + write, data = ml)
summary(modelo1)

names(modelo1)

ml$pron= modelo1$fitted.values

exp(coef(modelo1))

denom=1+exp(2.852198-0.0579287*35)+exp(5.218260-0.1136037*35)
a=1/denom
b=exp(2.852198-0.0579287*35)/denom
c=exp(5.218260-0.1136037*35)/denom
a+b+c

z_mod1 <- summary(modelo1)$coefficients/summary(modelo1)$standard.errors
z_mod1

p_mod1 <- (1 - pnorm(abs(z_mod1), 0, 1))/2
p_mod1



modelo2 <- multinom(prog2 ~ ses+read  + write + math+ science + socst, data = ml)
summary(modelo2)
anova(modelo1,modelo2)
fitted.values(modelo2)

modelonulo=multinom(prog2 ~ 1, data = ml)
summary(modelonulo)
z_mod2 <- summary(modelo2)$coefficients/summary(modelo2)$standard.errors
z_mod2

p_mod2 <- (1 - pnorm(abs(z_mod2), 0, 1))/2
p_mod2


Rdevianza= 1-( 329.9511/412.1933)
