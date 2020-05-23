# 
# datos <- read.delim("clipboard")
# dput(datos)

datos <- data.frame(ID = 1:12, X1 = c(16L, 12L, 13L, 11L, 10L, 9L, 
                                          8L, 7L, 5L, 3L, 2L, 0L), 
                        X2 = c(8L, 10L, 6L, 2L, 8L, -1L, 4L, 
                             6L, -3L, -1L, -3L, 0L), stringsAsFactors = F)
cor(datos[,-1])
cov(datos[,-1])
colMeans(datos[,-1])
plot(datos$X1, datos$X2, pch = 20)

datosC <- datos
datosC[,-1] <- scale(datosC[,-1], center = T,scale = F)
colMeans(datosC[,-1])
cov(datosC[,-1])
cor(datosC[,-1])

X_c <- datosC[,-1]
n <- nrow(X_c)
nsimula <- 8
Y1 <- matrix(0, n, nsimula)
Y2 <- matrix(0, n, nsimula)

for(i in 1:n){
  for(j in 1:nsimula){
    Y1[i,j] <- X_c[i,1] * cos(j * 10 / 180 * pi) + X_c[i,1] * sin((j * 10 / 180) * pi) 
    Y2[i,j] <- -X_c[i,1] * sin(j * 10 / 180 * pi) + X_c[i,1] * cos((j * 10 / 180) * pi)
    }
}

var_y1 <- diag(cov(Y1))
var_y2 <- diag(cov(Y2))
var_y1 + var_y2
var_y1 / (var_y1 + var_y2) # j = 4 , retiene el 99%

# En 40 grados se maximiza
4 * 10 / 180 * pi


############# 43.261
Comp <- matrix(0, n, 2)
for(i in 1:n){
    Comp[i,1] <- X_c[i,1] * cos(43.261 / 180 * pi) + X_c[i,2] * sin(43.261 / 180 * pi) 
    Comp[i,2] <- -X_c[i,1] * sin(43.261 / 180 * pi) + X_c[i,2] * cos(43.261 / 180 * pi)
  }

cov(Comp)
diag(cov(Comp))
diag(cov(Comp))[1] / sum(diag(cov(Comp)))


# Eigenvectores y eigenvalores
S <- cov(X_c)
eigen(S)
C1 <- -0.72 * X_c[,1] - 0.68 *  X_c[,2]
var(C1)
sum(diag(S))
var(C1) / sum(diag(S)) # Proporción de variabilidad explicada por C1 (sobre la varianza total) 

################ Ejemplo #######################################
library(FactoMineR)
data(iris)
df <- iris[,-5]
cov(df)
df_escalado <- scale(df)
cov(df_escalado)
colMeans(df_escalado)

pca_iris <- PCA(X = df_escalado,graph = F, scale.unit = F)
# Porcentajes de explicación de varianza de cada componente (eigenvalores)
pca_iris$eig # Valores propios
df_componentes <- pca_iris$ind$coord # Componentes principales 
cor(df_componentes)

PCA(X = df_escalado,graph = T, scale.unit = F)

# a_i, pesos para hacer la combinación lineal
pca_iris$var$coord
head(df_escalado)
# Revisar
 #0.8871966 * -0.8976739 -0.4586063 * 1.01560199  + 0.9882445 * -1.335752 + 0.9617570 *  -1.311052

# Norma de la primera componente principal
t(matrix(pca_iris$ind$coord[,1])) %*% matrix(pca_iris$ind$coord[,1])

# modelo <- RandomForest(iris$Species ~ df_componentes[,1] + df_componentes[,2] )
# modelo <- RandomForest(Species ~ ., data = iris)


df_contribuciones <- pca_iris$ind$contrib
colSums(pca_iris$ind$contrib)
# que tna bien representados quedan los individuos en cada una de las componentes principales
df_cos <- pca_iris$ind$cos2
rowSums(df_cos)

pca_iris$var$coord
# 0.89 * Sepal.Length - 0.45 * Sepal.Width + 0.98 * Petal.Length + 0.96 * Petal.Width 


# Cada componente con que variable está correlacionado
pca_iris$var$cor
cor(iris$Sepal.Length, pca_iris$ind$coord[,1]) # Correlación de SepalLength con la 1ra comp princ.


# c1 = a1 X1 + a2 X2 + a3 X3
# c2 = 
# Para reconstruir a partir de las variables originales escaladas como crear una lineal
loadings <- sweep(pca_iris$var$coord,2,sqrt(pca_iris$eig[,1]),FUN="/")
0.5210659 * iris$Sepal.Length[1] + -0.2693474 * iris$Sepal.Width[1] +
  0.5804131 * iris$Petal.Length[1]  + 0.5648565 * iris$Petal.Width[1] 

# Con kmeans haciendo 2 componentes principales

############### Ejercicio #######################################
# Objetivo clasificar de forma supervisada las 150 flores a partir de ACP
# Paso 1: Crear las componentes principales
pca_iris <- FactoMineR::PCA(df_escalado, graph = F)
componentes <- pca_iris$ind$coord

# Paso2: Evaluar con cuantas con componentes me puedo quedar
pca_iris$eig
# Me quedo con dos componentes

# Selecciona un conjunto de prueba para realizar mi clasificación no supervisada
set.seed(12345)
train <- as.data.frame(df_escalado[sample(150, 100),])
# Paso3: COnservar las dos primeras componentes principales
componentes_train <- FactoMineR::predict.PCA(pca_iris, train)
componentes_train <- as.data.frame(componentes_train$coord[,1:2])
kmedias <- kmeans(componentes_train, centers = 3)

train$grupo <- kmedias$cluster
componentes_train$grupo <- kmedias$cluster
aggregate(Sepal.Length ~ grupo, FUN = mean , data = train)
aggregate(Sepal.Width ~ grupo, FUN = mean , data = train)
library(ggplot2)
ggplot(data = componentes_train, aes(x = Dim.1 , Dim.2, color = factor(grupo))) +
  geom_point()


ggplot(data = train, aes(x = Sepal.Length , Sepal.Width, color = factor(grupo))) +
  geom_point()

# Si lo hiciera con la información recomendación
kmedias2 <- kmeans(iris[,-5], centers = 3)
