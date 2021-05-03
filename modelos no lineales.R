###########################################################################
##################### Ejemplos de regresion no lineal #####################
############## Creado por Mauricio Lopera y Freddy Hernandez ##############
###########################################################################


# Ejemplo 5 ---------------------------------------------------------------

# En este ejemplo se van a simular datos de un modelo con la
# estructura y = x ^ beta1 + e
# El objetivo es estimar beta1 de forma manual y con nls

# Vamos a fijar nosotros el parametro que vamos a estimar
beta1 <- 1.5

# Para simular los datos
n <- 100 # numero de valores a simular
x <- seq(from=1, to=n, by=1) # valores x de 1 a 200
set.seed(467382)  # fija la semilla
e <- rnorm(n=n, mean=0, sd=80) # 200 valores de normal(med=0, var=50)
y <- x^beta1 + e # genera los valores y

# Para graficar los datos simulados
plot(x, y)

# Vamos a estimar beta1 Aplicando Gauss-Newton
# https://en.wikipedia.org/wiki/Gauss%E2%80%93Newton_algorithm

k <- 15
b <- rep(NA, times=k) # crea la secuencia vacia de beta1 para k iteraciones
b[1] <- 2 # valor inicial
for (i in 2:k) {
  b[i] <- b[i-1] + sum((y-x^b[i-1])*x^b[i-1]*log(x)) / sum((x^b[i-1]*log(x))^2)
}

b # Para ver la secuencia con los estimadores de beta1
plot(b, type='b') # Para dibujar la secuencia de los estimadores

# Vamos a estimar beta1 usando la funcion nls 

# Construyendo el dataframe con los datos
datos <- data.frame(x, y)
head(datos)

modelo <- nls(y~x^b, start=c(b=2), algorithm='default',
              control=nls.control(maxiter=50), data=datos)
summary(modelo)

# Comparemos los resultados para este ejemplo

# Usando el metodo manual
b[k]

# Usando nls
coef(modelo)






# Ejemplo 6 ---------------------------------------------------------------

# Vamos a fijar nosotros los parametros que vamos a estimar
beta1 <- 2.5
beta2 <- 3

# Para simular los datos

n <- 100 # Numero de valores a simular
x1 <- seq(from=0.01, to=1, by=0.01) # valores de x1
x2 <- log(x1) # valores de x2, transformando x1
set.seed(467382) # para fijar la semilla
e <- rnorm(n=n, mean=0, sd=1) #200 valores de una normal(0,1)
y <- exp(beta1*x1 + beta2*x2) + e # genera los valores y

# Para graficar los datos simulados
library(scatterplot3d)
scatterplot3d(x=x1, y=x2, z=y, pch=16, cex.lab=1,
              highlight.3d=TRUE, type="h", 
              xlab='X1',
              ylab='X2', 
              zlab='Y')

# Vamos a estimar beta1 Aplicando Gauss-Newton
# https://en.wikipedia.org/wiki/Gauss%E2%80%93Newton_algorithm

k <- 18
# crea la secuencia vacia de beta1 para k iteraciones
b <- array(NA, dim=c(2, 1, k))

# Vamos a definir los valores iniciales de beta1 y beta2
beta1_inicial <- 10
beta2_inicial <- 20
b[, , 1] <- c(beta1_inicial, beta2_inicial) # Valores iniciales

for (i in 2:k) {
  b[, , i] <- b[,,i-1] + solve(matrix(c(sum(exp(2*b[1,1,i-1]*x1+2*b[2,1,i-1]*x2)*x1^2),sum(exp(2*b[1,1,i-1]*x1+2*b[2,1,i-1]*x2)*x1*x2),
sum(exp(2*b[1,1,i-1]*x1+2*b[2,1,i-1]*x2)*x1*x2),sum(exp(2*b[1,1,i-1]*x1+2*b[2,1,i-1]*x2)*x2^2)),nrow=2,ncol=2))%*%+##el +  indica que sigue la linea misma linea abajo
matrix(c(sum((exp(b[1,1,i-1]*x1+b[2,1,i-1]*x2))*x1*(y-exp(b[1,1,i-1]*x1+b[2,1,i-1]*x2))),
sum((exp(b[1,1,i-1]*x1+b[2,1,i-1]*x2))*x2*(y-exp(b[1,1,i-1]*x1+b[2,1,i-1]*x2)))),2,1)
}

# Para ver la secuencia de beta1 y beta2 estimados
b[1, , ]
b[2, , ]

par(mfrow=c(2,1))
plot(b[1, , ], type='b', main='Trayectoria de beta1',
     ylab='Beta1',xlab='Iteración', las=1)
plot(b[2, , ], type='b', main='Trayectoria de beta2',
     ylab='Beta2',xlab='Iteración', las=1)


# Vamos a estimar beta1 usando la funcion nls 

# Construyendo el dataframe con los datos
datos <- data.frame(x1, x2, y)
head(datos)

modelo <- nls(y ~ exp(b1*x1+b2*x2), data=datos,
              start=c(b1=5, b2=5),
              control=nls.control(maxiter=50),
              algorithm="default")
summary(modelo)

# Comparemos los resultados para este ejemplo

# Usando el metodo manual
b[1, , k]
b[2, , k]

# Usando nls
coef(modelo)

