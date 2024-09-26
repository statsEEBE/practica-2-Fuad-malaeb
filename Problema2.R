#Codigo para problema 2
#por ley
iris
mis_dades<-iris

#vamos a hacer una regresion lineal por minimos cuadrados

y <- mis_dades$Sepal.Length
y

x <- mis_dades$Petal.Length
x

plot(x,y)

xbar <- mean(x) #esto es para calcular la media
xbar
ybar <- mean(y)
ybar
m <-(sum((x-xbar)*(y-ybar)))/sum((x-xbar)^2)
m
b <- ybar- m*xbar
b

m*1.5+b

mod <-lm(y~x)
#es lineal modelque sirbe para calcular mas rapidamente m y b

ypredicted <- predict(mod, data.frame(x=x))
# data frame es una vase de datos con una sola columna donde se define todas las x
ypredicted
plot(x,y)
lines(x, ypredicted)
rsq <- sum((ypredicted-ybar)^2)/sum((y-ybar)^2)
rsq

summary(mod)#para que me saque mas info
# el error es el del multiole R-squared
sqrt(rsq)
cor.test(x,y)
