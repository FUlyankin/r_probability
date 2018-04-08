require(ggplot2)
require(mvtnorm)
require(rgl)


mu<-c(-1,0,2)
bro<-matrix(c(1,0,0,0,4,4,0,4,9),nrow=3)

eigen(bro) #собственные числа и собственные вектора
eigen(bro)$values[2]
eigen(bro)$vectors[,2]

#Сгенерируем случайную выборку размера 1000 для нашей многомерной случайной величины.
x<-rmvnorm(10^3,mean=mu,bro)
head(x,10)

qplot(x[,3])
qplot(x[,1],x[,2]) #нет ковариации, поэтому облако аморфно
mean(x[,1])
qplot(x[,2],x[,3]) #облако вытянуто из-за наличия ковариации
cov(x[,2],x[,3])
cov(x[,1],x[,2])

#Найти Р(x1-x2>x3) тремя способами. Функция плотности в Р, табличка, эксперимент.
#Экспериментальный способ:
appo <- x[,1]-x[,2]>x[,3]
head(appo,10)
sum(appo)/10^3

#истиная вероятность:
1-pnorm(0,mean=-3,sd=sqrt(22))

#Теперь построим табличку. (Как сделать это на контрольной, если компьютер забыл дома?)
#Стандартизация, масштабирование. см тетрадку

#Строим график

x1 <- seq(-2,2,by=0.05) # Сетка для построения
x2 <- seq(-2,2,by=0.05) # графика

alex <- expand.grid(privet=x1,poka=x2) #создание сеточки для построения графика
head(alex)

mu.a <- c (1,0)
cov.a <- matrix(c(4,5,5,9),ncol=2)

alex$z <- dmvnorm(alex,mu.a,cov.a)  #в алексея добавился новый столбик апликант

plot3d(alex$privet,alex$poka,alex$z)               

#Изобразим на одном графике гистограмму и функцию плотности для одномерного распределения.

qplot(x[,1])
qplot(x[,1],geom="blank")+geom_histogram(aes(y=..density..)) 
#получаем ту же самую гистограмму, где площадь равна 1, т.е. провели нормировку

qplot(x[,1],geom="blank")+geom_histogram(aes(y=..density..),alpha=0.3) 
#альфа это степень прозрачности. Лежит от 0 до 1.

ros <- function(t){
  return(dnorm(t,mean=-1,sd=1)) 
} #что-то не так...



qplot(x[,1],geom="blank")+
  geom_histogram(aes(y=..density..),alpha=0.3)+
  stat_function(fun=ros))
  
  
  
  
  
  
  
  
  
  
  
