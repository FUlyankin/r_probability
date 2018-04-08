

library('ggplot2')

# Упражнение 1
# Монте- Карло для интеграла.
n <- 10000
f <- function(x) x^2

x <- seq(0,1,length.out = n)
y <- f(x)

x_genr <- runif(n)
y_genr <- runif(n)

df <- data.frame('x' = x, 'y' = y, 'x_obstrel' = x_genr, 'y_obstrel' = y_genr)

ggplot(df) + 
  geom_point(aes(x = x_obstrel, y = y_obstrel),color = 'blue') +
  geom_line(aes(x = x, y = y),color = 'white') +
  ggtitle('Графический график')+
  xlab('ось для иксов') + ylab('Ось для игрэков')
  

g <- function(x,y) ifelse(y <= x^2,1,0)

z <- g(df$x_obstrel,df$y_obstrel)

dff <- data.frame('x' = x, 'y' = y, 'x_obstrel_up' = x_genr*z, 'y_obstrel_up' = y_genr*z,
                  'x_obstrel_down' = x_genr*(1-z), 'y_obstrel_down' = y_genr*(1-z))


ggplot(dff) + 
  geom_point(aes(x = x_obstrel_up, y = y_obstrel_up),color = 'blue') +
  geom_point(aes(x = x_obstrel_down, y = y_obstrel_down),color = 'green') +
  geom_line(aes(x = x, y = y),color = 'white') +
  ggtitle('Графический график')+
  xlab('ось для иксов') + ylab('Ось для игрэков')

# Итоговая площадь: 
sum(z)/n


# Погрешность измерения. А сколько примерно точек надо кинуть, чтобы получить хороший результат? 
# Не забываем, что оптимальное количество шагов случайное! 
ks <- 1:4
sq_finder <- function(n) {
  f <- function(x,y) y <= x^2
  z <- f(runif(n), runif(n))
  sum(z[z]) / n
}

expost <- sq_finder(49)
exante <- sq_finder(50)
k <- 50
while(abs(expost - exante) > 0.0001){
  k <- k +1 
  exante <- expost
  expost <- sq_finder(k)
}
expost
exante
k

# Интересно было бы посмотреть на распределение оптимального количества шагов. 
optim_steps <- c( )
for(i in 1:1000){
  expost <- sq_finder(49)
  exante <- sq_finder(50)
  k <- 50
  while(abs(expost - exante) > 0.0001){
    k <- k +1 
    exante <- expost
    expost <- sq_finder(k)
  }
optim_steps[i] <- k
}

# Это распределение явно ненормальное. Более того, оно довольно сложно устроено.
# Было бы интересно попытаться вычислить его. 
qplot(optim_steps)

# Если поставить количество шагов в районе 4000, то ошибка будет с очень низкой вероятностью.
sum(optim_steps>4000)  # Таковых всего два наблюдения. 













# Упражнение 2 
# Попытка намонтекарлить проверку гипотезы о значимости коэффициента в парной регрессии

tst <- rep(0, 1000)

for(i in 1:1000){
  eps <- rnorm(1000,mean=0,sd=1)
  x <- runif(1000,-100,100)
  y <- 10 + 0*x + eps
  
  model <- lm(y~x)
  sm <- summary(model)
  tst[i] <- sm$coefficients[,3][2]
}

tst
qplot(tst)

quantile(tst,c(0.01,0.025,0.05,0.1,0.9,0.95,0.975,0.99))


# Сценарий номер два. Одна и та же выборка, разные коэфициенты. Гипотеза о пятёрке.
df <- cars
model <- lm(data = df,'speed~dist')
summary(model)
tst <- rep(0,1000) 

for(i in 1:1000){
  eps = rnorm(length(df$dist),mean=0,sd=1)
  y = 5*df$dist + eps
  model <- lm(y~df$dist)
  sm <- summary(model)
  tst[i] <- (sm$coefficients[,1][2]-5)/sm$coefficients[,2][2]
} 

qplot(tst)
quantile(tst,c(0.01,0.025,0.05,0.1,0.9,0.95,0.975,0.99))




# Упражнение 3 
# Гипотеза о равенстве среднего чему-то там. 
# Что ставить в вычитаемое? Конкретное число или рандомное? 

for(i in 1:1000){
  x <- rnorm(1000,51,16)
  for_h0 <- 51
  t_sample <- (mean(x)-for_h0)/(sd(x))*sqrt(1000)
}

qplot(t_sample)




# Упражнение 4 Единичный корень

library('tseries')
library('forecast')

N = 10000
DFtst <- rep(0,N)
for(j in 1:N){
  eps <- rnorm(1000,0,9)
  y <- rep(0,1000)
  for(i in 2:1000){
    y[i] <- y[i-1] + eps[i]
  }
  y_lag <- y[1:(length(y)-1)]
  y <- y[-1]
  
  model <- lm(y~y_lag)
  sm <- summary(model)
  DFtst[j] <- (sm$coefficients[,1][2]-1)/sm$coefficients[,2][2]
}
DFtst

qplot(DFtst)
quantile(DFtst,c(0.01,0.025,0.05,0.1,0.9,0.95,0.975,0.99))



# Статистика для проверки моды или медианы.
# Куда ставить сиды?
# Доверительный интервал







# Часть 2. Бутстрапирование. 

# Бутстрап для линейной регрессии
df <- cars
N = 10000
tst <- rep(0,N)

base_model <- lm(data = df,'speed~dist')
base_residuals <- base_model$residuals
base_forecasts <- base_model$fitted.values

for(i in 1:N){
  shuffle_reseduals <- sample(1:length(base_residuals),length(base_residuals))
  
  # Как правильно делать эту часть?!
  h0_coef <- 5   # Сюда подставляю коэфициент, для которого проверяется гипотеза. 
  y_star <- base_model$coefficients[1] + h0_coef*df$dist + shuffle_reseduals
  # y_star <- base_forecasts + shuffle_reseduals
  
  butstrap_model <- lm(y_star~df$dist)
  sm <- summary(butstrap_model)
  tst[i] <- (sm$coefficients[,1][2]-h0_coef)/sm$coefficients[,2][2]
}

qplot(tst)
quantile(tst,c(0.01,0.025,0.05,0.1,0.9,0.95,0.975,0.99))
# Когда надо центрировать???


# Бутстрап для проверки гипотезы о равенстве средних - ? 
# Выборка, удаляем по 1 элементу случайно => много выборок. Это бутстрап?
# 





# Бутстрап для линейной регрессии
df <- cars
N = 1000
tst <- rep(0,N)

base_model <- lm(data = df,'speed~dist')
base_residuals <- base_model$residuals

for(i in 1:N){
  shuffle_reseduals <- sample(1:length(base_residuals),length(base_residuals))
  
  # Как правильно делать эту часть?!
  h0_coef <- 5   # Сюда подставляю коэфициент, для которого проверяется гипотеза. 
  y_star <- base_model$coefficients[1] + h0_coef*df$dist + shuffle_reseduals
  # y_star <- base_forecasts + shuffle_reseduals
  
  butstrap_model <- lm(y_star~df$dist)
  sm <- summary(butstrap_model)
  tst[i] <- (sm$coefficients[,1][2]-h0_coef)/sm$coefficients[,2][2]
}

qplot(tst)
quantile(tst,c(0.01,0.025,0.05,0.1,0.9,0.95,0.975,0.99))
# Когда надо центрировать???
























