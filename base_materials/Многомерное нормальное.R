require(ggplot2)
require(mvtnorm)
require(rgl)


mu<-c(-1,0,2)
bro<-matrix(c(1,0,0,0,4,4,0,4,9),nrow=3)

eigen(bro) #����������� ����� � ����������� �������
eigen(bro)$values[2]
eigen(bro)$vectors[,2]

#����������� ��������� ������� ������� 1000 ��� ����� ����������� ��������� ��������.
x<-rmvnorm(10^3,mean=mu,bro)
head(x,10)

qplot(x[,3])
qplot(x[,1],x[,2]) #��� ����������, ������� ������ �������
mean(x[,1])
qplot(x[,2],x[,3]) #������ �������� ��-�� ������� ����������
cov(x[,2],x[,3])
cov(x[,1],x[,2])

#����� �(x1-x2>x3) ����� ���������. ������� ��������� � �, ��������, �����������.
#����������������� ������:
appo <- x[,1]-x[,2]>x[,3]
head(appo,10)
sum(appo)/10^3

#������� �����������:
1-pnorm(0,mean=-3,sd=sqrt(22))

#������ �������� ��������. (��� ������� ��� �� �����������, ���� ��������� ����� ����?)
#��������������, ���������������. �� ��������

#������ ������

x1 <- seq(-2,2,by=0.05) # ����� ��� ����������
x2 <- seq(-2,2,by=0.05) # �������

alex <- expand.grid(privet=x1,poka=x2) #�������� ������� ��� ���������� �������
head(alex)

mu.a <- c (1,0)
cov.a <- matrix(c(4,5,5,9),ncol=2)

alex$z <- dmvnorm(alex,mu.a,cov.a)  #� ������� ��������� ����� ������� ��������

plot3d(alex$privet,alex$poka,alex$z)

#��������� �� ����� ������� ����������� � ������� ��������� ��� ����������� �������������.

qplot(x[,1])
qplot(x[,1],geom="blank")+geom_histogram(aes(y=..density..))
#�������� �� �� ����� �����������, ��� ������� ����� 1, �.�. ������� ����������

qplot(x[,1],geom="blank")+geom_histogram(aes(y=..density..),alpha=0.3)
#����� ��� ������� ������������. ����� �� 0 �� 1.

ros <- function(t){
  return(dnorm(t,mean=-1,sd=1))
} #���-�� �� ���...



qplot(x[,1],geom="blank")+
  geom_histogram(aes(y=..density..),alpha=0.3)+
  stat_function(fun=ros))










  library("mvtnorm") # для многомерного нормального
  library("ggplot2") # для графиков

  n_obs <- 200
  corr <- 0.9

  mu <- c(10, 15)
  # ковариационная матрица
  #   4   corr * 2 * 3
  #   ?   9
  A <- matrix(
    c(4, corr*6, corr*6, 9),
    nrow = 2)

  X <- rmvnorm(n_obs, mean = mu, sigma = A)
  # goo.gl/Lxd4yL

  qplot(X[, 2])
  qplot(x = X[, 1], y = X[, 2])




  library("shiny")
  shinyUI(pageWithSidebar(
    headerPanel("Многомерное нормальное и Винни-Пух:"),
    sidebarPanel(
      sliderInput("n_obs", "Число точек",
                  min = 50, max = 500, step = 10, value = 100,
                  animate = TRUE),
      sliderInput("corr", "Корреляция",
                  min = -1, max = 1, step = 0.1, value = 0.5,
                  animate = TRUE)
    ),
    mainPanel(
      plotOutput("histogram"),
      plotOutput("scatter")
    )
  ))






  library("ggplot2")
  library("mvtnorm")


  shinyServer( function(input, output) {
    update_x <- reactive({
      mu <- c(10, 15)
      A <- matrix(c(4, input$corr * 6,
                    input$corr * 6, 9),
                  nrow = 2)
      X <- rmvnorm(input$n_obs, mean = mu,
                   sigma = A)
      X
    })
    output$histogram <- renderPlot({
      X <- update_x()
      qplot(X[, 1])
    })
    output$scatter <- renderPlot({
      X <- update_x()
      qplot(x = X[, 1], y = X[, 2])
    })
  })




  # n_obs <- 100
  # corr <- 0.5
