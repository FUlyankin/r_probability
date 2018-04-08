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
