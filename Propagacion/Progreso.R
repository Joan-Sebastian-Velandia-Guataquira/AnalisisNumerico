#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(deSolve)

# Define UI for application that draws a histogram
#-----------
ui <- fluidPage(

   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   sidebarLayout
   (
     
     sidebarPanel
     (
       selectInput("modelo", "Modelo:", choices = c("SIR", "SI", "seleccionar"), selected = "seleccionar" ),
       selectInput("metodologia", "metodologia:", choices = c("EDO", "EDF","seleccionar"), selected = "seleccionar" ),
       
       conditionalPanel
       (
         condition = "input.modelo == 'SI' && input.metodologia == 'EDO'",
         radioButtons(inputId = "metodoSI", "método:", c("Euler", "Runge-kutta4"), selected = "Euler")
       ),
       conditionalPanel
       (
         condition = "input.modelo == 'SIR' && input.metodologia == 'EDO'",
         radioButtons(inputId = "metodoSIR", "método:", c("Euler", "Adams"), selected = "Euler")
       ),
       conditionalPanel
       (
         condition = "input.modelo == 'SI'&& input.metodologia != 'seleccionar'",
         numericInput(inputId = "numS", label = "Numero de Suceptibles iniciales", value = 1-1e-6),
         numericInput(inputId = "numI", label = "Numero de Infectados iniciales", value = 1e-6),
         numericInput(inputId = "Betha", label = "Tasa de transmición e infeccion", value = 1.4247)
         #plotOutput("modelSI")
       ),
       conditionalPanel
       (
         condition = "input.modelo == 'SIR'&& input.metodologia != 'seleccionar'",
         numericInput(inputId = "nums", label = "Numero de Suceptibles iniciales", value = 1-1e-6),
         numericInput(inputId = "numi", label = "Numero de Infectados iniciales", value = 1e-6),
         numericInput(inputId = "numr", label = "Numero de Recuperados iniciales", value = 0),
         numericInput(inputId = "betha", label = "Tasa de transmición e infeccion", value = 1.4247),
         numericInput(inputId = "gamma", label = "Tasa de recuperacion de la infeccion", value = 0.14286)
         #plotOutput("modelSIR")
       ) 
     ),
     
     mainPanel
     (
       plotOutput("model")
     )
   )
)
#-----------

# Define server logic required to draw a histogram
server = function(input, output)
{
  output$model <- renderPlot({
    #estado inicial de los compartimentos
    #parámetros del modelo (coeficientes de las variables)
  
    if(input$modelo == "SI" && input$metodoSI == "Euler" && input$metodologia == "EDO")
    {
      init <- c(S = input$numS, I = input$numI, R = 0)
      N = init[1]+init[2]+init[3]
      param <- c(beta = input$Betha, gamma = 0)
      init = init/N
      N = 1
      
      graficar(init, param, N)
    }
    else if(input$modelo == "SIR" && input$metodoSIR == "Euler" && input$metodologia == "EDO")
    {
      init <- c(S = input$nums, I = input$numi, R = input$numr)
      N = init[1]+init[2]+init[3]
      param <- c(beta = input$betha, gamma = input$gamma)
      init = init/N
      N = 1
      
      graficar2(init, param, N)
    }
    else if(input$modelo == "SI" && input$metodoSI == "Runge-kutta4" && input$metodologia == "EDO")
    {
      init <- c(S = input$numS, I = input$numI, R = 0)
      N = init[1]+init[2]+init[3]
      param <- c(beta = input$Betha, gamma = 0)
      init = init/N
      N = 1
      
      graficar3(init, param, N)
    }
    else if(input$modelo == "SIR" && input$metodoSIR == "Adams" && input$metodologia == "EDO")
    {
      init <- c(S = input$nums, I = input$numi, R = input$numr)
      N = init[1]+init[2]+init[3]
      param <- c(beta = input$betha, gamma = input$gamma)
      init = init/N
      N = 1
      
      graficar4(init, param, N)
    }
  })
}

#SI-EDO-Euler
graficar = function(init, param, N)
{
  
  #crear la función con las ODE
  sir <- function(times, init, param) 
  {
    with(as.list(c(init, param)), 
    {
           
      #ecuaciones diferenciales   
      dS <- beta*-1 * S * I 
      dI <- beta * S * I - gamma * I
      dR <- gamma * I
          
      #resultados de las tasas de cambio    
      return(list(c(dS, dI, dR)))
    })
    
  }
  
  #intervalo de tiempo y resolución
  times <- seq(0, 70, by = 1)
  #resolver el sistema de ecuaciones con función 'ode'
  out <- ode(y = init, times = times, func = sir, parms = param, method = "euler")
  #cambiar out a un data.frame
  out <- as.data.frame(out*N) #aqui puede multiplicar 'out' por N
  #eliminar la variable 'time' en out
  out$time <- NULL
  out$R = NULL
  #mostrar 10 primeros datos
  #head(out, 10)
  #gráfica
  matplot(x = times, y = out, type = "l", xlab = "Tiempo", ylab = "S.I", main = "Modelo SI básico",
          lwd = 1, lty = 1, bty = "l", col = 2:4)
  #añadir leyenda de l�?neas
  legend(40, 0.7, c("Susceptibles", "Infectados"), 
           pch = 1, col = 2:4, bty = "n", cex = 1)
}

#SIR-EDO-Euler
graficar2 = function(init, param, N)
{
  #crear la función con las ODE
  sir <- function(times, init, param) 
  {
    with(as.list(c(init, param)), 
    {
      #ecuaciones diferenciales   
      dS <- beta*-1 * S * I 
      dI <-  beta * S * I - gamma * I
      dR <- gamma * I
      #resultados de las tasas de cambio    
      return(list(c(dS, dI, dR)))
    })
  }
  
  #intervalo de tiempo y resolución
  times <- seq(0, 70, by = 1)
  #resolver el sistema de ecuaciones con función 'ode'
  out <- ode(y = init, times = times, func = sir, parms = param, method = "euler")
  #cambiar out a un data.frame
  out <- as.data.frame(out*N) #aqui puede multiplicar 'out' por N
  #eliminar la variable 'time' en out
  out$time <- NULL
  #mostrar 10 primeros datos
  #head(out, 10)
  #gráfica
  matplot(x = times, y = out, type = "l", xlab = "Tiempo", ylab = "S.I.R", main = "Modelo SIR básico",
            lwd = 1, lty = 1, bty = "l", col = 2:4)
  #añadir leyenda de l�?neas
  legend(40, 0.7, c("Susceptibles", "Infectados", "Recuperados"), 
           pch = 1, col = 2:4, bty = "n", cex = 1)
}

#SI-EDO-Runge-kutta4
graficar3 = function(init, param, N)
{
  #crear la función con las ODE
  sir <- function(times, init, param) 
  {
    with(as.list(c(init, param)), 
         {
           
           #ecuaciones diferenciales   
           dS <- beta*-1 * S * I 
           dI <- beta * S * I - gamma * I
           dR <- gamma * I
           
           #resultados de las tasas de cambio    
           return(list(c(dS, dI, dR)))
         })
    
  }
  
  #intervalo de tiempo y resolución
  times <- seq(0, 70, by = 1)
  #resolver el sistema de ecuaciones con función 'ode'
  out <- ode(y = init, times = times, func = sir, parms = param, method = "rk4")
  #cambiar out a un data.frame
  out <- as.data.frame(out*N) #aqui puede multiplicar 'out' por N
  #eliminar la variable 'time' en out
  out$time <- NULL
  out$R = NULL
  #mostrar 10 primeros datos
  #head(out, 10)
  #gráfica
  matplot(x = times, y = out, type = "l", xlab = "Tiempo", ylab = "S.I", main = "Modelo SI básico",
          lwd = 1, lty = 1, bty = "l", col = 2:4)
  #añadir leyenda de l�?neas
  legend(40, 0.7, c("Susceptibles", "Infectados"), 
         pch = 1, col = 2:4, bty = "n", cex = 1)
}
#SIR-EDO-Adams
graficar4 = function(init, param, N)
{
  #crear la función con las ODE
  sir <- function(times, init, param) 
  {
    with(as.list(c(init, param)), 
         {
           #ecuaciones diferenciales   
           dS <- beta*-1 * S * I 
           dI <-  beta * S * I - gamma * I
           dR <- gamma * I
           #resultados de las tasas de cambio    
           return(list(c(dS, dI, dR)))
         })
  }
  
  #intervalo de tiempo y resolución
  times <- seq(0, 70, by = 1)
  #resolver el sistema de ecuaciones con función 'ode'
  out <- ode(y = init, times = times, func = sir, parms = param, method = "adams")
  #cambiar out a un data.frame
  out <- as.data.frame(out*N) #aqui puede multiplicar 'out' por N
  #eliminar la variable 'time' en out
  out$time <- NULL
  #mostrar 10 primeros datos
  #head(out, 10)
  #gráfica
  matplot(x = times, y = out, type = "l", xlab = "Tiempo", ylab = "S.I.R", main = "Modelo SIR básico",
          lwd = 1, lty = 1, bty = "l", col = 2:4)
  #añadir leyenda de l�?neas
  legend(40, 0.7, c("Susceptibles", "Infectados", "Recuperados"), 
         pch = 1, col = 2:4, bty = "n", cex = 1)
}

# Run the application 
shinyApp(ui = ui, server = server)

