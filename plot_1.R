# COVID-19 Project Plot
# Load libraries
library(shiny)
library(dplyr)
library(plotly)
library(reticulate)
library(shinythemes)

use_python("/usr/local/bin/python")
# Uncomment if you haven't installed this before
py_install(c("numpy", "scipy", "matplotlib"))

my_ui_plot_1 <- fluidPage(
    plotOutput("plot", width = "80%", height = "300px"),
    hr(),
    
    fluidRow(
      column(4,
        helpText("Change the parameters and see how those factors 
                 influence the spread of COVID-19."),
        numericInput("N", label = h5("Total population"), value = 1000),
        numericInput("days", label = h5("Total days"), value = 200),
        numericInput("alpha", label = h5("Natural Birth/Death Rate"), value = 0.02),
        sliderInput("beta", label = h5("Transmission Rate"), min = 0, max= 1, value = 0.58)
      ),
      column(4,
        sliderInput("incubation", label = h5("Average Days until Infectious"), min = 0, max= 30, value = 13),
        sliderInput("m", label = h5("Mortality Rate of infected people dying"), min = 0, max = 1, value = 0.15),
        sliderInput("r", label = h5("Average Days until Recovered"), min = 0, max= 50, value = 20),
        sliderInput("d", label = h5("Average Days until Death"), min = 0, max = 50, value = 10)
      ),
      column(4,
        sliderInput("v", label = h5("Total Percentage of vaccinated people"), min = 0, max= 1, value = 0.7),
        sliderInput("p", label = h5("Average Days until Qurantine"), min = 0, max = 10, value = 7),
        sliderInput("e", label = h5("Percentage of Exposed who choose to Quarantine"), min = 0, max = 1, value = 0.2),
        sliderInput("sigma", label = h5("Vaccine Inefficacy"), value = 0.05, min = 0, max = 1)
      )
    )
)

my_server_plot_1 <- function(input, output) {
  # ------------------ App virtualenv setup  ------------------- #
  
  virtualenv_dir = Sys.getenv('VIRTUALENV_NAME')
  python_path = Sys.getenv('PYTHON_PATH')
  
  # Create virtual env and install dependencies
  #reticulate::virtualenv_create(envname = virtualenv_dir, python = python_path)
  #reticulate::virtualenv_install(virtualenv_dir, packages = c("numpy"))
  #reticulate::use_virtualenv(virtualenv_dir, required = T)
  
  output$plot <- renderPlot({
    # Import python functions to R
    reticulate::source_python('model.py')
    
    ## Basic parameters
    N = input$N                 # total population number
    alpha = input$alpha         # natural birth and death rate
    total_days = input$days
    beta = input$beta
    sigma = input$sigma                          # vaccine inefficacy
    v = input$v/total_days                       # rate of people getting vaccinated per day
    incubation = 1.0/input$incubation            # incubation rate
    mortality_rate = input$m                     # mortality rate of infected people dying
    recovery_days = input$r                      # average length of infection (recovered)
    recovered = (1.0/recovery_days) * (1 - mortality_rate)           # recovered rate
    death_days = input$d                         # average length of infection (death)
    death = (1.0/death_days) * mortality_rate    # death rate
    p_i = 1.0/input$p                            # infectious quarantine rate
    p_e = input$e/input$p                        # exposed quarantine rate
    
    params <- py$result(N, total_days, alpha, beta, incubation, recovered, death, v, p_i, p_e, sigma)
    
    S <- unlist(params[1])
    V <- unlist(params[2])
    E <- unlist(params[3])
    I <- unlist(params[4])
    Q <- unlist(params[5])
    R <- unlist(params[6])
    D <- N - S - V - E - I - Q - R
    t <- c(1:input$days)
    
    
    plot(t, S, type = "l", col = 'red', xlab = "Time", ylab = "Num",
         xlim=c(0, as.integer(input$days)), ylim=c(0, as.integer(input$N)))
    lines(t, V, type = "l", col = 'purple')
    lines(t, E, type = "l", col = 'green')
    lines(t, I, type = "l", col = 'blue')
    lines(t, Q, type = "l", col = 'yellow')
    lines(t, R, type = "l", col = 'orange')
    lines(t, D, type = "l", col = 'black')
    legend("topleft", c("Suceptible", "Vaccined", "Exposed", "Infectious",
                         "Quarantined", "Recovered", "Death"), 
           fill = c("red", "purple", "green", "blue", "yellow", "orange", "black"))
  })
}

shinyApp(ui = my_ui_plot_1, server = my_server_plot_1)
  