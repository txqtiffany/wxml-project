# Age Structure Plot

# Load libraries
library(shiny)
library(dplyr)
library(plotly)
library(reticulate)
library(shinythemes)
library(gridExtra)

use_python("/usr/local/bin/python")
# Uncomment the following if you haven't installed them yet
py_install("numpy")
py_install("pandas")

my_ui_plot_2 <- fluidPage(
  titlePanel("Age Group Distribution"),
  hr(),
  sidebarLayout(
    sidebarPanel(
      helpText("Choose a particular age group to see its spread of COVID-19 using real population data of King County using Age-structured SEIRD Model."),
      
      # Choose which curves to display
      checkboxGroupInput("curves", h5("Show curves"), 
                         choices = list("Suspected" = 1, 
                                        "Exposed" = 2, 
                                        "Infected" = 3,
                                        "Recovered" = 4,
                                        "Death" = 5),
                         selected = c(3, 5)
      ),
      
      # Choose which age group to display
      sliderInput("age", label = h4("Age"), min = 0, 
                  max = 85, value = 30, step = 1),
      
      # Choose which graphs to compare with
      checkboxGroupInput("compare", h5("Compare it with"), 
                         choices = list("Another age group" = 1, 
                                        "Non-age structured model" = 2),
                         selected = NULL
      ),
      
      # If comparing with another age group, display second sliderbar
      conditionalPanel(
        condition = "input.compare.includes('1')",
        sliderInput("age2", label = h4("Age"), min = 0, 
                    max = 85, value = 30, step = 1)
      ),
      
      # Ask which plot the user want to see
      selectInput("graph", label = h5("Select which plot to display"),
                  choices = list("Total Population" = 1, "Log of Population" = 2, 
                                 "Percentage Population" = 3),
                  selected = 1),
      width = 3
    ),
    
    # Display plots
    mainPanel(fluidRow(
      verticalLayout(h4("COVID-19 under Age Structure Model"),
                     plotOutput("plot1"), 
                     h4("Comparison between Age Structure Model and spread of COVID-19 in King County"),
                     plotOutput("plot2")))
    )
  )
)

my_server_plot_2 <- function(input, output) {
  # ------------------ App virtualenv setup  ------------------- #
  
  virtualenv_dir = Sys.getenv('VIRTUALENV_NAME')
  python_path = Sys.getenv('PYTHON_PATH')
  
  # Create virtual env and install dependencies
  #reticulate::virtualenv_create(envname = virtualenv_dir, python = python_path)
  #reticulate::virtualenv_install(virtualenv_dir, packages = c("numpy"))
  #reticulate::use_virtualenv(virtualenv_dir, required = T)
  
  # Transfer age input into age group index
  selection <- function(age) {
    if (age <=12 & age >= 10) {
      selection <- 3
    } else if (age <= 14 & age >= 13 ){
      selection <- 4
    } else if (age <= 17 & age >= 15 ){
      selection <- 5
    } else if (age <= 19 & age >= 18 ){
      selection <- 6
    } else if (age <= 21 & age >= 20 ){
      selection <- 7
    } else if (age <= 24 & age >=22 ) {
      selection <- 8
    } else if (age >= 75 ){
      selection <- 19
    } else if (age <= 9 ) {
      selection <- (floor(age / 5) + 1)
    } else {
      selection <- (floor(age / 5) + 4)
    }
    return(unlist(selection))        
  }
  
  # A function, age as input, SEIRD parameters as output. 
  paramasfunc <- function(age) {
    # Set up the numbers
    params <- py$result(as.integer(selection(age)-1)) # put in -1 because index in Python start with 0 instead of 1
    S <- unlist(params[1])
    E <- unlist(params[2])
    I <- unlist(params[3])
    R <- unlist(params[4])
    D <- unlist(params[5])
    confirmed <- I + R + D
    
    if (input$graph == 2) {
      S <- log(S)
      E <- log(E)
      I <- log(I)
      R <- log(R)
      D <- log(D)
      confirmed <- log(confirmed)
    } else if (input$graph == 3) {
      total = S[1] + E[1] + I[1] + R[1] + D[1]
      S <- S/total * 100
      E <- E/total * 100
      I <- I/total * 100
      R <- R/total * 100
      D <- D/total * 100
      confirmed <- confirmed/total * 100
    }
    return(list(S, E, I, R, D, confirmed))
  }
  
  output$plot1 <- renderPlot({
    # Import python functions to R
    reticulate::source_python('model2.py')
    curves <- input$curves
    
    # Create an empty plot at first
    t <- c(1:200)
    plot.new()
    if (input$graph == 1) {
      plot(0, type = "n", xlab = "Time", ylab = "Total Number in each category", 
           xlim=c(0, 200), ylim = c(0, 200000))
    } else if (input$graph == 2) {
      plot(0, type = "n", xlab = "Time", ylab = "Log of Number in each category", 
           xlim=c(0, 200), ylim = c(-10, log(200000)))
    } else if (input$graph == 3) {
      plot(0, type = "n", xlab = "Time", ylab = "Percentage of numbers in each category",
           xlim=c(0, 200), ylim = c(0, 100))
    }
    
    params1 <- paramasfunc(input$age) 
    S <- unlist(params1[1])
    E <- unlist(params1[2])
    I <- unlist(params1[3])
    R <- unlist(params1[4])
    D <- unlist(params1[5])
    
    # Plot the first set of curves
    if (is.element(1, curves)) {
      lines(t, S, type = "l", col = 'blue')
    }
    if (is.element(2, curves)) {
      lines(t, E, type = "l", col = 'orange')
    }
    if (is.element(3, curves)) {
      lines(t, I, type = "l", col = 'red')
    }
    if (is.element(4, curves)) {
      lines(t, R, type = "l", col = 'springgreen4')
    }
    if (is.element(5, curves)) {
      lines(t, D, type = "l", col = 'black')
    }
    legend("topright", c("Susceptible", "Exposed", "Infected", "Recovered", "Death"), 
           fill = c("blue", "orange", "red", "springgreen4", "black"), cex = 1.2)
    
    # If chosen, plot the second set of curves
    if (is.element(1, input$compare)) {
      params2 <- paramasfunc(input$age2) 
      S2 <- unlist(params2[1])
      E2 <- unlist(params2[2])
      I2 <- unlist(params2[3])
      R2 <- unlist(params2[4])
      D2 <- unlist(params2[5])

      if (is.element(1, curves)) {
        lines(t, S2, type = "l", col = 'deepskyblue1')
      }
      if (is.element(2, curves)) {
        lines(t, E2, type = "l", col = 'gold')
      }
      if (is.element(3, curves)) {
        lines(t, I2, type = "l", col = 'tomato')
      }
      if (is.element(4, curves)) {
        lines(t, R2, type = "l", col = 'seagreen2')
      }
      if (is.element(5, curves)) {
        lines(t, D2, type = "l", col = 'gray48')
      }
      legend("topright", c("Susceptible", "Exposed", "Infected", "Recovered", "Death","Susceptible", "Exposed", "Infected", "Recovered", "Death"),
             fill = c("blue", "orange", "red", "springgreen4", "black", "deepskyblue1", "gold", "tomato", "seagreen2", "gray48"), cex = 1.2, ncol = 2)
    }
    
    ## Compare with Non-age Structure
    reticulate::source_python('model.py')
    if(is.element(2, input$compare)){
      ## Basic parameters
      pop_dist <- read.csv("pop_dist_1.csv", header = T)
      N <- as.integer(as.vector(pop_dist[[2]] + pop_dist[[3]]))  # population number categorized in age groups
      i <- 13.112221321200105          # average length of incubation period
      sigma <- 1.0/i      # incubation rate
      dinfe <- as.integer(14)           # average length of infection (recovered)
      gamma <- 1.0/dinfe      # recovery rate
      m <- 0.00053           # infected death rate
      death <- as.integer(7)            # average length of infection (dead)
      p <- 1.0/death          # mortality rate
      
      ## Lockdown measures
      ## WA Shelter In-Place issued 3/23/2020
      ## 1st case in King County: 1/22/2020
      ## 2nd case in King County: 2/29/2020
      lock_day <- as.integer(62)      # day of lockdown measure
      
      ## Age groups
      prop_age <- N/sum(N)      # proportions of age groups in total population
      m_age <- c(m, m, m, m, m, m, m, m, m, m, m, m, m, m, m, m, m, m, m) # non-age structured
      beta_start <- 0.7
      beta_end <- 0.05
      k <- 0.18
      
      params3 <- py$result(N[as.integer(selection(input$age))], i, sigma, 
                           dinfe, gamma, m, death,
                           p, lock_day, as.integer(200), prop_age, m_age,
                           beta_start, beta_end, 0.18)
      
      S3 <- unlist(params3[1])
      E3 <- unlist(params3[2])
      I3 <- unlist(params3[3])
      R3 <- unlist(params3[4])
      D3 <- unlist(params3[5])
      
      if (input$graph == 2) {
        S3 <- log(S3)
        E3 <- log(E3)
        I3 <- log(I3)
        R3 <- log(R3)
        D3 <- log(D3)
      } else if (input$graph == 3) {
        total = S3[1] + E3[1] + I3[1] + R3[1] + D3[1]
        S3 <- S3/total * 100
        E3 <- E3/total * 100
        I3 <- I3/total * 100
        R3 <- R3/total * 100
        D3 <- D3/total * 100
      }
      
      # Plot the Curves
      if (is.element(1, curves)) {
        lines(t, S3, type = "l", col = 'deepskyblue1')
      }
      if (is.element(2, curves)) {
        lines(t, E3, type = "l", col = 'gold')
      }
      if (is.element(3, curves)) {
        lines(t, I3, type = "l", col = 'tomato')
      }
      if (is.element(4, curves)) {
        lines(t, R3, type = "l", col = 'seagreen2')
      }
      if (is.element(5, curves)) {
        lines(t, D3, type = "l", col = 'gray48')
      }
      
      legend("topright", c("Susceptible", "Exposed", "Infected", "Recovered", "Death","Susceptible", "Exposed", "Infected", "Recovered", "Death"),
             fill = c("blue", "orange", "red", "springgreen4", "black", "deepskyblue1", "gold", "tomato", "seagreen2", "gray48"), cex = 1.2, ncol = 2)
    }
  })
  
  output$plot2 <- renderPlot({
    # Import python functions to R
    reticulate::source_python('model2.py')
    curves <- input$curves
    
    # Create an empty plot at first
    t <- c(1:200)
    plot.new()
    
    if (input$graph == 1) {
      plot(0, type = "n", xlab = "Time", ylab = "Total Number in each category", 
           xlim=c(0, 200), ylim = c(0, 200000))
    } else if (input$graph == 2) {
      plot(0, type = "n",  xlab = "Time", ylab = "Log of Number in each category", 
           xlim=c(0, 200), ylim = c(-10, log(200000)))
    } else if (input$graph == 3) {
      plot(0, type = "n", xlab = "Time", ylab = "Percentage of numbers in each category",
           xlim=c(0, 200), ylim = c(0, 100))
    }
    
    params1 <- paramasfunc(input$age) 
    S <- unlist(params1[1])
    E <- unlist(params1[2])
    I <- unlist(params1[3])
    R <- unlist(params1[4])
    D <- unlist(params1[5])
    confirmed <- unlist(params1[6])
    
    # Plot the Curves
    lines(t, confirmed, type = "l", col = 'blue')
    lines(t, D, type = "l", col = "black")
    
    # King County confirmed cases
    confirmedcases = read.csv("cases.csv")
    x1 <- seq(1,144,by=1)
    y1 <- unlist(confirmedcases[3020,])
    y1 <- y1[-1:-4]
    if (input$graph == 2) {
      y1 <- log(y1)
    } else if (input$graph == 3){
      y1 <- y1 / 2253000 * 100
    }
    
    lines(x1, y1, type="l", col='deepskyblue1')
    
    # King County Deaths cases
    deaths = read.csv("deaths.csv")
    x2 <- seq(1,144,by=1)
    y2 <- unlist(deaths[3020,])
    y2 <- y2[-1:-4]
    if (input$graph == 2) {
      y2 <- log(y2)
    } else if (input$graph == 3){
      y2 <- y2 / 2253000 * 100
    }
    
    lines(x2, y2, type="l", col='gray48')
    
    legend("bottomright", c("Confirmed Cases in Age-structure Model", "Death under in Age-structure Model", "Confirmed Cases in King County", "Deaths in King County"), 
           fill = c("blue", "black", "deepskyblue1", "gray48"), cex = 1.2)
  })
}

shinyApp(ui = my_ui_plot_2, server = my_server_plot_2)
