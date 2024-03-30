# COVID-19 Project App Page
source("plot_1.R")
source("plot_2.R")

library(shinythemes)
library(shiny)

panel_1 <- tabPanel("Plot with Params", value = "plot1", my_ui_plot_1)
panel_2 <- tabPanel("Age Group", value = "plot2", my_ui_plot_2)

my_ui <- navbarPage(
  theme = shinytheme("united"),
  "COVID-19 Model",
  panel_1,
  panel_2
)

my_server <- function(input, output) {
  output$plot1 <- my_server_plot_1(input, output)
  output$plot2 <- my_server_plot_2(input, output)
}

shinyApp(ui = my_ui, server = my_server)
