library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "My Shiny Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Iris", tabName = "iris", icon = icon("tree")),
      menuItem("Cars", tabName = "cars", icon = icon("car"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("iris",
              box(plotOutput("Correlation_Plot"), width = 8),
              box(selectInput("features", "Features:",
                              c("Petal.Length", "Sepal.Width", "Petal.Width")), width = 4)
              )
      ),
      tabItem("cars",
              fluidPage(
                h1("Cars")
                )
      )      
    )
  )


server <- function(input, output) {
  output$Correlation_Plot <- renderPlot({
    plot(iris$Sepal.Length, iris[[input$features]],
         xlab = "Sepal Lenght", ylab = "Feature")
  })
  
}

shinyApp(ui, server)
