library(shiny)

ui <- fluidPage(
  titlePanel('Hello World!')
)

server <- function(input, output, session) {
  
}

shinyApp(ui = ui, server = server)