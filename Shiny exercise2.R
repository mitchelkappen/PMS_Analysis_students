library(shiny)

ui<- fluidPage(
  sliderInput(inputId= "num", label="choose a number", value=25, min=1, max=100),
  plotOutput(outputId="hist")
)

server <- function (input, output){
  output$hist<- renderPlot({hist(rnorm(100))})
}

shinyApp(ui=ui, server=server)