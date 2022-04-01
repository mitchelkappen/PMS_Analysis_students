library(shiny)

ui<- fluidPage( # makes the User Interface

  sliderInput(inputId= "num", label="choose a number", value=25, min=1, max=100),
  plotOutput(outputId="hist")
)

server <- function (input, output){ #we have to add this to the server
  output$hist<- renderPlot({ # here we say what we want to do with output$hist and make it with render
    title <- "100 random normal values" #you can add as many code as you want between these braces
    hist(rnorm(input$num))
    })
}

#use server function to assemple inputs into outputs. 3 rules:
# -save output that you build to output$
# - build the output with a render*() function()
# -access input values with input$
shinyApp(ui=ui, server=server)

##you MUST use the name app.R if you want to share it on the web. 
#shinyapps.io free website that allows you to run your app on a distant server



shinyApp(ui=ui, server=server)

