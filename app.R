library(shiny)

ui<- fluidPage( # makes the User Interface
  selectInput("formula_choice", label="Which Formula:",
              choices= c('PSS ~ TestMoment', 'BSRI ~ TestMoment', 'PSS~PMSScoreNew*TestMoment'),
              selected='PSS ~ TestMoment'), # puts the choices in the first element which is accessed via input$formula_choice
  plotOutput(outputId="vis")
)

server <- function (input, output){ 
  output$vis<- renderPrint({ 
    title <- "The chosen formula" 
    print(input$formula_choice)
    })
}


shinyApp(ui=ui, server=server)

