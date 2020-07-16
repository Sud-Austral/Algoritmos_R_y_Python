
library(shiny)

ui <- fluidPage(
  sliderInput(inputId = "num", 
              label = "Elija un numero", 
          
                  value = 25, min = 1, max = 100),

  
  #updateRadioButtons()
  
        # p("The first radio button group controls the second"),
        # radioButtons("inRadioButtons", "Input radio buttons",
        #              c("Item A", "Item B", "Item C")),
        # radioButtons("inRadioButtons2", "Input radio buttons 2",
        #              c("Item A", "Item B", "Item C"))
)

# ?RadioButtons

server <- function(input, output) {}

shinyApp(ui = ui, server = server)