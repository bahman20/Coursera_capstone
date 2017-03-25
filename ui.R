library(shiny)

fluidPage(
  
  
  mainPanel(
    h2("Instructions:"),
    h5("This is a Shiny app that takes a string from the user and predicts the next word using an n-gram analysis with Kneser-Ney smoothing. Please enter your string and click on the Enter botton."),
    textInput("inpstr",label=h4("Type your word(s) here:")),
    submitButton('Enter'),
    h4('Next word prediction is:'),
    verbatimTextOutput("pred")
    
  )
)