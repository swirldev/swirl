library(shiny)

shinyServer(function(input, output) {
  
  # When submit button is pressed, it's value increments to 1
  observe({
    if(input$addit == 0) {
      return()
    }
    
    # Set up return values
    return_vals <- switch(input$class,
                          
                      "text" = list(
                          Output = input$text_output),
                      
                      "cmd_question" = list(
                          Output = input$cmd_output,
                          CorrectAnswer = input$cmd_correct_answer,
                          AnswerTests = input$cmd_answer_tests,
                          Hint = input$cmd_hint),
                      
                      "mult_question" = list(
                          Output = input$mult_output,
                          AnswerChoices = input$mult_answer_choices,
                          CorrectAnswer = input$mult_correct_answer,
                          Hint = input$mult_hint),
                      
                      "exact_question" = list(
                          Output = input$num_output,
                          CorrectAnswer = input$num_correct_answer,
                          Hint = input$num_hint),
                      
                      "video" = list(
                          Output = input$video_output,
                          VideoLink = input$video_link),
                      
                      "figure" = list(
                          Output = input$fig_output,
                          Figure = input$figure,
                          FigureType = input$figure_type)
    )
  
    return_vals <- c(Class = input$class, return_vals)
    
    # Return only non-empty values
    stopApp(return_vals)
  })
  
  observe({
    if(input$done == 0) {
      return()
    }
    # Quit and return "done" to break loop
    stopApp(NULL)
  })
})
