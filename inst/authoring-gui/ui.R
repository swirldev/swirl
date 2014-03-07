library(shiny)

shinyUI(bootstrapPage(
    
  # Lesson name will go here
  headerPanel("swirl content authoring tool"),
  
  # Select unit class
  sidebarPanel(
    
    tags$link(
      rel = "stylesheet", 
      href = "http://fonts.googleapis.com/css?family=Source+Sans+Pro"
    ),
    
    tags$style("body { font-family: 'Source Sans Pro', sans-serif; }",
               "h1 { color: #3399ff; }",
               "#addit { color: #3399ff; }",
               "#done { color: red; }",
               "button { font-family: inherit; }",
               "textarea { font-family: inherit; }",
               "select { font-family: inherit; }"
    ),
    
    helpText(tags$ol(tags$li("Select a content type."),
                     tags$li("Complete the form."),
                     tags$li("Press the ", em("Add it!"), " button."),
                     tags$li("Repeat steps 1-3 until complete."),
                     tags$li("Press", em("I'm done!"), " to exit."))),

    br(),
    
    # Select unit class
    selectInput("class", "Content type:",
                choices = c("Text" = "text", 
                            "Question - R Command" = "cmd_question",
                            "Question - Multiple Choice" = "mult_question",
                            "Question - Exact Numerical" = "exact_question",
                            "Video" = "video", 
                            "Figure" = "figure")
                )
    ),
  
  # Display appropriate form based on unit class
  mainPanel(
    
    # Output current unit class selected for testing purposes
# 		verbatimTextOutput("unitClass"),
    
    # Text form
    conditionalPanel(
      condition = "input.class == 'text'",
      tags$textarea(id="text_output", rows=3, cols=40, 
                    placeholder="Text output")
      ),
    
    # Command question form
  	conditionalPanel(
    	condition = "input.class == 'cmd_question'",
    	tags$textarea(id="cmd_output", rows=3, cols=40, 
    	              placeholder="Question"),
    	tags$textarea(id="cmd_correct_answer", rows=3, cols=40,
                    placeholder="Correct answer (a valid R expression)"),
    	tags$textarea(id="cmd_answer_tests", rows=3, cols=40, 
                    placeholder="Answer tests (separated by semi-colons)"),
    	tags$textarea(id="cmd_hint", rows=3, cols=40, 
    	              placeholder="Hint")
      ),
    
		# Multiple choice question form
		conditionalPanel(
		  condition = "input.class == 'mult_question'",
		  tags$textarea(id="mult_output", rows=3, cols=40, 
		                placeholder="Question"),      
      tags$textarea(id="mult_answer_choices", rows=3, cols=40,
		                placeholder="Answer choices (one per line, no maximum)"),
		  tags$textarea(id="mult_correct_answer", rows=3, cols=40,
		                placeholder="Correct answer (must match exactly one answer choice)"),
		  tags$textarea(id="mult_hint", rows=3, cols=40, 
		                placeholder="Hint")
      ),
    
		# Numeric question
		conditionalPanel(
		  condition = "input.class == 'exact_question'",
		  tags$textarea(id="num_output", rows=3, cols=40, 
		                placeholder="Question"),
		  tags$textarea(id="num_correct_answer", rows=3, cols=40,
		                placeholder="Correct answer (a decimal number or integer)"),
		  tags$textarea(id="num_hint", rows=3, cols=40, 
		                placeholder="Hint")
      ),
    
    # Video form
		conditionalPanel(
		  condition = "input.class == 'video'",
		  tags$textarea(id="video_output", rows=3, cols=40, 
		                placeholder="Would you like to watch a video about <insert topic here> ?"),
		  tags$textarea(id="video_link", rows=3, cols=40, 
		                placeholder="Video URL (http://youtu.be/S1tBTlrx0JY)")
      ),
    
    # Figure form
		conditionalPanel(
		  condition = "input.class == 'figure'",
		  tags$textarea(id="fig_output", rows=3, cols=40, 
		                placeholder="Text output"),
		  tags$textarea(id="figure", rows=3, cols=40, 
		                placeholder="my_figure.R"),
		  selectInput("figure_type", "Figure type:",
                  choices = c("New" = "new", "Additional" = "add"))
      ),
    
    # Button to add unit
    actionButton("addit", "Add it!"),
    
    # Button to close the authoring tool
    actionButton("done", "I'm done!")
    )
))