#' @import shiny
display_math <- function(x) {
  runApp(list(
    ui = basicPage(
      tags$head(tags$script(src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML-full", type = 'text/javascript'),
                tags$script( "MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}});", type='text/x-mathjax-config')
      ),
      br(),
      h4(helpText(x)),
      br(),
      actionButton("cont", "Continue!")
    ),
    server = function(input, output) {
      observe({
        if(input$cont == 0) {
          return()
        }
        stopApp()
      })
    }
  ), launch.browser = rstudio::viewer)
}

# tester <- "Here is the formula for simple linear regression: $$y_i = \\beta_0 + \\beta_1 x_i$$Here is what it looks like when you add a second covariate: $$y_i = \\beta_0 + \\beta_1 x_{1i} + \\beta_2 x_{2i}$$"

# display_math(tester)