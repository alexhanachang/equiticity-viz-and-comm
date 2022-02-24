library(shiny)

ui <- fluidPage(
  r <- reactiveValues(count = 0, x = 1)
  class(r)
  #> [1] "rv_flush_on_write" "reactivevalues"
  observe({
    r$x
    r$count <- isolate(r$count) + 1
  })
  
  r$x <- 1
  r$x <- 2
  r$count
  #> [1] 2
  
  r$x <- 3
  r$count
  #> [1] 3
)

server <- function(input, output, session) {
  observeEvent(r$x, { 
    r$count <- r$count + 1 
  }) 
}

shinyApp(ui, server)