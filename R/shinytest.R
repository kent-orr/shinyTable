

shinyTableUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("out")  )
    , actionButton(ns("howdy"), "jey")
    , verbatimTextOutput(ns("console"))
  )
}

shinyTableServer <- function(id
                             , x
                             , table_id = NULL
                             , id_cols = 1
                             , skip_cols = NULL
                             , type_list = NULL
                             , ...
                             ) {
  moduleServer(
    id,
    function(input, output, session) {
      
      init = if(is.reactive(x)) x else reactiveVal(x)
      current = if(is.reactive(x)) x else reactiveVal(x)
      
      output$out <- renderUI({
        
        shiny_table(x = init, table_id = table_id
                   , id_cols = id_cols
                   , skip_cols = skip_cols
                   , type_list = type_list
                   , id = id
                   , ...)
      })
      
      output$console <- renderPrint({
        list(reactiveValuesToList(input)
             , input[[table_id]]
        )
      })
      
      observeEvent(input[[table_id]], ignoreNULL = TRUE, {
        x = current()
        l = input[[table_id]]
        i = l$i; j = l$j; value = l$value
        # browser()
        x[i][[j]] <- value
        current(x)
        
        
      })
      
      return(current)
      
    }
  )
}

ui <- fluidPage(
  shinyTableUI("a")
  , verbatimTextOutput("main_console")
)

server <- function(input, output, session) {
  y= mtcars[1:3, 1:2]; y$newcol = c(TRUE, FALSE, TRUE); y$datetime = Sys.time()
  x = shinyTableServer("a", y,  table_id = "test_table", id_cols = 1)
  output$main_console <- renderPrint(x())
}

# shiny::shinyApp(ui, server)

