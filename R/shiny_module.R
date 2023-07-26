add_row <- function(x, ...) {
  if (is.reactive(x)) y <- x()
  y = cbind(y, y[nrow(y)])
  if (is.reactive(x)) x(y)
  y
}

#' UI for an interactive shinyTable module
#'
#' @param id module ID
#'
#' @export
#'
shinyTableUI <- function(id, verbose = interactive()) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("out")  )
    , actionButton(ns("howdy"), "jey")
    , if (verbose) verbatimTextOutput(ns("console"))
  )
}

#' Server logic for a shinyTable instance
#'
#' @param id module ID
#' @param x a dataframe or reactive dataframe
#' @param mode either "data.frame" or "inputs"
#' @inheritParams shiny_table
#'
#' @return 
#' @export
#'
#' @examples
shinyTableServer <- function(id
                             , x
                             , mode = "inputs"
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
        
        shinyTable(x = init
                   , mode = mode
                   , table_id = table_id
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
        if (is.null(value)) value = NA
        x[i][[j]] <- value
        current(x)
        
        
      })
      
      return_out <- reactive({
        if (mode == "inputs") {
          x = input[[table_id]]
          x[[table]] = gsub(paste0(id, "-"), "", x[[table]])
          x
        } else if (mode == "data.frame") {
          current
        }
      })
      
      return(return_out)
      
    }
  )
}



#' Run a test of the shiny table
#' 
#' @export
run_test <- function(mode = "data.frame") {
  ui <- fluidPage(
    shinyTableUI("a", verbose = TRUE)
    , verbatimTextOutput("main_console")
  )
  
  server <- function(input, output, session) {
    y= mtcars[1:3, 1:2]; y$newcol = c(TRUE, FALSE, TRUE); y$datetime = Sys.time()
    x = shinyTableServer("a", y, mode = mode,  table_id = "test_table", id_cols = 1)
    output$main_console <- renderPrint(x())
  }
  
  shiny::shinyApp(ui, server)
}