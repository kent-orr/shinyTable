


#' UI for an interactive shinyTable module
#'
#' @param id module ID
#' @param add_remove should there be add/remove row buttons
#' @param verbose print console?
#' @param add_text button label for add row
#' @param remove_text button label for remove row
#'
#' @export
#'
shinyTableUI <- function(id
                         , add_remove = TRUE
                         , verbose = interactive()
                         , add_text = "Add Row"
                         , remove_text = "Remove Row") {
  ns <- NS(id)
  tagList(
    if (add_remove) 
      tags$div(actionButton(ns("add_row"), add_text, icon = icon("plus"))
              , actionButton(ns("remove_row"), remove_text, icon = icon("minus")))
    , uiOutput(ns("out"))
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
      
      # table_id = paste(id, table_id, sep = "-")
      
      init = if(is.reactive(x)) x else reactiveVal(x)
      current = if(is.reactive(x)) x else reactiveVal(x)
      
      output$out <- renderUI({
        
        shinyTable(x = init
                   , mode = mode
                   , table_id = table_id
                   , id_cols = id_cols
                   , skip_cols = skip_cols
                   , type_list = type_list
                   , ns = session$ns
                   , ...)
      })
      
      output$console <- renderPrint({
        list(init()
             , current()
        )
      })
      
      observeEvent(input$add_row, ignoreInit = TRUE, {
        y = init()[1,]
        classes = sapply(y, class)
        classes = sapply(classes, `[`, 1)
        f = \(x) switch(x, 
                        numeric = 0
                        , character = "-"
                        , logical = FALSE
                        , POSIXct = as.POSIXct(Sys.time()))
        y = data.frame(lapply(classes, f))
        init(rbind(y, init()))
        current(init())
        
        
        input_out(list(
          table = table_id
          , action = "add_row"
          , i = y
        ))
        
      })
      
      observeEvent(input$remove_row, ignoreInit = TRUE, {
        ch = 1:nrow(current())
        names(ch) <- lapply(1:nrow(current()), \(i) {
          x = current()[i,] |> unlist() |> as.character() |> 
            stringr::str_trunc(9, side = "right") |> 
            stringr::str_pad(9, side = "left", pad = ".") |>
            paste(collapse = ", ")
          
          paste(paste0("[", i, "]: "), x)
        })
        
        showModal(
          modalDialog(
            title = "Remove"
            , size = "xl"
            , selectInput(session$ns("remove_choices")
                          , label = "Rows to Remove"
                          , choices = ch
                          , multiple = TRUE
                          , selectize = FALSE
                          , size = min(15, nrow(current()))
                          , width = "100%")
            , footer = actionButton(session$ns("remove_submit"), "Submit", icon = icon("close"))
            , easyClose = TRUE
          )
        )
      })
      
      observeEvent(input$remove_submit, ignoreInit = TRUE, {
        if (is.null(input$remove_choices) || length(input$remove_choices) == 0)
          return()
        
        x = current()
        i = as.numeric(input$remove_choices)
        x = x[!i,]
        current(x)
        init(x)
        
        input_out(list(
          table = table_id
          , action = "remove_row"
          , i = i
        ))
        
        removeModal()
      })
      
      observeEvent(input[[table_id]], ignoreNULL = TRUE, {
        x = current()
        l = input[[table_id]]
        i = l$i; j = l$j; value = l$value
        if (is.null(value)) value = NA
        x[i][[j]] <- value
        current(x)
        init(x)
      })
      
      input_out <- reactiveVal(list())
      
      observeEvent(input[[table_id]], ignoreNULL = TRUE, {
        x = input[[table_id]]
        if (is.null(x)) return("nothing")
        x$table = gsub(paste0(id, "-"), "", x$table)
        input_out(x)
      })
      
      return_out <- reactive({
        if (mode == "inputs") {
          input_out()
        } else if (mode == "data.frame") {
          current()
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
    y= mtcars[1:3, 1:2]; y$newcol = c(TRUE, FALSE, TRUE); y$datetime = as.POSIXct(Sys.time())
    x = shinyTableServer("a", y, mode = mode,  table_id = "test_table", id_cols = 1)
    output$main_console <- renderPrint(x())
  }
  
  shiny::shinyApp(ui, server)
}

# run_test(mode = "inputs")
