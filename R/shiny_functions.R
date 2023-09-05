


#' UI for an interactive shinyTable module
#'
#' This function generates the user interface (UI) for an interactive shinyTable module within a Shiny application. It provides options to add and remove rows from the table, and can also display a sorting option.
#'
#' @param id The module ID.
#' @param add_remove Should there be add/remove row buttons.
#' @param verbose Whether to print console output.
#' @param sort Whether to include a sorting option for the table.
#' @param add_text The label for the "Add Row" button.
#' @param remove_text The label for the "Remove Row" button.
#'
#' @export
#'
#' @examples
#' # Create the UI for a shinyTable module
#' shinyTableUI("table_module", add_remove = TRUE, sort = TRUE)
#'
#' # Use the module in the UI
#' ui <- fluidPage(
#'   shinyTableUI("table_module", add_remove = TRUE, sort = TRUE),
#'   # Other UI elements...
#' )
#'
#' # Define server logic for the UI
#' server <- function(input, output, session) {
#'   shinyTableServer("table_module", data.frame(Name = c("Alice", "Bob"), Age = c(25, 30)), mode = "inputs")
#'   # Other server logic...
#' }
#'
#' shinyApp(ui, server)
#'
shinyTableUI <- function(id
                         , add_remove = TRUE
                         , sort = TRUE
                         , add_text = "Add Row"
                         , remove_text = "Remove Row"
                         , verbose = interactive()) {
  ns <- NS(id)
  tagList(
    tags$script(js_helpers)
    , if (add_remove) 
      tags$div(actionButton(ns("add_row"), add_text, icon = icon("plus"))
              , actionButton(ns("remove_row"), remove_text, icon = icon("minus"))
              , if (sort) uiOutput(ns("sort")))
    , uiOutput(ns("out"))
    , if (verbose) verbatimTextOutput(ns("console"))
  )
}

#' Server logic for a shinyTable instance
#'
#' This module provides the server logic for a shinyTable instance, which allows you to create and manage an editable HTML table within a Shiny application.
#'
#' @param id The module ID.
#' @param x A data frame or reactive data frame containing the data for the table.
#' @param mode The mode for the table, either "data.frame", "inputs", or "both".
#' @param table_id An optional ID for the table. If not provided, a default ID will be used.
#' @param id_cols A numeric vector of column indices to be displayed as static text.
#' @param sort_cols A numeric vector of column indices to use for sorting the table.
#' @param col_names A character vector specifying custom column names for the table headers.
#' @param uid_cols A numeric vector of column indices to be used as unique identifiers for each row.
#' @param skip_cols A numeric vector of column indices to skip during table generation.
#' @param type_list A list specifying input types for specific columns.
#' @param ... Additional arguments (currently not used).
#'
#' @return The module server function that defines the behavior of the shinyTable instance.
#'
#' @export
#'
#' @examples
#'
#' # Use the module in the UI
#' ui <- fluidPage(
#'   shinyTableUI("table_module"),
#'   # Other UI elements...
#' )
#'
#' # Define server logic for the UI
#' server <- function(input, output, session) {
#'   shinyTableServer("table_module", data.frame(Name = c("Alice", "Bob"), Age = c(25, 30)), mode = "inputs")
#'   # Other server logic...
#' }
#'
#' shinyApp(ui, server)
#'
shinyTableServer <- function(id
                             , x
                             , mode = "inputs"
                             , table_id = NULL
                             , id_cols = NULL
                             , sort_cols = id_cols
                             , col_names = NULL
                             , uid_cols = NULL
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
                   , uid_cols = uid_cols
                   , sort_cols = id_cols
                   , skip_cols = skip_cols
                   , col_names = col_names
                   , type_list = type_list
                   , ns = session$ns
                   , ...)
      })
      
      output$console <- renderPrint({
        list(init()
             , current()
        )
      })
      
      output$sort <- renderUI({
        nm = names(init())
        vals = seq_along(nm)
        if(length(skip_cols) > 0) {
          nm <- nm[-skip_cols]
          vals <- vals[-skip_cols]
        } else {nm}
        for (i in seq_along(nm)) {
          j = which(names(col_names) == nm[i])
          if (length(j) > 0)
            nm[i] <- unlist(col_names[j])
        }
        
        ch = vals; names(ch) = nm
        selectInput(session$ns("sort"), "Sort Table", choices = ch, selected = ch[1], multiple = TRUE, selectize = TRUE)
      })
      
      observeEvent(input$sort, ignoreNULL = TRUE, {
        # browser()
        sort_cols = input$sort
        if(any(is.na(sort_cols))) return()
        x = sapply(sort_cols, \(x) if(x<0) "desc" else "asc")
        x = as.list(x)
        names(x) <- sort_cols
        session$sendCustomMessage('sort_table', list(tableId = session$ns(table_id), sortingDict = x))
      })
      
      observeEvent(input$add_row, ignoreInit = TRUE, {
        # browser()
        init(current())
        y = init()[1,]
        classes = sapply(y, class)
        classes = sapply(classes, `[`, 1)
        f = \(x) switch(x, 
                        numeric = 0
                        , character = "-"
                        , logical = FALSE
                        , POSIXct = as.POSIXct(Sys.time()))
        y = data.frame(lapply(classes, f))
        if (!is.null(uid_cols)) {
          # browser()
          nm = names(uid_cols)
          vals = uid_cols
          for (i in seq_along(uid_cols)) {
            if (!is.null(nm[i]) && nm[i] != "") 
              y[[i]] = nm[i]
            else 
              y[[i]] = uid()
          }
        }
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
          x = current()[i,] |> unlist() |> as.character() 
          if (!is.null(skip_cols))
            x = x[-skip_cols] 
          
          x = x |>
            stringr::str_trunc(20, side = "right") |> 
            stringr::str_pad(20, side = "left", pad = "_") |>
            paste(collapse = "|")
          
          paste(paste0("[", i, "]: "), x)
        })
        
        showModal(
          modalDialog(
            title = "Remove"
            , size = "xl"
            , tags$div(style='font-family:monospace;',
                       selectInput(session$ns("remove_choices")
                                   , label = "Rows to Remove"
                                   , choices = ch
                                   , multiple = TRUE
                                   , selectize = FALSE
                                   , size = min(15, nrow(current()))
                                   , width = "100%")
            )
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
        
        input_out(list(
          table = table_id
          , action = "remove_row"
          , i = current()[i,]
        ))
        
        current(x)
        init(x)
        
        removeModal()
      })
      
      observeEvent(input[[table_id]], ignoreNULL = TRUE, {
        y = current()
        l = input[[table_id]]
        i = l$i; j = l$j; value = l$value
        if (is.null(value)) value = NA
        y[i][[j]] <- value
        current(y)
        # init(x)
      })
      
      input_out <- reactiveVal(list())
      
      observeEvent(input[[table_id]], ignoreNULL = TRUE, {
        x = input[[table_id]]
        if (is.null(x)) return("nothing")
        x$table = gsub(paste0(id, "-"), "", x$table)
        input_out(x)
      })
      
      return_out <- reactive({
        df = current()
        if (mode == "inputs") {
          input_out()
        } else if (mode == "data.frame") {
          current()
        } else {
          list(inp = input_out(), df = df
               , current = current, init = init)
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
    shinyTableUI("a", verbose = TRUE, sort = TRUE)
    , verbatimTextOutput("main_console")
  )
  
  server <- function(input, output, session) {
    y= mtcars[1:3, 1:2]; y$newcol = c(TRUE, FALSE, TRUE); y$datetime = as.POSIXct(Sys.time())
    x = shinyTableServer("a", y, mode = mode, table_id = "test_table", col_names = list("mpg" = "Miles per Gallon"), id_cols = 1)
    output$main_console <- renderPrint(x())
  }
  
  shiny::shinyApp(ui, server)
}

if (interactive()) run_test(mode = "inputs")
