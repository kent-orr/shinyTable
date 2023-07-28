#' Add a New Row
#'
#' This function adds a new row to a dataframe. The new row is populated with default values based on the data types of the columns present in the input object.
#'
#' @param x A data table-like object or a reactive expression returning a data table-like object.
#' @param position Character or numeric value indicating where the new row should be inserted. It can take one of the following values:
#'   \itemize{
#'     \item "first": Insert the new row at the beginning of the data table (top).
#'     \item "last": Insert the new row at the end of the data table (bottom).
#'     \item numeric: A numeric value representing the row number where the new row should be inserted. If the value is within the range of the current data table, the new row will be inserted at the specified position.
#'   }
#' @param ... Additional arguments not used in this function.
#'
#' @return A data table-like object with the new row added.
#'
#' @examples
#' # Create a sample data table
#' dt <- data.table::data.table(
#'   x = c(1L, 2L, 3L),
#'   y = c(0.1, 0.2, 0.3),
#'   z = c("A", "B", "C"),
#'   flag = c(TRUE, FALSE, TRUE),
#'   date_val = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
#'   timestamp_val = as.POSIXct(c("2023-01-01 12:00:00", "2023-02-01 12:00:00", "2023-03-01 12:00:00"))
#' )
#'
#' # Add a new row with default values at the beginning (top) of the data table
#' new_dt_first <- add_row(dt, position = "first")
#'
#' # Add a new row with default values at the end (bottom) of the data table
#' new_dt_last <- add_row(dt, position = "last")
#'
#' # Add a new row with default values at the specified position (between row 2 and row 3)
#' new_dt_custom <- add_row(dt, position = 2)
#'
#' @export
add_row <- function(x, position = "first", session) {
  
  # capture current table
  y <- if (is.reactive(x)) x() else x
  data.table::setDT(y)
  
  # determine column types
  col_types = sapply(y, class)
  for (i in seq_along(col_types)) {
    if ("POSIXct" %in% col_types[[i]])
      col_types[i] <- "POSIXct"
  }
  # create new row with default values
  new_row = lapply(col_types, \(x) switch(x,
                                integer = 0,
                                numeric = 0,
                                character = "",
                                logical = TRUE,
                                Date = as.Date("1970-01-01"),  # Change the default date value as needed.
                                POSIXct = as.POSIXct("1970-01-01 00:00:00"),  # Change the default POSIXct value as needed.
                                default = NA)) |>
    data.table::as.data.table()
  
  # inform session
  session$userData$shinyTable(
    list(action = "add_row"
         , rows = new_row
         , position = if(position == "first") 1 else if (position == "last") nrow(y) else position)
  )
  
  # change current table
  z <- if (position == "first" | position == 1) 
    rbind(new_row, y)
  else if (position == "last" | position == nrow(y))
    rbind(y, new_row)
  else if (position %in% seq_len(nrow(y))) {
    top = y[1:(position-1)]
    bottom = y[position:nrow(y)]
    rbind(top, new_row) |>
      rbind(bottom)
  }
  
  if (is.reactive(x)) x(z)
  
}

#' Delete a Row
#'
#' This function removes a row from a dataframe object based on the specified row number.
#'
#' @param x A data table-like object or a reactive expression returning a data table-like object.
#' @param position Numeric value representing the row number to be deleted.
#'
#' @return A data table-like object with the specified row removed.
#'
#' @examples
#' # Create a sample data table
#' dt <- data.table::data.table(
#'   x = c(1L, 2L, 3L),
#'   y = c(0.1, 0.2, 0.3),
#'   z = c("A", "B", "C"),
#'   flag = c(TRUE, FALSE, TRUE),
#'   date_val = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
#'   timestamp_val = as.POSIXct(c("2023-01-01 12:00:00", "2023-02-01 12:00:00", "2023-03-01 12:00:00"))
#' )
#'
#' # Delete row 2 from the data table
#' new_dt <- delete_row(dt, row_num = 2)
#'
#' @import data.table
#' @export
remove_row <- function(x, position) {
  y <- if (is.reactive(x)) x() else x
  data.table::setDT(y)
  
  # send removed rows to session
  session$userData$shinyTable(
    list(action = "remove_row"
         , rows = y[-position])
  )
  
  # remove selected rows
  if (row_num %in% seq_len(nrow(y))) {
    y <- y[-position]
  } else {
    warning("Row number out of range. No rows deleted.")
  }
  
  # return table
  if (is.reactive(x)) x(y)
  y
}




#' UI for an interactive shinyTable module
#'
#' @param id module ID
#'
#' @export
#'
shinyTableUI <- function(id, add_remove = TRUE, verbose = interactive()) {
  ns <- NS(id)
  tagList(
    if (add_remove) {tags$div(class = "add_remove"
                              , actionButton(ns("add_row"), "Add Row", icon("plus"))
                              , actionButton(ns("remove_row"), "Remove Row", icon("minus")))
      }
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
      
      init = if(is.reactive(x)) x else reactiveVal(x)
      current = if(is.reactive(x)) x else reactiveVal(x)
      if (is.null(session$userData$shinyTable)) session$userData$shinyTable <- reactiveVal(list())
      
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
        , session$userData$shinyTable()
        )
      })
      
      observeEvent(input[["add_row"]], ignoreInit = TRUE, {
        y = add_row(current, "first", session = session)
        init(current())
      })
      
      observeEvent(input[["remove_row"]], ignoreInit = TRUE, {
        y = remove_row(current, input[[table_id]]$selected$i)
        init(current)
      })
      
      observeEvent(input[[table_id]], ignoreNULL = TRUE, {
        
        x = current()
        
        if (input[[table_id]]$action == "value_change") {
          l = input[[table_id]]
          i = l$i; j = l$j; value = l$value
          
          if (is.null(value)) value = NA
          x[i][[j]] <- value
          
          a = list(table = gsub(session$ns(""), "", l$table)
                   , i = i, j = j, value = value
                   , action = "value_change")
          session$userData$shinyTable(a)
        } else if (input[[table_id]]$action == "add_row") {
            browser()
        }
        
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
    shinyTableUI("a", add_remove = TRUE, verbose = TRUE)
    , verbatimTextOutput("main_console")
  )
  
  server <- function(input, output, session) {
    y= mtcars[1:3, 1:2]; y$newcol = c(TRUE, FALSE, TRUE); y$datetime = Sys.time()
    x = shinyTableServer("a", y, mode = mode,  table_id = "test_table", id_cols = 1)
    output$main_console <- renderPrint(x())
  }
  
  shiny::shinyApp(ui, server)
}

run_test()

