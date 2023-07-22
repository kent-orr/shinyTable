# x = mtcars

get_column_input_type <- function(column_class) {
  if (any(column_class %in% c("POSIXct", "POSIXt")))
    column_class = "POSIXct"
  
  switch(column_class,
         character = "text",
         factor = "text",
         logical = "checkbox",
         integer = "number",
         numeric = "number",
         Date = "date",
         POSIXct = "datetime-local",
         times = "time",
         default = "text"
  )
}


#' Create an editable HTML table
#'
#' @param x a data.frame or reactive object
#' @param table_id the id of the table, defaults to x
#' @param id_cols a numeric vector of columns that are displayed as static text
#' @param type_list list of column input types of the format `list(text = 2, checkbox = c(3, 5))`. Columns are guessed by `shinyTable:::get_column_input_type`. Arguments override guesses
#' @param skip_cols a numeric vector of columns to skip
#' @param id if used in a shiny module, the module id
#' @param ... not used yet but don't knock it
#'
#' @return
#' @export
#'
#' @examples
shiny_table <- function(x, table_id = NULL, id_cols = 1, type_list = NULL, skip_cols = NULL, id = NULL, ...) {
  # browser()
  if (shiny::is.reactive(x)) x = x()
  
  if (is.null(table_id))
    table_id = deparse(substitute(x))
  
  if (!is.null(id))
    table_id = paste(id, table_id, sep = "-")
  
  data.table::setDT(x)
  
  # Create the table headers (thead)
  th <- names(x) |> lapply(\(nm) {
    j = which(nm == names(x))
    
    if (j %in% skip_cols) {
      # tags$th(style="wdth:0px;")
    } else {
      tags$th(nm, i = 0, j = j)
    }
    }) |> tags$thead()
  
  # Guess column input types
  col_types = lapply(x, \(y) get_column_input_type(class(y)))
  
  # Override guessed column types with the ones specified in type_list
  if (!is.null(type_list)) {
    nm = names(type_list)
    for (i in seq_along(type_list)) {
      col_types[type_list[[i]]] <- nm[i]
    }
  }
  
  # Create the table body (tbody)
  tb <- tags$tbody(lapply(1:nrow(x), \(i) {
    tags$tr(lapply(1:ncol(x), \(j) {
      
      # Create plain text columns for id cols
      if (j %in% id_cols) {
        tags$td(x[i][[j]], i = i, j = j, class="shinyTable")
      } 
      # Skip columns specified in skip_cols
      else if (j %in% skip_cols) {
        # tags$td(style = "width:0px;")
      } 
      # Create input cells for other columns
      else {
        tags$td(i = i, j = j, 
          tags$input(type = col_types[j] 
                     , checked = if (col_types[j] %in% c("radio", "checkbox") && x[i][[j]]) x[i][[j]] else NULL
                     , pattern = if (col_types[j] == "tel") "[0-9]{3}-[0-9]{3}-[0-9]{4}"
                     , placeholder = if (col_types[j] == "tel") "555-55-5555"
                     
                     , value = if (col_types[j] == "datetime-local") {
                       lubridate::format_ISO8601(x[i][[j]]) 
                     } else {
                       x[i][[j]] 
                     } 
                     
                     , i = i, j = j
                     , class="shinyTable-input"
                     , table = table_id
          ) # end input
          , class="shinyTable") # end td
      }
      
    }), class="shinyTable") # end tr
  }), class="shinyTable") # end tbody
  
  # Create the complete table
  tagList(
    tags$table(th, tb, id = paste("st", table_id, sep = "_"))
    , js_handle_input_change
  )
}
# y= mtcars[1:2, 1:3]; y$newcol = c(TRUE, FALSE); y$datetime = Sys.time(); y$phone = ""
# shiny_table(y, table_id = "test", type_list = list(tel = 6)) |> htmltools::html_print()


