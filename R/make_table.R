# x = mtcars

get_column_input_type <- function(column_class) {
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
#' @param type_list list of column input types of the format `list(text = 2, checkbox = c(3, 5))`. Columns are guessed by `shinyTable:::get_column_input_type`. Arguments override guesses
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
table_html <- function(x, id_cols = 1, skip_cols = 2, table_id = NULL, type_list = NULL, ...) {
  if (shiny::is.reactive(x)) x = x()
  
  if (is.null(table_id))
    table_id = deparse(substitute(x))
  
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
        tags$td(
          tags$input(type = col_types[j]
                     , value = x[i][[j]], i = i, j = j
                     , class="shinyTable-input"
                     , table = table_id
          ) # end input
          , class="shinyTable") # end td
      }
      
    }), class="shinyTable") # end tr
  }), class="shinyTable") # end tbody
  
  # Create the complete table
  tags$table(th, tb, id = paste("st", table_id, sep = "_"))
  
}

table_html(mtcars[1:3, 1:3]) |> htmltools::html_print()


