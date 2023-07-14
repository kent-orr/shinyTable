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


#' Title
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
table_html <- function(x, table_id = NULL, type_list = NULL, ...) {
  if (shiny::is.reactive(x)) x = x()
  
  if (is.null(table_id))
    table_id = deparse(substitute(x))
  
  data.table::setDT(x)
  
  th <- names(x) |> lapply(tags$th) |> tags$thead()
  
  col_types = lapply(x, \(y) get_column_input_type(class(y)))
  if (!is.null(type_list)) {
    nm = names(type_list)
    for (i in seq_along(type_list)) {
      col_types[type_list[[i]]] <- nm[i]
    }
  }
  
  tb <-
    tags$tbody(lapply(1:nrow(x), \(i) {
      tags$tr(lapply(1:ncol(x), \(j) {
        tags$td(tags$input(type = col_types[j]
                           , value = x[i][[j]], i = i, j = j
                           , class="shinyTable-input"
                           , table = table_id)
                , class="shinyTable")
      }), class="shinyTable")
    }), class="shinyTable")
  
  tags$table(th, tb, id = paste("shinyTable", table_id, sep = "-"))
  
}

table_html(x[1:2, 1:5])


