#' Get the input type for a given column class.
#'
#' This function maps a given column class to an appropriate input type for HTML forms.
#' The input type is selected based on the column class and is returned as a character string.
#' If the column class is "character" or "factor", the input type will be "text".
#' If the column class is "logical", the input type will be "checkbox".
#' If the column class is "integer" or "numeric", the input type will be "number".
#' If the column class is "Date", the input type will be "date".
#' If the column class is "POSIXct" or "POSIXt", the input type will be "datetime-local".
#' If the column class is "times", the input type will be "time".
#' For any other column class, the default input type will be "text".
#'
#' @param column_class A character string specifying the class of the column.
#'
#' @return A character string representing the input type for the given column class.
#'
#' @examples
#' # Example 1
#' get_column_input_type("character")
#' # Output: "text"
#' 
#' # Example 2
#' get_column_input_type("numeric")
#' # Output: "number"
#'
#' @export
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
         "text"
  )
}

#' Generate input tags based on column types.
#'
#' This function generates input tags based on the column types specified in the \code{col_types} argument.
#' The generated input tag is determined by the column type \code{col_types[[j]]} and is adjusted
#' according to the specific conditions for certain types.
#'
#' @param col_types A character vector specifying the types of columns.
#' @param x A data structure containing the data for the specified column.
#' @param i An integer index indicating the row number.
#' @param j An integer index indicating the column number.
#' @param table_id A character string representing the table identifier.
#' 
#' @export
generate_tags_input <- function(col_types, x, i, j, table_id) {
  # browser()
  type = col_types[[j]]
  data.table::setDT(x)
  value <- if (type == "datetime-local") {
    lubridate::format_ISO8601(x[i][[j]])
  } else {
    x[i][[j]]
  }
  
  size = 3 + max(nchar(x[[j]]))
  
  tags$input(type = type
             , checked = if (type %in% c("radio", "checkbox") && x[i][[j]]) {
               x[i][[j]]
             } else {
               NULL
             } # end checked
             , pattern = if (type == "tel") {"[0-9]{3}-[0-9]{3}-[0-9]{4}"
             } else {
               NULL
             } # end NULL
             , placeholder = if (type == "tel") {"555-55-5555"
             } else {
               NULL
             } # end placeholder
             , step = if(grepl("number", type)) {
               stp = strsplit(type, "-")[[1]]
               if (length(stp) == 1) 1 else stp[2]
             } 
             , value = value,
             i = i, j = j, class = "shinyTable-input", table = table_id,
             size = size,
             style='transition:5s; position:relative; border:none; width:"100%"')
}

#' Create an editable HTML table
#'
#' This function generates an editable HTML table based on the provided data. It can be used in Shiny applications to display and interact with tabular data.
#'
#' @param x A data.frame or reactive object containing the data to be displayed in the table.
#' @param table_id An optional ID for the table. If not provided, the ID will default to the name of the input data.
#' @param id_cols A numeric vector of column indices that should be displayed as static text.
#' @param type_list A list specifying input types for specific columns. The format should be `list(input_type = c(column_indices))`. Column input types are guessed using `shinyTable:::get_column_input_type`, and this argument can be used to override the guesses. To set step use 'number-<step>' as in 'number-.01'
#' @param col_names A character vector specifying custom column names for the table headers. If not provided, column names from the input data will be used.
#' @param skip_cols A numeric vector of column indices to skip during table generation.
#' @param sortable either "asc" or "desc" or NULL, giving sort order or no sort at all
#' @param searchable A boolean to indicate if a search box should be implemented.
#' @param ns The namespace of the Shiny module if used within a module context.
#' @param ... Additional arguments (currently not used).
#'
#' @return An HTML table with interactive input cells and static text cells based on the provided data and parameters.
#'
#' @export
#'
#' @examples
#' # Generate a simple editable table with default settings
#' shinyTable(data.frame(A = 1:5, B = 6:10))
#'
#' # Generate a table with custom column names and input types
#' shinyTable(data.frame(Name = c("Alice", "Bob"), Age = c(25, 30)), col_names = c("Person", "Years"), type_list = list(text = 1, numeric = 2))
#'
#' # Generate a table with specified static text columns and skipped columns
#' shinyTable(data.frame(ID = 1:3, Name = c("Alice", "Bob", "Carol"), Value = c(10, 20, 30)), id_cols = 1, skip_cols = 3)
#'
shinyTable <- function(x,
                       table_id = NULL,
                       id_cols = 1,
                       type_list = NULL,
                       col_names = NULL,
                       skip_cols = NULL,
                       sortable = "asc",
                       searchable = TRUE,
                       ns = NULL,
                       ...) {
  
  if (shiny::is.reactive(x)) x = x()
  
  if (is.null(x) || nrow(x) == 0)
    return(tagLIst(tags$p("No Table Data")))
  
  if (is.null(table_id))
    table_id = deparse(substitute(x))
  
  if (!is.null(ns))
    table_id = ns(table_id)
  
  data.table::setDT(x)
  
  # Create the table headers (thead)
  
  nms = if(!is.null(col_names)) {
    names(col_names)
  } else {
    names(x)
  }
    
  th <- names(x) |> lapply(\(nm) {
    j = which(nm == names(x))
    if (j %in% skip_cols) {
      NULL
    } else {
      # browser()
      i = which(nms == nm)
      tags$th(if(is.null(col_names) || length(i) == 0) nm else col_names[i], i = 0, j = j)
    }
    }) |> tags$thead(class="shinyTable")
  
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
   
    tags$tr(
   
      lapply(1:ncol(x), \(j) {
      # Create plain text columns for id cols
      if (j %in% id_cols) {
        tags$td(x[i][[j]], i = i, j = j, class="shinyTable")
      } 
      # Skip columns specified in skip_cols
      else if (j %in% skip_cols) {
        return()
      } 
      # Create input cells for other columns
      else {
        tags$td(i = i, j = j,
          generate_tags_input(col_types, x, i, j, table_id) # end input
          , class="shinyTable") # end td
          
      }
      
    }), class="shinyTable", onclick="trSelect(this)", i = i) # end trs
    
  }), class="shinyTable"
  ) # end tbody
  
  # Create colgroups
  tg = tags$colgroup(lapply(setdiff(1:ncol(x), skip_cols), \(j) {
    tags$col(j=j)
  }))
  
  # Create the complete table
  tagList(
    if(!is.null(sortable) && sortable %in% c("asc", "desc")) {
      tagList(
        
        tags$label(`for`=paste0(table_id, "-sort"), "Sort By")
        , tags$select(name=paste0(table_id, "-sort")
                      , class = "shinyTable-sort"
                      , onchange=paste0("(function(x){sort_table('", table_id, "', {[x.value]: '", sortable, "'})})(event.target)")
                      , lapply(names(x), \(nm) {
                        # browser()
                        j = which(nm == names(x))
                        if (j %in% skip_cols) {
                          NULL
                        } else {
                          i = which(nms == nm)
                          tags$option(if(is.null(col_names) || length(i) == 0) nm else col_names[i], value = j)
                        }
                      })
        )
      )
    } # end if sortable
    , if(searchable) {
      tagList(
      tags$label(`for` = paste0(table_id, "-search"), "Search"),
      tags$input(type="text", class = "shinyTable-search", id = paste0(table_id, "-search"), onkeyup="searchTable(this)")
      )
    } # end if searchable
    , tags$table(tg, th, tb, id = table_id, width="100%")
    , tags$script(inputChange)
    , tags$script(searchTable)
    , tags$script(sortTable)
  )
}


# shinyTable(mtcars[1:5, 1:5]
#            , col_names = c("mpg" = "MPG")
#            , type_list = list("number-.01" = 2)) |> htmltools::html_print()
