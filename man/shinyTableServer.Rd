% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shiny_functions.R
\name{shinyTableServer}
\alias{shinyTableServer}
\title{Shiny Table Server Module}
\usage{
shinyTableServer(
  id,
  x,
  table_id = "tab",
  shiny_search = TRUE,
  shiny_sort = "asc",
  id_cols = NULL,
  col_names = NULL,
  skip_cols = NULL,
  type_list = NULL,
  uid_cols = NULL,
  mode = "inputs"
)
}
\arguments{
\item{id}{A unique ID to identify the module server instance.}

\item{x}{A data.frame or reactive object containing the data to be displayed in the table.}

\item{table_id}{An optional ID for the table. If not provided, the ID will default to the name of the input data.}

\item{shiny_search}{A parameter to define if the search functionality should be done via shiny. NULL means no search, FALSE means search is done via js with table input, TRUE means a shiny input will manage search.}

\item{shiny_sort}{A parameter to define if the sort functionality should be done via shiny. NULL means no search, FALSE means ascending sort is done via js with table input, "asc" or "desc" means a shiny input will manage search.}

\item{id_cols}{A numeric vector of column indices that should be displayed as static text.}

\item{col_names}{A character vector specifying custom column names for the table headers \code{new_name = old_name}. If not provided, column names from the input data will be used.}

\item{skip_cols}{A numeric vector of column indices to skip during table generation.}

\item{type_list}{A list specifying input types for specific columns. The format should be \code{list(input_type = c(column_indices))}. Column input types are guessed using \code{shinyTable:::get_column_input_type}, and this argument can be used to override the guesses. To set step use 'number-\if{html}{\out{<step>}}' as in 'number-.01'}

\item{uid_cols}{A vector specifying the columns that contain unique ID information. Default is NULL.}

\item{mode}{A string specifying the mode of the module ("inputs" or "data.frame" or "both). Default is "inputs".}
}
\value{
A reactive expression containing the user inputs or the current data frame based on the specified mode.
}
\description{
This module serves a Shiny table which supports a variety of functionalities including sorting, searching,
adding and removing rows, etc. It also manages the user input and output in the server side of a Shiny application.
}
\examples{
# To use this module in a Shiny app, you would typically call shinyTableServer in the server function
# and shinyTableUI in the UI function. For example:
# server <- function(input, output, session) {
#   shinyTableServer("table1", reactive({ data.frame(x = 1:10, y = 11:20) }))
# }
# ui <- fluidPage(
#   shinyTableUI("table1")
# )
# shinyApp(ui, server)

}
