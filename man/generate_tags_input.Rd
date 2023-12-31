% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shinyTable.R
\name{generate_tags_input}
\alias{generate_tags_input}
\title{Generate input tags based on column types.}
\usage{
generate_tags_input(col_types, x, i, j, table_id)
}
\arguments{
\item{col_types}{A character vector specifying the types of columns.}

\item{x}{A data structure containing the data for the specified column.}

\item{i}{An integer index indicating the row number.}

\item{j}{An integer index indicating the column number.}

\item{table_id}{A character string representing the table identifier.}
}
\description{
This function generates input tags based on the column types specified in the \code{col_types} argument.
The generated input tag is determined by the column type \code{col_types[[j]]} and is adjusted
according to the specific conditions for certain types.
}
