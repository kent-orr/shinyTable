# shinyTable

shinyTable is meant to be a lightweight means of creating interactive tables in 
R Shiny. 

The end goal in functionality is to provide a sql connection, display a query, 
and allow users to modify that data. 

The concept is simple. The user provides a dataframe, and that dataframe then is used to create an array of inputs in an html table. Each table cell and input has `i` and `j` attributes that can be read by shiny, then used to edit the original data.frame using `x[i,j] <- y`.
