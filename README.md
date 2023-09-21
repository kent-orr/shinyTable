# shinyTable
## Interactive Table Editing in Shiny

shinyTable is a simple means of creating editable html tables that focuses on 
bringing a more R native way to handle edits. shinyTable makes a simple grid of html inputs, 
each with `i`, `j` attributes that are gathered as a reactive input. 

The idea is to use the indexing of base R to make changes, such that `x[i, j] <- input_value`

For example: the value of `input$tab` for a `shinyTable` with `table_id="tab"` after editing row 1 column 7 t be a value of "new_value" would be as follows:

```{r}
$i
[1] 1

$j
[1] 3

$value
[1] "new_value"

$table
[1] "tab"

$action
[1] "value_change"
```

from there you can use that information to update a backend database, local csv or dataframe, etc.

```
# assuming a shinyTable is made from a table `df` with a `unique_identifier` for each row

observeEvent(input[['tab']], {
  i = input$tab$i
  j = input$tab$j
  value = input$tab$value
  
  if (input$tab$action == "value_change") {
    uid = df[1, "unique_identifier"]
    jcol = names(df)[j]
    
    dbExecute(conn, glue::glue("update table set '{jcol}' = '{value}' where unique_identifier = '{uid}'"))
  }
  
})

```


## Getting Started

**Installation:** Install from github with:
```{r}
remotes::install_github('kent-orr/shinyTable')
# for dev branch remotes::install_github('kent-orr/shinyTable', ref = 'dev')
```

## Module Integration

`shinyTable` comes with a server and UI module pair for drop-in use in a shiny application. You can use the code below, or run it from shinyTable::run_test()

```{r}
ui <- fluidPage(
  shinyTableUI("module_id", shiny_sort = TRUE, shiny_search = TRUE)
  , verbatimTextOutput("main_console")
)

server <- function(input, output, session) {
  y <- mtcars[1:5, 1:2]; y$newcol = c(TRUE, FALSE, TRUE, FALSE, TRUE); y$datetime = as.POSIXct(Sys.time())
  y <- cbind(list("name" = c("A", "C", "B", "B", "C")), y)
  
  x <- shinyTableServer("module_id"
                       , y
                       , table_id = "tab"
                       , shiny_sort = "asc"
                       , shiny_search = TRUE
                       , col_names = c("MPG" = "mpg")
  )
  
  output$main_console <- renderPrint(x())
}

shiny::shinyApp(ui, server)
```


## CSS Styling

If you do choose to style your shinyTable, unlike DT which requires JS callback function, shinyTables are simple html tables of class 'shinyTable', which means that styling can be applied quickly and simply via css. for example:

```{css}
tbody.shinyTable { border: 1px black dotted; }
th.shinyTable { border: 1px black solid; }
```

