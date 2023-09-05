# shinyTable: Interactive Table Editing in Shiny

shinyTable is an R package that provides an intuitive and simple interface for creating and interacting with tables in Shiny apps. Unlike the popular Shiny extension DT, which focuses on providing a DataTables interface, shinyTable streamlines the process of creating editable tables. Its emphasis is on interacting with data in an R-native way, utilizing the familiar syntax of the base R / `data.table` package for editing a dataframe: `x[i,j] <- value`. To do so the html generated from a dataframe includes i and j attributes for the input objects, which trigger reactive inputs. So changing the 2nd column on the first row for a table with the id `test` would create an `input$test` with `input$test$i = 1` and `input$test$j = 2`

## Getting Started

**Installation:** Install from github with:
```{r}
remotes::install_github(kent-orr/shinyTable)
# for dev branch remotes::install_github(kent-orr/shinyTable, ref = 'dev')
```

## Module Integration

shinyTable comes with a server and UI module pair for drop-in use in a shiny application.

```{r}
ui <- fluidPage(
  shinyTableUI("a", verbose = FALSE, sort = FALSE)
  , verbatimTextOutput("main_console")
)

server <- function(input, output, session) {
  # y = mtcars[1:3, 1:2]; y$newcol = c(TRUE, FALSE, TRUE); y$datetime = as.POSIXct(Sys.time())
  x = shinyTableServer("a", data.frame(Name = c("Alice", "Bob"), Age = c(25, 30)), mode = "both", table_id = "test_table", id_cols = 1)
  output$main_console <- renderPrint(x())
}

shiny::shinyApp(ui, server)
```


Here are some key differences between `shinyTable` and `DT`:

## Simplicity of Interactive Tables

shinyTable takes a straightforward approach to creating interactive tables, focusing on simple interactions like editing cells without additional complexities like filtering and pagination offered by DT. This makes shinyTable a great choice for users seeking a minimalistic, straightforward table-editing interface.

## Edit Table Using R-Native Syntax

shinyTable's real strength lies in its use of native R syntax for table manipulation. It leverages the syntax from the data.table package for editing dataframes, i.e., `x[i,j] <- value`. This makes it easy for R users to grasp and efficiently manipulate their data. Use `run_test(mode = "both")` to see all available outputs from a shinyTable.

## CSS Styling

If you do choose to style your shinyTable, unlike DT which requires JS callback function, shinyTables are simple html tables of class 'shinyTable', which means that styling can be applied quickly and simply via css. for example:

```{css}
tbody.shinyTable { border: 1px black dotted; }
th.shinyTable { border: 1px black solid; }
```

## Specialized Interface for Shiny

shinyTable provides a built-in module specifically designed to seamlessly integrate with Shiny apps. This makes the process of incorporating interactive tables into your Shiny app a breeze. By contrast, DT has a more general focus and can be used outside of Shiny, which can add extra complexity when embedding it into Shiny apps.

## Integration with Reactive Programming

shinyTable is designed with Shiny's reactive programming model in mind. Its interface has reactive inputs and outputs, making it highly compatible with reactive functions in Shiny. The interactivity and reactivity are handled in a more R-centric way compared to DT, which handles these features via DataTables JavaScript library.

<table id="mtcars[1:2, 1:4]" width="100%">
  <colgroup>
    <col j="1"/>
    <col j="2"/>
    <col j="3"/>
    <col j="4"/>
  </colgroup>
  <thead>
    <th i="0" j="1">Mile Per Gallon</th>
    <th i="0" j="2">cyl</th>
    <th i="0" j="3">disp</th>
    <th i="0" j="4">hp</th>
  </thead>
  <tbody class="shinyTable">
    <tr class="shinyTable" onclick="trSelect(this)" i="1">
      <td i="1" j="1" class="shinyTable">21</td>
      <td i="1" j="2" class="shinyTable">
        <input type="number" step="0.01" value="6" i="1" j="2" class="shinyTable-input" table="mtcars[1:2, 1:4]" size="4" style="transition: size 5s;position: relative;border:none;"/>
      </td>
      <td i="1" j="3" class="shinyTable">
        <input type="number" step="0.01" value="160" i="1" j="3" class="shinyTable-input" table="mtcars[1:2, 1:4]" size="6" style="transition: size 5s;position: relative;border:none;"/>
      </td>
      <td i="1" j="4" class="shinyTable">
        <input type="number" step="0.01" value="110" i="1" j="4" class="shinyTable-input" table="mtcars[1:2, 1:4]" size="6" style="transition: size 5s;position: relative;border:none;"/>
      </td>
    </tr>
    <tr class="shinyTable" onclick="trSelect(this)" i="2">
      <td i="2" j="1" class="shinyTable">21</td>
      <td i="2" j="2" class="shinyTable">
        <input type="number" step="0.01" value="6" i="2" j="2" class="shinyTable-input" table="mtcars[1:2, 1:4]" size="4" style="transition: size 5s;position: relative;border:none;"/>
      </td>
      <td i="2" j="3" class="shinyTable">
        <input type="number" step="0.01" value="160" i="2" j="3" class="shinyTable-input" table="mtcars[1:2, 1:4]" size="6" style="transition: size 5s;position: relative;border:none;"/>
      </td>
      <td i="2" j="4" class="shinyTable">
        <input type="number" step="0.01" value="110" i="2" j="4" class="shinyTable-input" table="mtcars[1:2, 1:4]" size="6" style="transition: size 5s;position: relative;border:none;"/>
      </td>
    </tr>
  </tbody>
</table>
