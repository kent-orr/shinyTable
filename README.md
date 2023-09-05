# shinyTable: Interactive Table Editing in Shiny

shinyTable is an R package that provides an intuitive and simple interface for creating and interacting with tables in Shiny apps. Unlike the popular Shiny extension DT, which focuses on providing a DataTables interface, shinyTable streamlines the process of creating editable tables. Its emphasis is on interacting with data in an R-native way, utilizing the familiar syntax of the base R / `data.table` package for editing a dataframe: `x[i,j] <- value`. To do so the html generated from a dataframe includes i and j attributes for the input objects, which trigger reactive inputs. So changing the 2nd column on the first row for a table with the id `test` would create an `input$test` with `input$test$i = 1` and `input$test$j = 2`

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

