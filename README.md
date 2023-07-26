# shinyTable: Interactive Table Editing in Shiny

shinyTable is an R package that provides an intuitive and simple interface for creating and interacting with tables in Shiny apps. Unlike the popular Shiny extension DT, which focuses on providing DataTables interface, shinyTable streamlines the process of creating editable tables. Its emphasis is on interacting with data in an R-native way, utilizing the familiar syntax of the data.table package for editing a dataframe: x[i,j] <- value.

Here are some key differences between shinyTable and shiny::DT:

## Simplicity of Interactive Tables

shinyTable takes a straightforward approach to creating interactive tables, focusing on simple interactions like editing cells, without additional complexities like sorting, filtering, and pagination offered by DT. This makes shinyTable a great choice for users seeking a minimalistic, straightforward table-editing interface.

## Edit Table Using R-Native Syntax

shinyTable's real strength lies in its use of native R syntax for table manipulation. It leverages the syntax from the data.table package for editing dataframes, i.e., x[i,j] <- value. This makes it easy for R users to grasp and efficiently manipulate their data.

## Specialized Interface for Shiny

shinyTable provides a built-in module specifically designed to seamlessly integrate with Shiny apps. This makes the process of incorporating interactive tables into your Shiny app a breeze. By contrast, DT has a more general focus and can be used outside of Shiny, which can add extra complexity when embedding it into Shiny apps.

## Integration with Reactive Programming

shinyTable is designed with Shiny's reactive programming model in mind. Its interface has reactive inputs and outputs, making it highly compatible with reactive functions in Shiny. The interactivity and reactivity are handled in a more R-centric way compared to DT, which handles these features via DataTables JavaScript library.

shinyTable offers a streamlined and intuitive way of creating editable tables in Shiny, especially for users who prefer using native R syntax for dataframe manipulation. DT may be a more suitable choice for those needing extensive DataTables features or working outside of Shiny apps. Choose shinyTable if you're looking for an interactive table package that is deeply integrated with Shiny and leverages familiar R syntax for easy and efficient data manipulation.
