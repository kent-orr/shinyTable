## code to prepare `js_scripts` dataset goes here
inputChange = readLines("inst/inputChange.js", warn = FALSE) |> paste(collapse = "\n") |> htmltools::HTML()
js_helpers = readLines("inst/helpers.js", warn = FALSE) |> paste(collapse = "\n") |> htmltools::HTML() 
searchTable = readLines("inst/searchTable.js", warn = FALSE) |> paste(collapse = "\n") |> htmltools::HTML() 
sortTable = readLines("inst/sortTable.js", warn = FALSE) |> paste(collapse = "\n") |> htmltools::HTML() 
hideRows = readLines("inst/hideRows.js", warn = FALSE) |> paste(collapse = "\n") |> htmltools::HTML() 


usethis::use_data(inputChange, overwrite = TRUE)
usethis::use_data(js_helpers, overwrite = TRUE)
usethis::use_data(searchTable, overwrite = TRUE)
usethis::use_data(sortTable, overwrite = TRUE)
usethis::use_data(hideRows, overwrite = TRUE)
