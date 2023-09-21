i = 0
for (filename in list.files("inst")) {
  func_name = gsub(".js", "", filename)
  func_value = readLines(paste0("inst/", filename), warn = FALSE) |> paste(collapse = "\n") |> htmltools::HTML() 
  assign(func_name, func_value, envir = .GlobalEnv)
  if (i == 0)
    file.remove("R/js_scripts_documentation.R")
  cat(glue::glue("#' {func_name} js function
  #'
  '{func_name}'
  \n"), file = "R/js_scripts_documentation.R", sep = "\n", append = TRUE)
  i = i + 1
}

usethis::use_data(hideRows, inputChange, searchTable, sortTable, updateInput, compress = "bzip2")
