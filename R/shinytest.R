

shinyTableUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("out")  )
    , actionButton(ns("howdy"), "jey")
    , verbatimTextOutput(ns("console"))
  )
}

shinyTableServer <- function(id
                             , x
                             , table_id = NULL
                             , id_cols = 1
                             , skip_cols = NULL
                             , type_list = NULL
                             , ...
                             ) {
  moduleServer(
    id,
    function(input, output, session) {
      
      init = if(is.reactive(x)) x else reactiveVal(x)
      current = if(is.reactive(x)) x else reactiveVal(x)
      
      output$out <- renderUI({
        
        shiny_table(x = init, table_id = table_id
                   , id_cols = id_cols
                   , skip_cols = skip_cols
                   , type_list = type_list
                   , id = id
                   , ...)
      })
      
      output$console <- renderPrint({
        list(reactiveValuesToList(input)
             , input[[table_id]]
        )
      })
      
      observeEvent(input[[table_id]], ignoreNULL = TRUE, {
        x = current()
        l = input[[table_id]]
        i = l$i; j = l$j; value = l$value
        # browser()
        if (is.null(value)) value = NA
        x[i][[j]] <- value
        current(x)
        
        
      })
      
      return(current)
      
    }
  )
}

ui <- fluidPage(
  shinyTableUI("a")
  , verbatimTextOutput("main_console")
)

server <- function(input, output, session) {
  y= mtcars[1:3, 1:2]; y$newcol = c(TRUE, FALSE, TRUE); y$datetime = Sys.time()
  x = shinyTableServer("a", y,  table_id = "test_table", id_cols = 1)
  output$main_console <- renderPrint(x())
}

#' Run a test of the shiny table
#' 
#' @export
run_test <- function() {
  shiny::shinyApp(ui, server)
}


test_that("shiny_table works correctly", {
  data <- data.frame(
    id = 1:3,
    name = c("Alice", "Bob", "Charlie"),
    active = c(TRUE, FALSE, TRUE),
    phone = c("123-45-6789", "234-56-7890", "345-67-8901"),
    stringsAsFactors = FALSE
  )
  
  # Test 1: table_id, id_cols, type_list, and skip_cols are default (NULL)
  table1 <- shiny_table(data)
  expect_equal(table1[[1]]$name, "table")
  expect_equal(table1[[1]]$attribs$id, "st_data")
  expect_equal(length(table1[[1]]$children[[1]]$children[[1]])
               , ncol(data))  # number of columns
  expect_equal(length(table1[[1]]$children[[2]]$children)
               , nrow(data))  # number of rows
  
  # Test 2: table_id, id_cols, type_list, and skip_cols are specified
  table2 <- shiny_table(data, table_id = "test_table", id_cols = 2,
                        type_list = list(text = 1, checkbox = 3, tel = 4
                                         ),
                        skip_cols = 1
                        )
  
  
  expect_equal(table2[[1]]$attribs$id, "st_test_table")
  expect_equal(length(table2[[1]]$children[[1]]$children[[1]])
               , ncol(data) - 1)  # number of columns
  expect_equal(length(table2[[1]]$children[[2]]$children[[1]]) - 1
               , nrow(data))  # number of rows (header row is a NULL child)
  
  # Test 3: id argument is specified
  table3 <- shiny_table(data, id = "module1")
  expect_equal(attr(table3[[1]], "id"), "st_module1_data")
  
  # Test 4: id_cols includes all columns
  table4 <- shiny_table(data, id_cols = 1:ncol(data))
  # Check that all columns are rendered as static text
  all_static_text <-
    sapply(table4[[1]]$children[[2]]$children[[1]]$children, function(child) {
      "shinyTable" %in% attr(child, "class")
    })
  expect_true(all(all_static_text))
})
