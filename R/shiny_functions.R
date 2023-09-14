#' Sort Table in a Shiny App
#'
#' This function sends a custom message to the Shiny session to sort a table based on the given parameters.
#' 
#' @param table_id A string specifying the ID of the table that needs to be sorted. If used in a module you may need to add the namespace.
#' @param sortDict A list specifying the sorting parameters. The names of the list are the column names and the list values are the sorting directions (e.g., 'asc' for ascending).
#' @param session A reactive domain (usually from `shiny::getDefaultReactiveDomain()`), representing the Shiny session. Defaults to the current session.
#' 
#' @return Invisible NULL.
#' 
#' @examples
#' \dontrun{
#'   sortTableR("myTableID", list(column1 = 'asc', column2 = 'desc'))
#' }
#' 
#' @seealso 
#' \code{\link[shiny]{session}}
#' \code{\link[shiny]{getDefaultReactiveDomain}}
#' 
#' @export
sortTableR <- function(table_id, sortDict, session = getDefaultReactiveDomain()) {
  session$sendCustomMessage("sortTableMessage", list(tableId = table_id, sortDict = sortDict))
}

#' Search Table in a Shiny App
#'
#' This function sends a custom message to the Shiny session to perform a search within a table based on the provided search string.
#'
#' @inheritParams sortTableR
#' @param searchString A string representing the search query that will be used to filter the table.
#' 
#' @return Invisible NULL.
#' 
#' @examples
#' \dontrun{
#'   searchTableR("myTableID", "searchQuery")
#' }
#' 
#' @seealso 
#' \code{\link[shiny]{session}}
#' \code{\link[shiny]{getDefaultReactiveDomain}}
#' 
#' @export
searchTableR <- function(table_id, searchString, session = getDefaultReactiveDomain()) {
  session$sendCustomMessage('searchTableMessage'
                            , list(table_id = table_id 
                                   , value = searchString))
}


#' Shiny Table UI
#'
#' This function creates a user interface for a Shiny table with optional features like search, sort, and add/remove rows being implemented in Shiny.
#'
#' @param id A string specifying the ID for the UI element.
#' @param shiny_search A logical value indicating whether the search functionality should be enabled via shiny. Defaults to TRUE.
#' @param shiny_sort A logical value indicating whether the sorting functionality should be enabled via shiny. Defaults to TRUE.
#' @param add_remove A character vector with two elements: "Add Row" and "Remove Row". Indicates the labels for the add and remove buttons. Can be NULL to disable the add/remove feature. Defaults to c("Add Row", "Remove Row").
#' 
#' @return A tag list that can be added to a Shiny UI.
#'
#' @examples
#' \dontrun{
#'   ui <- fluidPage(
#'     shinyTableUI("tableID", shiny_search = TRUE, shiny_sort = TRUE, add_remove = c("Add Row", "Remove Row"))
#'   )
#' }
#' 
#' @seealso 
#' \code{\link[shiny]{tagList}}
#' \code{\link[shiny]{textInput}}
#' \code{\link[shiny]{uiOutput}}
#' \code{\link[shiny]{actionButton}}
#' 
#' @export
shinyTableUI <- function(id
                         , shiny_search = TRUE
                         , shiny_sort = TRUE
                         , add_remove = c("Add Row", "Remove Row")
) {
  ns <- NS(id)
  tagList(
    # If shiny_search is TRUE, add a text input field for search functionality
    if (shiny_search) 
      textInput(ns("search"), "Search", value = "", width = "100%"),
    
    # If shiny_sort is TRUE or non-null, add a sorting script and UI output for sorting functionality
    if (!is.null(shiny_sort) | isTRUE(shiny_sort)) 
      tagList(tags$script(sortTable), 
              uiOutput(ns("sort")) 
              ),
    # If add_remove is non-null, add buttons for adding and removing rows
    if (!is.null(add_remove)) {
      tags$div(
        style = "display:flex;justify-content:space-evenly;",
        actionButton(ns("add"), add_remove[1], icon = icon("plus"), width = "49%"),
        actionButton(ns("remove"), add_remove[2], icon = icon("minus"), width = "49%")
      )
    },
    
    # Add UI output for the table
    uiOutput(ns("table"))
  )
}

#' Shiny Table Server Module
#'
#' This module serves a Shiny table which supports a variety of functionalities including sorting, searching, 
#' adding and removing rows, etc. It also manages the user input and output in the server side of a Shiny application.
#'
#' @param id A unique ID to identify the module server instance.
#' @inheritParams shinyTable
#' @param shiny_search A parameter to define if the search functionality should be done via shiny. Should match value provided in shinyTableUI. Default is TRUE.
#' @param shiny_sort A parameter to define the sort functionality should be done via shiny. Should match value provided in shinyTableUI. Default is "asc", use NULL or false to use shinyTable sort. 
#' @param uid_cols A vector specifying the columns that contain unique ID information. Default is NULL.
#' @param mode A string specifying the mode of the module ("inputs" or "data.frame" or "both). Default is "inputs".
#'
#' @return A reactive expression containing the user inputs or the current data frame based on the specified mode.
#' 
#' @examples
#' # To use this module in a Shiny app, you would typically call shinyTableServer in the server function
#' # and shinyTableUI in the UI function. For example:
#' # server <- function(input, output, session) {
#' #   shinyTableServer("table1", reactive({ data.frame(x = 1:10, y = 11:20) }))
#' # }
#' # ui <- fluidPage(
#' #   shinyTableUI("table1")
#' # )
#' # shinyApp(ui, server)
#'
shinyTableServer = function(id 
                            , x
                            , table_id = "tab"
                            , shiny_search = TRUE
                            , shiny_sort = "asc"
                            , id_cols = NULL
                            , col_names = NULL
                            , skip_cols = NULL
                            , type_list = NULL
                            , uid_cols = NULL
                            , mode = "inputs"
) {
  moduleServer(id, function(input, output, session) {
    
    init = if(is.reactive(x)) x else reactiveVal(x)
    current = if(is.reactive(x)) x else reactiveVal(x)
    
    # Table -------------------------------------------------------------------
    
    output$table <- renderUI({
      shinyTable(init()
                 , table_id = table_id
                 , searchable = !shiny_search
                 , sortable = !is.character(shiny_sort)
                 , id_cols = id_cols
                 , col_names = col_names
                 , skip_cols = skip_cols
                 , type_list = type_list
                 , uid_cols = uid_cols
                 , ns = session$ns
      )
    })
    
    # Input Change ------------------------------------------------------------
    
    
    observeEvent(input[[table_id]], ignoreNULL = TRUE, {
      y = current()
      l = input[[table_id]]
      i = l$i; j = l$j; value = l$value
      if (is.null(value)) value = NA
      y[i][[j]] <- value
      current(y)
      # init(x)
    })
    
    # Add Row -----------------------------------------------------------------
    
    observeEvent(input$add, ignoreInit = TRUE, {
      # browser()
      init(current())
      y = init()[1,]
      classes = sapply(y, class)
      classes = sapply(classes, `[`, 1)
      f = \(x) switch(x, 
                      numeric = 0
                      , character = "-"
                      , logical = FALSE
                      , POSIXct = as.POSIXct(Sys.time()))
      y = data.frame(lapply(classes, f))
      if (!is.null(uid_cols)) {
        # browser()
        nm = names(uid_cols)
        vals = uid_cols
        for (i in seq_along(uid_cols)) {
          if (!is.null(nm[i]) && nm[i] != "") 
            y[[i]] = nm[i]
          else 
            y[[i]] = uid()
        }
      }
      init(rbind(y, init()))
      current(init())
      
      input_out(list(
        table = table_id
        , action = "add_row"
        , i = y
      ))
      
    })
    
    # Remove Row --------------------------------------------------------------
    
    observeEvent(input$remove, ignoreInit = TRUE, {
      ch = 1:nrow(current())
      names(ch) <- lapply(1:nrow(current()), \(i) {
        x = current()[i,] |> unlist() |> as.character() 
        if (!is.null(skip_cols))
          x = x[-skip_cols] 
        
        x = x |>
          stringr::str_trunc(20, side = "right") |> 
          stringr::str_pad(20, side = "left", pad = "_") |>
          paste(collapse = "|")
        
        paste(paste0("[", i, "]: "), x)
      })
      
      showModal(
        modalDialog(
          title = "Remove"
          , size = "xl"
          , tags$div(style='font-family:monospace;',
                     selectInput(session$ns("remove_choices")
                                 , label = "Rows to Remove"
                                 , choices = ch
                                 , multiple = TRUE
                                 , selectize = FALSE
                                 , size = min(15, nrow(current()))
                                 , width = "100%")
          )
          , footer = actionButton(session$ns("remove_submit"), "Submit", icon = icon("close"))
          , easyClose = TRUE
        )
      )
    })
    
    observeEvent(input$remove_submit, ignoreInit = TRUE, {
      if (is.null(input$remove_choices) || length(input$remove_choices) == 0)
        return()
      
      x = current()
      i = as.numeric(input$remove_choices)
      x = x[!i,]
      
      input_out(list(
        table = table_id
        , action = "remove_row"
        , i = current()[i,]
      ))
      
      current(x)
      init(x)
      
      removeModal()
    })
    
    # Search ------------------------------------------------------------------
    
    
    observeEvent(input$search, ignoreInit = TRUE, {
      searchTableR(session$ns(table_id), input$search)
    })
    
    # Sort --------------------------------------------------------------------
    
    output$sort <- renderUI({
      
      original_names = names(init())
      if (!is.null(col_names)) {
        for (i in seq_along(original_names)) {
          I = which(col_names == original_names[i])
          if (length(I) == 1)
            original_names[i] <- names(col_names)[I]
        }
      }
      
      l = 1:length(original_names); names(l) = original_names
      
      if (!is.null(skip_cols)) l = l[-skip_cols]
      
      selectInput(session$ns("sort"), "Sort By", l, width = "100%", multiple = TRUE, selected = l[1])
      
    })
    
    
    observeEvent(input$sort, {
      sort_ = if (is.null(input$sort)) 0 else input$sort
      sortDict = as.list(rep(shiny_sort, max(1, length(sort_))))
      names(sortDict) = sort_
      sortTableR(session$ns(table_id), sortDict)
    })
    
    # Return ------------------------------------------------------------------
    
    input_out <- reactiveVal(list())
    
    observeEvent(input[[table_id]], ignoreNULL = TRUE, {
      x = input[[table_id]]
      if (is.null(x)) return("nothing")
      x$table = gsub(paste0(id, "-"), "", x$table)
      input_out(x)
    })
    
    return_out <- reactive({
      df = current()
      if (mode == "inputs") {
        input_out()
      } else if (mode == "data.frame") {
        current()
      } else if (mode == "both") {
        list(inp = input_out(), df = df
             , current = current, init = init)
      }
    })
    
    return(return_out)
    
  }) # end module
}

# end server --------------------------------------------------------------


run_test <- function(mode = "both", options = "test.mode") {
  ui <- fluidPage(
    shinyTableUI("module_id", shiny_sort = TRUE, shiny_search = TRUE)
    , verbatimTextOutput("main_console")
  )
  
  server <- function(input, output, session) {
    y = mtcars[1:5, 1:2]; y$newcol = c(TRUE, FALSE, TRUE, FALSE, TRUE); y$datetime = as.POSIXct(Sys.time())
    y = cbind(list("name" = c("A", "C", "B", "B", "C")), y)
    x = shinyTableServer("module_id"
                         , y
                         , table_id = "tab"
                         , shiny_sort = "asc"
                         , shiny_search = TRUE
                         , col_names = c("MPG" = "mpg")
    )
    output$main_console <- renderPrint(x())
  }
  
  shiny::shinyApp(ui, server)
}

if (interactive()) run_test(mode = "inputs")
