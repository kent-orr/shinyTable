
#' Custom Output for Shiny Tables
#'
#' Generate a custom shiny table output element.
#'
#' @param id A character string indicating the ID of the output div.
#' @return A Shiny tagList containing the custom table div and JavaScript dependencies.
#' @export
shinyTableOutput <- function(id) {
  
  # Create a div with the given ID and class "shiny-table-output"
  div <- tags$div(
    id = id, class = "shiny-table-output"
  )
  
  # Combine the div and required scripts into a tag list
  tagList(
    div,
    # Include shinyTable included scripts, ensuring they're included only once
    shiny::singleton(tags$script(hideRows)),
    shiny::singleton(tags$script(searchTable)),
    shiny::singleton(tags$script(sortTable)),
    shiny::singleton(tags$script("
      var shinyTableBinding = new Shiny.OutputBinding();
      $.extend(shinyTableBinding, {
        find: function(scope) {
          return $(scope).find('.shiny-table-output');
        },
        getID: function(el) {
          return el['data-input-id'] || el.id;
        },
        renderValue: function(el, data) {
          console.log(el);
          console.log(data);
          el.innerHTML = data.table_HTML;
          eval(data.inputScript);
        }
      });
      Shiny.outputBindings.register(shinyTableBinding, 'kent.shinyTableBinding');
    "))
  )
}

#' Custom Render Function for Shiny Tables
#'
#' Render a shiny table using a custom output element.
#'
#' @param expr An expression that returns the table to be rendered.
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This is useful if you want to save an expression in a variable.
#' @return A function that returns a list containing the HTML of the table and any additional input scripts.
#' @export
renderShinyTable <- function(expr, env = parent.frame(), quoted = FALSE) {
  
  # Convert the expression to a function
  func <- shiny::exprToFunction(expr, env, quoted)
  
  # Evaluate the function to get the table element x
  x <- func()
  
  # Return a function that renders the table and runs the inputScript
  return(\() {
    list(
      table_HTML = htmltools::doRenderTags(x[which(sapply(x, \(y) y[["name"]]) == "table")]),
      inputScript = inputChange
    )
  })
}

