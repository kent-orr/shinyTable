#' Handle input change javascript
#' 
#' This is the entire tags$script(HTML(...)) object. This object is inserted after
#' the table is generated.
#' 
#' @export
js_handle_input_change <- HTML(
'function handleInputChange(event) {
      const input = event.target;
      const i = parseInt(input.getAttribute("i"));
      const j = parseInt(input.getAttribute("j"));
      const tab = input.getAttribute("table");
      
      var validity_exemptions = ["datetime-local", "date"];
      
      if (!validity_exemptions.includes(input.type)) {
       if (!input.checkValidity()) {
          input.value = null;
          return 0;
       }
      }
      
      switch(input.type) {
        case "checkbox":
        console.log(input.checked);
        var value = input.checked;
        break;
        
        case "radio":
        console.log(input.checked);
        var value = input.checked;
        break;
        
        case "date":
        var value = Math.round(input.valueAsNumber / (1000 * 60 * 60 * 24));
        if (value === "") value = NaN
        break;
        
        case "datetime-local":
        var value = Math.round(input.valueAsNumber / 1000);
        if (value === "") value = NaN
        break;
        
        default: 
        var value = input.value
      }
      
      if (typeof Shiny !== "undefined") {
        Shiny.setInputValue(tab, {i: i, j: j, value: value, table: tab}, {priority: "event"});
        };
      
      console.log(input.type);
      console.log("i:", i);
      console.log("j:", j);
      console.log("value:", value);
    }
    
    // Attach event listener to all input elements with the class "shinyTable-input"
    document.querySelectorAll(".shinyTable-input").forEach(input => {
      input.addEventListener("change", handleInputChange);
    });'
)
