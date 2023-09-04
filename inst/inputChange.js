function handleInputChange(event) {
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
        var value = input.value;
        var col_inputs = document.querySelectorAll("#st_" + tab + " input[j='" + j + "']");
        col_array = [];
        col_inputs.forEach(x => col_array.push(x.value.length));
        col_inputs.forEach(x => x.size = 3 + Math.max.apply(Math, col_array));
      }
      
      if (typeof Shiny !== "undefined") {
        Shiny.setInputValue(tab, {i: i, j: j, value: value, table: tab, action: "value_change"}, {priority: "event"});
        };
      
      console.log({i: i, j: j, value: value, table: tab, action: "value_change"})
      
    }
    
    // Attach event listener to all input elements with the class "shinyTable-input"
    document.querySelectorAll(".shinyTable-input").forEach(input => {
      input.addEventListener("change", handleInputChange);
    });