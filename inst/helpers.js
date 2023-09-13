

function col_apply(table_id, col, f) {
  var table_id = "#st_" + table_id;
  var xh = document.querySelectorAll(table_id + " th[j='" + col + "']");
  var xd = document.querySelectorAll(table_id + " td[j='" + col + "']");
  
  xd.forEach(f);
  xh.forEach(f);
}

function inp_apply(table_id, col, f) {
  var table_id = "#st_" + table_id;
  var xd = document.querySelectorAll(table_id + " input[j='" + col + "']");
  
  xd.forEach(f);
}

// Attach event listener to all input elements with the class "shinyTable-input"
const inputs = document.querySelectorAll(".shinyTable-input");
inputs.forEach(input => {
  input.addEventListener("change", handleInputChange);
});

