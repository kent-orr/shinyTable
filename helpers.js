function col_apply(table_id, col, f) {
  var table_id = "#st_" + table_id;
  var xh = document.querySelectorAll(table_id + " th[j='" + col + "']");
  var xd = document.querySelectorAll(table_id + " td[j='" + col + "']");
  
  xd.forEach(f);
  xh.forEach(f);
}

function handleInputChange(event) {
  const input = event.target;
  const i = input.getAttribute("i");
  const j = input.getAttribute("j");
  const tab = input.getAttribute("table")
  const value = input.value;
  
  Shiny.setInputValue("sT_" + tab, {i: i, j: j, value: value, table: tab})
  
  console.log("i:", i);
  console.log("j:", j);
  console.log("value:", value);
}

// Attach event listener to all input elements with the class "shinyTable-input"
const inputs = document.querySelectorAll(".shinyTable-input");
inputs.forEach(input => {
  input.addEventListener("change", handleInputChange);
});

