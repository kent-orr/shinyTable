function handleInputChange(event) {
  const input = event.target;
  const i = input.getAttribute("i");
  const j = input.getAttribute("j");
  const tab = input.getAttribute("table")
  const value = input.value;
  
  Shiny.setInputValue("table", tab)
  Shiny.setInputValue("i", i);
  Shiny.setInputValue("j", j);
  Shiny.setInputValue("value", value)
  
  console.log("i:", i);
  console.log("j:", j);
  console.log("value:", value);
}

// Attach event listener to all input elements with the class "shinyTable-input"
const inputs = document.querySelectorAll(".shinyTable-input");
inputs.forEach(input => {
  input.addEventListener("change", handleInputChange);
});