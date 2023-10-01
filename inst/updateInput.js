function updateInput(tableId, i, j, value) {
  var input = document.querySelector("#" + tableId + " input[i='" + i + "'][j='" + j + "']");
  input.value = value;
  Shiny.setInputValue(tableId, {i: i, j: j, value: value, action: "value-change"}, {priority: 'event'})
  return;
}

Shiny.addCustomMessageHandler("updateInputMessage", function(message) {
  updateInput(message.tableId, message.i, message.j, message.value);
})