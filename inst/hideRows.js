function hideRows(table_id, hideIndex) {
  if (typeof(hideIndex) === "number") { hideIndex = [hideIndex] }
  var table = document.getElementById(table_id)
  rows = table.getElementsByTagName('tr');
  
  for (i = 1; i < rows.length; i++) {
    if (hideIndex.includes(i)) {
      rows[i].style.display = 'none';
      rows[i].setAttribute('shinyTable-hidden', true);
      
    } else {
      rows[i].style.display = '';
      rows[i].setAttribute('shinyTable-hidden', false);
    }
  }
  
}

Shiny.addCustomMessageHandler('hideRowsMessage', function(message) {
  hideRows(message.table_id, message.hideIndex)
})