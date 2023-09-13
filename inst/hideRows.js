function hideRows(table_id, hideIndex) {
  var table = document.getElementById(table_id)
  rows = table.getElementsByTagName('tr');
  
  for (i = 1; i < rows.length; i++) {
    if (hideIndex.includes(i)) {
      rows[i].style.display = 'none'
    } else {
      rows[i].style.display = '';
    }
  }
  
}

Shiny.addCustomMessageHandler('hideRows', function(message) {
  hideRows(message.table_id, message.hideIndex)
})