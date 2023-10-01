function searchTable(x) {
  var input = document.getElementById(x.id)
  var filter, table, tr, td, i, j, txtValue;
  filter = input.value.toUpperCase();
  table = document.getElementById(input.id.replace('-search', ''));
  tr = table.getElementsByTagName('tr');

  if (filter === "") {
    for (i = 1; i < tr.length; i++) {
      if (tr[i].getAttribute('shinyTable-hidden') !== 'true') {
        tr[i].style.display = '';
      }
    }
  } else {
    for (i = 1; i < tr.length; i++) {
      tr[i].style.display = 'none';
      td = tr[i].getElementsByTagName('td');
      for (j = 0; j < td.length; j++) {
        if (td[j]) {
          txtValue = td[j].textContent || td[j].innerText;
          if (td[j].querySelector('input')) {
            txtValue = td[j].querySelector('input').value;
          }
          if (txtValue.toUpperCase().indexOf(filter) > -1) {
            if (tr[i].getAttribute('shinyTable-hidden') !== 'true') {
              tr[i].style.display = '';
            }
            break;
          }
        }
      }
    }
  }
}

function searchTableMessage(x) {
  var filter, table, tr, td, i, j, txtValue;
  filter = x.value.toUpperCase();
  table = document.getElementById(x.tableId);
  tr = table.getElementsByTagName('tr');

  if (filter === "") {
    for (i = 1; i < tr.length; i++) {
      if (tr[i].getAttribute('shinyTable-hidden') !== 'true') {
        tr[i].style.display = '';
      }
    }
  } else {
    for (i = 1; i < tr.length; i++) {
      tr[i].style.display = 'none';
      td = tr[i].getElementsByTagName('td');
      for (j = 0; j < td.length; j++) {
        if (td[j]) {
          txtValue = td[j].textContent || td[j].innerText;
          if (td[j].querySelector('input')) {
            txtValue = td[j].querySelector('input').value;
          }
          if (txtValue.toUpperCase().indexOf(filter) > -1) {
            if (tr[i].getAttribute('shinyTable-hidden') !== 'true') {
              tr[i].style.display = '';
            }
            break;
          }
        }
      }
    }
  }
}

Shiny.addCustomMessageHandler('searchTableMessage', function(message) {
  searchTableMessage(message)
});