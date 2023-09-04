function sort_table(tableId, sortDict) {
  const table = document.getElementById(tableId);
  if (!table) {
    console.error("Table not found");
    return;
  }

  const tbody = table.querySelector('tbody');
  const rows = Array.from(tbody.querySelectorAll('tr'));

  rows.sort((rowA, rowB) => {
    for (const [colIndex, order] of Object.entries(sortDict)) {
      const index = parseInt(colIndex);
      const valA = rowA.querySelector(`td[j="${index}"] input`).value;
      const valB = rowB.querySelector(`td[j="${index}"] input`).value;

      if (valA !== valB) {
        if (order === 'asc') {
          return valA > valB ? 1 : -1;
        } else {
          return valA < valB ? 1 : -1;
        }
      }
    }
    return 0;
  });

  rows.forEach(row => tbody.appendChild(row));
}


Shiny.addCustomMessageHandler('sort_table', function(message) {
  sort_table(message.tableId, message.sortingDict)
});

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

