function sortTable(tableId, sortDict) {
  // Step 1: Select the table and the tbody
  const table = document.getElementById(tableId);
  const tbody = table.querySelector('tbody');
  
  // Step 2: Get all rows as an array
  const rows = Array.from(tbody.querySelectorAll('tr'));
  
  // Step 3: Sort rows based on the sortDict
  rows.sort((rowA, rowB) => {
    for (const [colIndex, order] of Object.entries(sortDict)) {
      const cellA = rowA.querySelector(`td[j='${colIndex}']`);
      const cellB = rowB.querySelector(`td[j='${colIndex}']`);
      
      const inputA = cellA.querySelector('input');
      const inputB = cellB.querySelector('input');
      
      const valA = inputA ? inputA.value : cellA.textContent;
      const valB = inputB ? inputB.value : cellB.textContent;
      
      if (valA !== valB) {
        if (order === 'asc') {
          return valA < valB ? -1 : 1;
        } else {
          return valA > valB ? -1 : 1;
        }
      }
    }
    
    return 0;
  });
  
  // Step 4: Replace old rows with the sorted rows
  rows.forEach(row => tbody.appendChild(row));
}

Shiny.addCustomMessageHandler('sortTableMessage', function(message) {
  sortTable(message.tableId, message.sortDict);
});