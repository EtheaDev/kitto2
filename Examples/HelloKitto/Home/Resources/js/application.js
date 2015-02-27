// Counts the occurrences of fieldName = value
// in the specified recordset. Useful to be called
// in a group template.
function countValues(recordSet, fieldName, value) {
  result = 0;
  for (i = 0; i < recordSet.length; i++)
    if (recordSet[i].data[fieldName] == value)
      result++;
  return result;
};

function getAgeClass(age) {
  if (age >= 15) {
    return 'important-row';
  } else if (age >= 12) {
    return 'semi-important-row';
  } else
    return '';
};
