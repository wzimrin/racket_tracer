
function showTree(traceNode, displayWhere) {
  var text = displayWhere.textContent;
  var class = displayWhere.getAttribute("class");
  var onclick = displayWhere.onclick;
  displayWhere.textContent = '';
  displayWhere.onclick = null;
  theTable = document.createElement('table');
  bgColor = displayWhere.getAttribute('class');
  //console.log(theTable.getAttribute('class'));
  //theTable.setAttribute('class', 'background1'); 
    //console.log(theTable.getAttribute('class'));
  console.log(displayWhere.getAttribute('class'));
  //theTable.border="1";
  displayWhere.appendChild(theTable);

  upperTR = document.createElement('tr');
  upperTD = document.createElement('td');
  upperTD.textContent = traceNode.formals + ' => ' + traceNode.result;

  upperTD.setAttribute('class', bgColor);
  upperTD.colSpan = traceNode.children.length;

  delButton = document.createElement('td');
  delButton.setAttribute('class', 'delButton');
  delButton.textContent = ' X ';
  delButton.rowSpan = 2;
  delButton.onclick =
	  (function(c, n) {
	    return function(evt) {
              n.setAttribute("class",class);
              n.textContent = text;
              n.onclick = onclick;
		evt.stopPropagation();
	    }})(traceNode, displayWhere);
  upperTR.appendChild(delButton);

  upperTR.appendChild(upperTD);

  theTable.appendChild(upperTR);
  
  actualsTR = document.createElement('tr');
  actualsTD = document.createElement('td');
  actualsTD.textContent = traceNode.actuals;
  
  actualsTD.setAttribute('class', bgColor);
  actualsTD.colSpan = traceNode.children.length;
  
  actualsTR.appendChild(actualsTD);
  theTable.appendChild(actualsTR);
  
  

  lowerTR = document.createElement('tr');
  //lowerTR.appendChild(document.createElement('td'));
  for (i = 0; i < traceNode.children.length; i++) {
    child = traceNode.children[i];
    newDisplay = document.createElement('td');
    newDisplay.setAttribute('class', 'shrunkenCall');
    if (displayWhere.getAttribute('class') == 'background1')
	  newDisplay.setAttribute('class', 'background2');
    else if (displayWhere.getAttribute('class') == 'background2')
	  newDisplay.setAttribute('class', 'background1');


    //newDisplay.setAttribute(
    newDisplay.textContent = traceNode.children[i].formals;
    newDisplay.onclick = 
      (function(c, n) {
        return function () {
                 showTree(c, n)}})(traceNode.children[i], newDisplay);
    lowerTR.appendChild(newDisplay);
  }
  theTable.appendChild(lowerTR);

  displayWhere.setAttribute('class', 'expandedCall');
}

