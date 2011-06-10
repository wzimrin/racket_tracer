$('.shrunkenCall').live('click',function (event) {
  var target = $(event.target)
  target.data("oldClass",target.attr("class"))
  target.data("oldHTML",target.contents())
  showTree(target.data('node'),target)
})

$('.delButton').live('click',function (event) {
  var target = $(event.target)
  var parent = target.parents('.expandedCall').first()
  parent.empty()
  parent.attr("class",parent.data("oldClass"))
  parent.append(parent.data("oldHTML"))
  event.stopPropagation();
})

function element(tag) {
  return $("<"+tag+'/>')
}

function makeShrunkenCall(child,parent) {
  var newDisplay = element('div');
  newDisplay.addClass('shrunkenCall');
  newDisplay.addClass('call');
  $(newDisplay).data("node",child)
  if (parent.hasClass("background1"))
    newDisplay.addClass('background2');
  else
    newDisplay.addClass("background1")


  //newDisplay.setAttribute(
  newDisplay.text('(' + child.name + ' '
                  + child.actuals + ') => '+ child.result);

  return newDisplay
}

function showTree(traceNode, displayWhere) {
  displayWhere = $(displayWhere)
  displayWhere.empty()
  
  theTable = element('table');
  displayWhere.append(theTable);

  upperTR = element('tr');
  actualsTR = element('tr');

  delTD = element('td')
  delTD.attr("rowspan",2)
  delTD.addClass('delButton');
  delButton = element('div');
  delButton.text(' X ');
  delButton.attr("rowspan",2);
  delTD.append(delButton);
  upperTR.append(delTD)
  
  nameTD = element('td')
  nameTD.attr("rowspan",2)
  nameTD.text("("+traceNode.name)
  nameTD.addClass("name")
  upperTR.append(nameTD)

  for (i = 0; i < traceNode.formals.length; i++) {
    formalTD = element('td')
    formalTD.text(traceNode.formals[i])
    formalTD.addClass("arg")
    upperTR.append(formalTD)

    actualTD = element('td')
    actualTD.text(traceNode.actuals[i])
    actualTD.addClass("arg")
    actualsTR.append(actualTD)
  }

  closeTD = element('td')
  closeTD.attr("rowspan",2)
  closeTD.text(") => "+traceNode.result)
  closeTD.addClass("close")
  upperTR.append(closeTD)

  theTable.append(upperTR);
  theTable.append(actualsTR);
  theTable.addClass("callTable")
  
  lowerTable = element('table')
  lowerTable.addClass("childTable")
  lowerRow = element('tr');
  lowerTable.append(lowerRow)
  for (i = 0; i < traceNode.children.length; i++) {
    shrunkDiv = makeShrunkenCall(traceNode.children[i],displayWhere);
    cell = element('td')
    cell.append(shrunkDiv)
    lowerRow.append(cell);
  }
  displayWhere.append(lowerTable);
  
  displayWhere.removeClass('shrunkenCall');
  displayWhere.addClass('expandedCall');
}

$(document).ready(function () {
  var div = $("#tracer")
  var child = makeShrunkenCall(theTrace,$(document.body))
  div.append(child)
  child.trigger('click')
})
