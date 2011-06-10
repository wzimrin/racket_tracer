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
  $(newDisplay).data("node",child)
  if (parent.hasClass("background1"))
    newDisplay.addClass('background2');
  else if (parent.hasClass("background2"))
    newDisplay.addClass("background1")


  //newDisplay.setAttribute(
  newDisplay.text('(' + child.name + ' '
                  + child.actuals + ')');

  return newDisplay
}

function showTree(traceNode, displayWhere) {
  displayWhere = $(displayWhere)
  displayWhere.empty()
  theTable = element('table');
  displayWhere.append(theTable);

  upperTR = element('tr');
  upperTD = element('td');
  upperTD.text('(' + traceNode.name + ' ' +
               traceNode.formals + ') => ' + traceNode.result);

  upperTD.addClass('bgColor');

  delButton = element('td');
  delButton.addClass('delButton');
  delButton.text(' X ');
  delButton.attr("rowspan",2);
  upperTR.append(delButton);

  upperTR.append(upperTD);

  theTable.append(upperTR);
  
  actualsTR = element("tr")
  actualsTD = element('td');
  actualsTD.text('(' + traceNode.name + ' ' + traceNode.actuals + ')');
  
  actualsTR.append(actualsTD);
  theTable.append(actualsTR);
  
  

  lowerTR = element('tr');
  lowerTD = element('td');
  lowerTD.attr("colspan",2)
  for (i = 0; i < traceNode.children.length; i++) {
    var shrunkDiv = makeShrunkenCall(traceNode.children[i],displayWhere);
    lowerTD.append(shrunkDiv);
  }
  lowerTR.append(lowerTD)
  theTable.append(lowerTR);
  
  displayWhere.removeClass('shrunkenCall');
  displayWhere.addClass('expandedCall');
}

$(document).ready(function () {
  var div = $("#tracer")
  var child = makeShrunkenCall(theTrace,div)
  div.append(child)
  child.trigger('click')
})
