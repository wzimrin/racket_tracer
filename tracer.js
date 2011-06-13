$('.shrunkenCall').live('click',function (event) {
  var target = $(this)
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
  
  table = element('table')
 /* table.addClass('childTable')*/
  var lowerRow = element('tr');

  funcName = element('td')
  //var funcName = element('div');
  funcName.text(child.name + ' ' + child.actuals);
  lowerRow.append(funcName);
  /*cell.append(funcName);
  lowerRow.append(cell);*/

  /*var arrow = element('div');*/
  arrow = element('td');
  arrow.text(' => ');
  /*cell.append(arrow);*/
  lowerRow.append(arrow);

  /*var result = element('div');*/
  result = element('td');
  result.text(child.result);
  /*cell.append(result);*/
  lowerRow.append(result);

  table.append(lowerRow);
  newDisplay.append(table);
  /*
  //newDisplay.setAttribute(
  newDisplay.text('(' + child.name + ' '
                  + child.actuals + ') => '+ child.result);
*/
  return newDisplay
}

function makeArg(arg) {
  formalTD = element('td')
  formalTD.addClass("arg")
  
  formalDiv = element("div")
  formalDiv.text(arg)
  formalTD.append(formalDiv)
  return formalTD
}

function showTree(traceNode, displayWhere) {
  displayWhere = $(displayWhere)
  displayWhere.empty()
  
  upperTable = element('table');
  displayWhere.append(upperTable);

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
    upperTR.append(makeArg(traceNode.formals[i]))

    actualsTR.append(makeArg(traceNode.actuals[i]))
  }

  closeTD = element('td')
  closeTD.attr("rowspan",2)
  closeTD.text(") =>")
  closeTD.addClass("close")
  upperTR.append(closeTD)

  resultTD = element('td')
  resultTD.attr("rowspan",2)
  resultTD.text(traceNode.result)
  resultTD.addClass("result")
  upperTR.append(resultTD)

  upperTable.append(upperTR);
  upperTable.append(actualsTR);
  upperTable.addClass("callTable")
  
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
  var div = $("#tabbar")
  var ul = element("ul")
  ul.addClass("tabs")
  div.append(ul)
  var first = false
  for (var i = 0; i < theTrace.children.length; i++) {
    var li = element("li")
    if (!first)
      first = li
    li.data("child",i)
    li.text(theTrace.children[i].name)
    ul.append(li)
  }
  first.trigger('click')
})

$('ul.tabs li').live('click', function (event) {
  target = $(this)
  var div = $("#tracer")
  div.empty()
  var child = makeShrunkenCall(theTrace.children[target.data("child")],$(document.body))
  div.append(child)
  child.trigger('click')
  $("ul.tabs li.picked").removeClass("picked")
  target.addClass("picked")
})
