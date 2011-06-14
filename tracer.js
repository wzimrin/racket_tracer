$('.shrunkenCall').live('click',function (event) {
  var target = $(this)
  target.data("oldClass",target.attr("class"))
  target.data("oldHTML",target.contents())

  target.data('node').expanded = true
  showTree(target.data('node'),target)
})

$('.delButton').live('click',function (event) {
  var target = $(event.target)
  var parent = target.parents('.expandedCall').first()
  parent.data('node').expanded = false
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
  var row = element('tr');

  funcName = element('td'); funcName.addClass('name');
  funcName.text(child.name); row.append(funcName);

  for (j = 0; j < child.actuals.length; j++) {
    var arg = element('td');
    if (child.actualsExpanded == undefined ||
        child.actualsExpanded[j] == false ||
        child.actualsExpanded[j] == undefined)
   	 arg.text(child.actualsShort[j]);
    else if(child.actualsExpanded[j] == true) 
	    arg.text(child.actuals[j])
    arg.data("type", ["actualsExpanded", j])
    row.append(arg)}

  arrow = element('td'); arrow.addClass('arrow'); 
  arrow.text(' => '); row.append(arrow);

  result = element('td');
  result.text(child.resultShort);
  row.append(result);

  table.append(row);
  newDisplay.append(table);
  
  return newDisplay
}

function makeArg(arg,otherForm, type, node) {
  var TD = element('td')
  TD.addClass("arg")
  
  var div = element("div")
  div.text(arg)
  if (arg != otherForm) {
    div.addClass("expandable")
    div.data("otherForm",otherForm)
    div.data("type", type)
    div.data("node", node)
  }
  TD.append(div)
  return TD
}

function showTree(traceNode, displayWhere) {
  displayWhere = $(displayWhere)
  displayWhere.empty()
  
  var upperTable = element('table');
  displayWhere.append(upperTable);

  var upperTR = element('tr');
  actualsTR = element('tr');

  var delTD = element('td')
  delTD.attr("rowspan",2)
  delTD.addClass('delButton');
  var delButton = element('div');
  delButton.text(' X ');
  delButton.attr("rowspan",2);
  delTD.append(delButton);
  upperTR.append(delTD)
  var nameTD = element('td')
  nameTD.attr("rowspan",2)
  nameTD.text(traceNode.name)
  nameTD.addClass("name")
  upperTR.append(nameTD)

  for (var i = 0; i < traceNode.formals.length; i++) {
    if(traceNode.formalsExpanded == undefined 
		    || traceNode.formalsExpanded[i] == undefined 
		    || traceNode.formalsExpanded[i] == false)
	upperTR.append(makeArg(traceNode.formalsShort[i],traceNode.formals[i], ["formalsExpanded", i], traceNode))
    else if(traceNode.formalsExpanded[i] == true)
    	 upperTR.append(makeArg(traceNode.formals[i], traceNode.formsShort[i], ["formalsExpanded", i], traceNode))
    
    if(traceNode.actualsExpanded == undefined 
		    || traceNode.actualsExpanded[i] == undefined 
		    || traceNode.actualsExpanded[i] == false)
	actualsTR.append(makeArg(traceNode.actualsShort[i],traceNode.actuals[i], ["actualsExpanded", i], traceNode))
    else if(traceNode.actualsExpanded[i] == true)
	actualsTR.append(makeArg(traceNode.actuals[i], traceNode.actualsShort[i], ["actualsExpanded", i], traceNode))
       	  }

  var arrow = element('td')
  arrow.attr("rowspan",2)
  arrow.text("=>")
  arrow.addClass("arrow")
  upperTR.append(arrow)

  var resultTD = element('td')
  resultTD.attr("rowspan",2)
  resultTD.addClass("result")
  if(traceNode.result != traceNode.resultShort) {
  	if (traceNode.resultExpanded == undefined || traceNode.resultExpanded[0] == undefined || traceNode.resultExpanded[0] == false) {
		  resultTD.text(traceNode.resultShort)
	  	resultTD.data("otherForm", traceNode.result)}
  	else if (traceNode.resultExpanded[0] == true) {
		  resultTD.text(traceNode.result)
 	  	resultTD.data("otherForm",traceNode.resultShort)}
  	resultTD.data("type", ["resultExpanded", 0])
  	resultTD.data("node", traceNode)
  	resultTD.addClass("expandable")
  }
  else if(traceNode.result == traceNode.resultShort)
	  resultTD.text(traceNode.result)
      
    
  upperTR.append(resultTD)

  upperTable.append(upperTR);
  upperTable.append(actualsTR);
  upperTable.addClass("callTable")
  
  var lowerTable = element('table')
  lowerTable.addClass("childTable")
  var lowerRow = element('tr');
  lowerTable.append(lowerRow)
  
  var shrunkenCalls = []	  
  
  for (var i = 0; i < traceNode.children.length; i++) {
    cell = element('td')
    shrunkDiv = makeShrunkenCall(traceNode.children[i],displayWhere);
    cell.append(shrunkDiv)
    
    lowerRow.append(cell);
    shrunkenCalls[i] = shrunkDiv;
  }
  displayWhere.append(lowerTable);
  
  displayWhere.removeClass('shrunkenCall');
  displayWhere.addClass('expandedCall');

  for (var i = 0; i < traceNode.children.length; i++) {
    if (traceNode.children[i].expanded == true) {
      shrunkenCalls[i].trigger('click')
    }
  }
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
    li.addClass("other")
    ul.append(li)
  }
  first.trigger('click')
})

$('ul.tabs li.other').live('click', function (event) {
  target = $(this)
  var div = $("#tracer")
  div.empty()
  var child = makeShrunkenCall(theTrace.children[target.data("child")],$(document.body))
  div.append(child)
  child.trigger('click')
  var oldPicked = $("ul.tabs li.picked")
  oldPicked.removeClass("picked")
  oldPicked.addClass("other")
  target.addClass("picked")
  target.removeClass("other")
})

$(".expandable").live("click", function (event) {
  target = $(this)
  var newText = target.data("otherForm")
  target.data("otherForm",target.text())
  var node = target.data("node");
  var path = target.data("type")
  var field = path[0]
  var index = path[1]

  //If true, was expanded, want to make unexpanded
  //Default is unexpanded, so if undefined and clicked, want to expand
  if(node[field] == undefined) {
 	node[field] = [];
	node[field][index] = true }
  else if (node[field][index] == undefined || node[field][index] == false) {
  	node[field][index] = true}
  else
  	node[field][index] = false;

  target.text(newText)
})

/*
function find(object,lst) {
  var obj = object
  for (var i = 0; i < lst.length; i++) {
    obj = obj[lst[i]]
  }
  return obj
}
*/
