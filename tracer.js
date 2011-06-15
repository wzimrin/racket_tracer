//William Zimrin and Jeanette Miranda
//tracer.js	6/02/2011

function element(tag) {
  return $("<"+tag+'/>')
}

//Creates a collapsed call -- the function, actuals and results in their correct
//expanded or collapsed form. Will alternate the colors of the background to 
//layer child calls on top of parent calls
function makeCollapsedCall(child,parent) {
  var newDisplay = element('div');
  newDisplay.addClass('collapsedCall');
  newDisplay.addClass('call');
  $(newDisplay).data("node",child)

  //Layer background colors so that child is opposite of its parent
  if (parent.hasClass("background1"))
    newDisplay.addClass('background2');
  else
    newDisplay.addClass("background1")
  
  table = element('table')
  var row = element('tr');
 
  //expand button
  var expTD = element('td')
  expTD.attr("rowspan",2)
  expTD.addClass('expandButton');
  expTD.addClass("button")
  var expButton = element('div');
  expButton.text(' + ');
  expTD.append(expButton);
  row.append(expTD)

  //Function Name
  funcName = element('td'); 
  funcName.addClass('name');
  funcName.text(child.name); 
  row.append(funcName);

  //Actuals (choose expanded or unexpanded form based on boolean stored in node)
  for (j = 0; j < child.actuals.length; j++) 
  {
    var arg = element('td');
    if (child.actualsExpanded == undefined ||
        child.actualsExpanded[j] == false ||
        child.actualsExpanded[j] == undefined)
   	 arg.text(child.actualsShort[j]);
    else if(child.actualsExpanded[j] == true) 
	    arg.text(child.actuals[j])
    arg.data("type", ["actualsExpanded", j])
    row.append(arg)
  }

  //Arrow
  arrow = element('td')
  arrow.addClass('arrow') 
  arrow.text(' => ')
  row.append(arrow)

  //Resort -- shortened form of result for shrunkenCalls
  result = element('td')
  result.text(child.resultShort)
  row.append(result)

  table.append(row)
  newDisplay.append(table)
  
  return newDisplay
}

//Helper to make an argument -- used for both formals and actuals
//Will check to see if two forms are the same, and if they are, will not make
//expandable/collapsible
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

//Makes an expandedCall: delete button, function, formals, actuals and result
//All in their appropriate expanded or unexpanded form
function makeExpandedCall(traceNode, displayWhere) {
  displayWhere = $(displayWhere)
  displayWhere.empty()
  
  var upperTable = element('table')
  displayWhere.append(upperTable)

  var upperTR = element('tr')
  actualsTR = element('tr')

  //Delete button
  var delTD = element('td')
  delTD.attr("rowspan",2)
  delTD.addClass('delButton');
  delTD.addClass("button")
  var delButton = element('div');
  delButton.text(' - ');
  delTD.append(delButton);
  upperTR.append(delTD)

  //Function name
  var nameTD = element('td')
  nameTD.attr("rowspan",2)
  nameTD.text(traceNode.name)
  nameTD.addClass("name")
  upperTR.append(nameTD)

  //Formals and actuals
  for (var i = 0; i < traceNode.formals.length; i++) {
    //Display in collapsed form if formalsExpanded is undefined or false
    if(traceNode.formalsExpanded == undefined 
		    || traceNode.formalsExpanded[i] == undefined 
		    || traceNode.formalsExpanded[i] == false)
	upperTR.append(makeArg(traceNode.formalsShort[i],traceNode.formals[i], ["formalsExpanded", i], traceNode))
    else if(traceNode.formalsExpanded[i] == true)
    	 upperTR.append(makeArg(traceNode.formals[i], traceNode.formsShort[i], ["formalsExpanded", i], traceNode))
    
    //Display in collapsed form if actualsExpanded is undefined or false
    if(traceNode.actualsExpanded == undefined 
		    || traceNode.actualsExpanded[i] == undefined 
		    || traceNode.actualsExpanded[i] == false)
	actualsTR.append(makeArg(traceNode.actualsShort[i],traceNode.actuals[i], ["actualsExpanded", i], traceNode))
    else if(traceNode.actualsExpanded[i] == true)
	actualsTR.append(makeArg(traceNode.actuals[i], traceNode.actualsShort[i], ["actualsExpanded", i], traceNode))
       	  }

  //Arrow
  var arrow = element('td')
  arrow.attr("rowspan",2)
  arrow.text("=>")
  arrow.addClass("arrow")
  upperTR.append(arrow)

  //Result
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
  
  
  //Add collapsedCalls and expand if necessary
  var collapsedCalls = []	  
  
  for (var i = 0; i < traceNode.children.length; i++) {
    cell = element('td')
    collapsedDiv = makeCollapsedCall(traceNode.children[i],displayWhere);
    cell.append(collapsedDiv)
    
    lowerRow.append(cell);
    collapsedCalls[i] = collapsedDiv;
  }

  displayWhere.append(lowerTable);
  displayWhere.removeClass('collapsedCall');
  displayWhere.addClass('expandedCall');

  for (var i = 0; i < traceNode.children.length; i++) 
  {
    if (traceNode.children[i].expanded == true)
      collapsedCalls[i].trigger('click')
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

  
/*
$('td').hover(
  function () {
    console.log("adding hover class")
    $(this).addClass('hover');
  },
  function () {
    console.log("removing hover class")
    $(this).removeClass("hover");
  }
)*/
})

// ----------------------------------------------------------------------------
//                                      EVENTS
// ----------------------------------------------------------------------------


$('.delButton div').live('mouseenter',function(event) {
  console.log("mouse entered!")
  $(this).trigger('click')
})

$('.expandButton div').live("mouseenter",function (event) {
  console.log("mouse enteres")
  $(this).parents(".collapsedCall").first().trigger("click")
  })

$(".expandable").live("mouseenter",function (event) {
  $(this).addClass("hover")
}).live("mouseleave",function (event) {
  $(this).removeClass("hover")
})

/*$('.expandedCall').live('mouseout',function(event) {
	console.log("mouse exited")
	$(this).find('.delButton').trigger('click') })*/


//EVENT: Expands shrunkenCall (child) on click
//Stores current class and html (know what to restore to if an expanded child 
//is collapsed) and then expands the child
$('.collapsedCall').live('click',function (event) {
  var target = $(this)
  target.data("oldClass",target.attr("class"))
  target.data("oldHTML",target.contents())
  target.data('node').expanded = true
  makeExpandedCall(target.data('node'),target)
})

//EVENT: Collapses the expandedCall (child) on click
//Stores the current class and html (know what to restore to if collapsedCall 
//is expanded again) and then collapses the child
$('.delButton div').live('click',function (event) {
  var target = $(event.target)
  var parent = target.parents('.expandedCall').first()
  parent.data('node').expanded = false
  parent.empty()
  parent.attr("class",parent.data("oldClass"))
  parent.append(parent.data("oldHTML"))
  event.stopPropagation();
})

$('ul.tabs li.other').live('click', function (event) {
  target = $(this)
  var div = $("#tracer")
  div.empty()
  var child = makeCollapsedCall(theTrace.children[target.data("child")],$(document.body))
  div.append(child)
  child.trigger('click')
  var oldPicked = $("ul.tabs li.picked")
  oldPicked.removeClass("picked")
  oldPicked.addClass("other")
  target.addClass("picked")
  target.removeClass("other")
})


//EVENT: Expandable/collapsible object on click
//When an expandable or collapsible object is clicked, swap to its other form
//And update the appropriate expanded boolean denoting whether it is expanded
//or collapse
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

