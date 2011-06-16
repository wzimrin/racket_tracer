//William Zimrin and Jeanette Miranda
//tracer.js	6/02/2011

function element(tag) {
    return $("<"+tag+'/>')
}

//Creates a collapsed call -- the function, actuals and results in their correct
//expanded or collapsed form. Will alternate the colors of the background to 
//layer child calls on top of parent calls
function makeCollapsedCall(child,parent) {
    var newDisplay = element('div')
    newDisplay.addClass('collapsedCall')
    newDisplay.addClass('call')
    $(newDisplay).data("node",child)

    //Layer background colors so that child is opposite of its parent
    if (parent.hasClass("background1"))
        newDisplay.addClass('background2')
    else
        newDisplay.addClass("background1")
  
    table = element('table')
    var row = element('tr')

    //expand button
    var expTD = element('td')
    expTD.attr("rowspan",2)
    expTD.addClass('expandButton')
    expTD.addClass("button")
    var expButton = element('div')
    expButton.text(' + ')
    expTD.append(expButton)
    row.append(expTD)

    //Function Name
    funcName = element('td'); 
    funcName.addClass('name');
    funcName.text(child.name); 
    row.append(funcName);

    //Actuals (choose expanded or unexpanded form based on boolean stored in node)
    for (j = 0; j < child.actuals.length; j++) 
    {
        var arg = element('td')
        console.log(child.actualsExpanded)
        console.log(child)
        if (child.actualsExpanded == undefined ||
            child.actualsExpanded[j] == false ||
            child.actualsExpanded[j] == undefined){
            console.log('actualsExpanded was false or undefined for actual ' + j)
            arg.text(child.actualsShort[j])
        }        
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
function makeArg(formShort, formFull, type, node) {
    var TD = element('td')
    TD.addClass("arg")

    var div = element("div")
    div.text(arg)
    if (form1 != form2) {
        div.addClass("expandable")
        div.data("otherForm",otherForm)
        div.data("type", type)
        div.data("node", node)
      }
  TD.append(div)
  return TD
}

//Makes the call table: function name, formals, actuals and result in table form
function makeCallTable(node) {
    var topRow = element('tr')
    var bottomRow = element('tr')

    //Function name
    var nameTD = element('td')
    nameTD.attr("rowspan",2)
    nameTD.text(node.name)
    nameTD.addClass("name")
    row.append(nameTD)

    //Formals and actuals
    for (var i = 0; i < node.formals.length; i++) {
    //Display in collapsed form if formalsExpanded is undefined or false
        if(node.formalsExpanded == undefined 
                || node.formalsExpanded[i] == undefined 
                || node.formalsExpanded[i] == false) {
            topRow.append(makeArg(node.formalsShort[i],node.formals[i], ["formalsExpanded", i], node))
        }
        else if(node.formalsExpanded[i] == true) {
             topRow.append(makeArg(node.formals[i], node.formsShort[i], ["formalsExpanded", i], node))
        }
        
        //Display in collapsed form if actualsExpanded is undefined or false
        if(node.actualsExpanded == undefined 
                || node.actualsExpanded[i] == undefined 
                || node.actualsExpanded[i] == false) {
            bottomRow.append(makeArg(node.actualsShort[i],node.actuals[i], ["actualsExpanded", i], node))
        }
        else if(node.actualsExpanded[i] == true) {
            bottomRow.append(makeArg(node.actuals[i], node.actualsShort[i], ["actualsExpanded", i], node))
        }
    }

    //Arrow
    var arrow = element('td')
    arrow.attr("rowspan",2)
    arrow.text("=>")
    arrow.addClass("arrow")
    topRow.append(arrow)

    //Result
    var resultTD = element('td')
    resultTD.attr("rowspan",2)
    resultTD.addClass("result")
  
    if(traceNode.result != traceNode.resultShort) {
    	if (traceNode.resultExpanded == undefined || traceNode.resultExpanded[0] == undefined || traceNode.resultExpanded[0] == false) {
            resultTD.text(traceNode.resultShort)
            resultTD.data("otherForm", traceNode.result)
        }
  	    else if (traceNode.resultExpanded[0] == true) {
            resultTD.text(traceNode.result)
            resultTD.data("otherForm",traceNode.resultShort)
        }
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


    var lowerTable = element('table')
    lowerTable.addClass("childTable")
    var lowerRow = element('tr');
    lowerTable.append(lowerRow)

    /*for(i = 0; i < traceNode.children.length; i++) {
        var cell = element('td')
        var div = element('div')
        expandedChild = makeExpandedCall(traceNode.children[i],div);
        cell.append(div)
        lowerRow.append(cell)
    }
    displayWhere.append(lowerTable)
    displayWhere.removeClass('collapsedCall')
    displayWhere.addClass('expandedCall')*/

    
    //Add collapsedCalls and expand if necessary
    var collapsedCalls = []	  

    for (var i = 0; i < traceNode.children.length; i++) {
        cell = element('td')
        collapsedDiv = makeCollapsedCall(traceNode.children[i],displayWhere);
        cell.append(collapsedDiv)

        lowerRow.append(cell);
        collapsedCalls[i] = collapsedDiv
    }

    displayWhere.append(lowerTable)
    displayWhere.removeClass('collapsedCall')
    displayWhere.addClass('expandedCall')

    for (var i = 0; i < traceNode.children.length; i++) 
    {
        //if (traceNode.children[i].expanded == true)
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
})

// ----------------------------------------------------------------------------
//                                      EVENTS
// ----------------------------------------------------------------------------

offButton = true

$('.delButton div').live('mouseenter',function(event) {
    console.log("enter collapse")
    if(offButton == true) {
        $(this).trigger('click')
        offButton = false
    }
}).live('mouseleave',function(event) {
    console.log("leave collapse")
    offButton = true
})

$('.expandButton div').live("mouseenter",function (event) {
    console.log("enter expand")
    if(offButton == true) {
        $(this).parents(".collapsedCall").first().trigger("click")
        offButton = false
    }
}).live('mouseleave',function(event) {
    console.log("leave expand")
    offButton = true
})

$(".expandable").live("mouseenter",function (event) {
    $(this).addClass("hover")
}).live("mouseleave",function (event) {
    $(this).removeClass("hover")
})

$(".other").live("mouseenter", function(event) {
    $(this).addClass("hover")
}).live("mouseleave", function(event) {
    $(this).removeClass("hover")
})

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

$('ul.tabs li.other').live('click', function (event) 
{
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
    target.removeClass("hover")
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
        node[field] = []
        node[field][index] = true
        console.log('Set ' + field + ' ' + index + ' to true')
        console.log(node)
    }
    else if (node[field][index] == undefined || node[field][index] == false) {
        node[field][index] = true
        console.log(field + ' ' + index + ' to true in else if')
    }
    else
        node[field][index] = false

    target.text(newText)
})

