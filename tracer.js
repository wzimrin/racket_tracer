//William Zimrin and Jeanette Miranda
//tracer.js	6/02/2011

function element(tag) {
    return $("<"+tag+'/>')
}

function setLoc(location,value) {
    location.obj[location.location]=value
}

function getLoc(location) {
    return location.obj[location.location]
}

function makeLoc(obj,address) {
    for (var x = 0; x < address.length-1; x++) {
        if (obj[address[x]]==undefined) {
            if (typeof (address[x+1]) == "number") {
                obj[address[x]]=[]
            }
            else {
                obj[address[x]]={}
            }
        }
        obj = obj[address[x]]
    }
    return {obj:obj,location:address[x]}
}

function updateExpandable(html) {
    var expanded = getLoc(html.data("location"))
    if (expanded)
        html.text(html.data("full"))
    else
        html.text(html.data("short"))
}

function toggleExpandable(html) {
    var loc = html.data("location")
    setLoc(loc,!getLoc(loc))
    updateExpandable(html)
}

//Creates a collapsed call -- the function, actuals and results in their correct
//expanded or collapsed form. Will alternate the colors of the background to 
//layer child calls on top of parent calls
//Helper to make an argument -- used for both formals and actuals
//Will check to see if two forms are the same, and if they are, will not make
//expandable/collapsible
function makeCell(formShort, formFull, location, cssClass) {
    var TD = element('td')
    TD.addClass(cssClass)

    var div = element("div")
    if (formShort != formFull) {
        div.addClass("expandable")
        div.data("short",formShort)
        div.data("full",formFull)
        div.data("location", location)
        updateExpandable(div)
    }
    else {
        div.text(formShort)
    }
    TD.append(div)
    return TD
}

//Makes the call table: function name, formals, actuals and result in table form
function makeCallTable(node) {
    var topRow = element('tr')
    var bottomRow = element('tr')
    var table = element("table")

    var hidable = []

    var button = element("td")
    button.attr("rowspan",2)
    var buttonDiv = element("div")
    buttonDiv.text("-")
    button.append(buttonDiv)
    button.addClass("button")
    topRow.append(button)

    //Function name
    var nameTD = element('td')
    nameTD.attr("rowspan",2)
    nameTD.text(node.name)
    nameTD.addClass("name")
    topRow.append(nameTD)

    //Formals and actuals
    for (var i = 0; i < node.formals.length; i++) {
        //Display in collapsed form if formalsExpanded is undefined or false
        var formal = makeCell(node.formalsShort[i],node.formals[i],
                              makeLoc(node,["formalsExpanded",i]),
                              "arg")
        topRow.append(formal)
        hidable.push(formal)
        
        //Display in collapsed form if actualsExpanded is undefined or false
        var actual = makeCell(node.actualsShort[i],node.actuals[i],
                              makeLoc(node,["actualsExpanded",i]),
                              "arg")
        bottomRow.append(actual)
        hidable.push(actual)
    }

    //Arrow
    var arrow = element('td')
    arrow.attr("rowspan",2)
    arrow.text("=>")
    arrow.addClass("arrow")
    hidable.push(arrow)
    topRow.append(arrow)

    //Result
    resultTD = makeCell(node.resultShort,node.result,
                        makeLoc(node,["resultExpanded"]),
                        "result")
    resultTD.attr("rowspan",2)
    hidable.push(resultTD)
    
    topRow.append(resultTD)

    table.append(topRow)
    table.append(bottomRow)
    table.addClass("callTable")
    return {table:table,hidable:hidable,button:buttonDiv}
}

function setHide(obj,hidden) {
    if (hidden)
        obj.hide()
    else
        obj.show()
}

function updateCall(html) {
    var expanded = getLoc(html.data("location"))
    var hidable = html.data("hidable")
    var button = html.data("button")
    for (var i = 0; i < hidable.length; i++) {
        setHide(hidable[i],!expanded)
    }
    if (expanded) {
        button.text("-")
    } else {
        button.text("+")
    }        
}

function toggleCall(html) {
    var loc = html.data("location")
    setLoc(loc,!getLoc(loc))
    updateCall(html)
}

//Makes an expandedCall: delete button, function, formals, actuals and result
//All in their appropriate expanded or unexpanded form
function makeCall(traceNode, parent) {
    parent = $(parent)

    var div = element("div")
    if (parent.hasClass("background1"))
        div.addClass("background2")
    else
        div.addClass("background1")
    div.addClass("call")

    var upperTableObj = makeCallTable(traceNode)
    var upperTable = upperTableObj.table
    var button = upperTableObj.button
    var hidable = upperTableObj.hidable

    var lowerTable = element('table')
    hidable.push(lowerTable)
    lowerTable.addClass("childTable")
    var lowerRow = element('tr');
    lowerTable.append(lowerRow)

    /*for(i = 0; i < traceNode.children.length; i++) {
        var cell = element('td')
        var div = element('div')
        expandedChild = makeCall(traceNode.children[i],div);
        cell.append(div)
        lowerRow.append(cell)
    }
    displayWhere.append(lowerTable)
    displayWhere.removeClass('collapsedCall')
    displayWhere.addClass('expandedCall')*/

    
    for (var i = 0; i < traceNode.children.length; i++) {
        var cell = element('td')
        collapsedDiv = makeCall(traceNode.children[i],div);
        cell.append(collapsedDiv)

        lowerRow.append(cell);
    }

    /*for (var i = 0; i < traceNode.children.length; i++) {
        //if (traceNode.children[i].expanded == true)
        collapsedCalls[i].trigger('click')
    }*/
    div.append(upperTable)
    div.append(lowerTable)
    div.data("location",makeLoc(traceNode,["expanded"]))
    div.data("hidable",hidable)
    div.data("button",button)
    updateCall(div)
    return div
}

$(document).ready(function () {
    var tabs = $("#tabbar")
    var bodies = $("#tracer")
    var ul = element("ul")
    ul.addClass("tabs")
    tabs.append(ul)
    var first = false
    for (var i = 0; i < theTrace.children.length; i++) {
        var li = element("li")
        if (!first)
            first = li
        li.text(theTrace.children[i].name)
        li.addClass("other")
        ul.append(li)
        var exp = makeCall(theTrace.children[i],tabs)
        exp.addClass("toplevel")
        toggleCall(exp)
        li.data("child",exp)
        bodies.append(exp)
    }
    first.trigger("click")
})

// ----------------------------------------------------------------------------
//                                      EVENTS
// ----------------------------------------------------------------------------

offButton = true


$('.button div').live('mouseenter',function(event) {
    toggleCall($(this).parents(".call").first())
})

$(".expandable").live("click",function (event) {
    toggleExpandable($(this))
})

$(".expandable").live("mouseenter",function (event) {
    $(this).addClass("hover")
}).live("mouseleave",function (event) {
    $(this).removeClass("hover")
})

$('ul.tabs li.other').live('click', function (event) {
    target = $(this)
    var div = $("#tracer")
    $(".toplevel").hide()
    var child = target.data("child")
    child.show()
    var oldPicked = $("ul.tabs li.picked")
    oldPicked.removeClass("picked")
    oldPicked.addClass("other")
    target.addClass("picked")
    target.removeClass("other")
    target.removeClass("hover")
})

$("ul.tabs li.other").live("mouseenter",function (event) {
    $(this).addClass("hover")
})

$("ul.tabs li.other").live("mouseleave",function (event) {
    $(this).removeClass("hover")
})

/*

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
    makeCall(target.data('node'),target)
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
*/

