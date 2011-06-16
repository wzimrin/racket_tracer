//William Zimrin and Jeanette Miranda
//tracer.js	6/02/2011

//creates a dom element of the type tag
function element(tag) {
    return $("<"+tag+'/>')
}

//makes an expandable show the correct text (as determined by html.data("expanded"))
function updateExpandable(html) {
    var expanded = html.data("expanded")
    if (expanded)
        html.text(html.data("full"))
    else
        html.text(html.data("short"))
}

//makes an expandable expand/collapse
function toggleExpandable(html) {
    html.data("expanded",!html.data("expanded"))
    updateExpandable(html)
}

//Helper to make an callTable cell -- used for formals, actuals, and results
//Will check to see if two forms are the same, and if they are, will not make
//expandable/collapsible
//takes the short form, the full form, and the class to apply to the td
function makeCell(formShort, formFull, cssClass) {
    var TD = element('td')
    TD.addClass(cssClass)

    var div = element("div")
    if (formShort != formFull) {
        div.addClass("expandable")
        div.data("short",formShort)
        div.data("full",formFull)
        div.data("expanded", false)
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
        var formal = makeCell(node.formalsShort[i],node.formals[i],"arg")
        topRow.append(formal)
        hidable.push(formal)
        
        //Display in collapsed form if actualsExpanded is undefined or false
        var actual = makeCell(node.actualsShort[i],node.actuals[i],"arg")
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
    resultTD = makeCell(node.resultShort,node.result,"result")
    resultTD.attr("rowspan",2)
    hidable.push(resultTD)
    
    topRow.append(resultTD)

    table.append(topRow)
    table.append(bottomRow)
    table.addClass("callTable")
    return {table:table,hidable:hidable,button:buttonDiv}
}

//sets whether obj is hidden
function setHide(obj,hidden) {
    if (hidden)
        obj.hide()
    else
        obj.show()
}

//makes a call display the appropriate amount of stuff,
//as determined by html.data("expanded"))
function updateCall(html) {
    var expanded = html.data("expanded")
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

//expands/collapses a call
function toggleCall(html) {
    html.data("expanded",!html.data("expanded"))
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

    for (var i = 0; i < traceNode.children.length; i++) {
        var cell = element('td')
        collapsedDiv = makeCall(traceNode.children[i],div);
        cell.append(collapsedDiv)

        lowerRow.append(cell);
    }

    div.append(upperTable)
    div.append(lowerTable)
    div.data("expanded",false)
    div.data("hidable",hidable)
    div.data("button",button)
    updateCall(div)
    return div
}

//sets up js stuff
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

//makes the expand/collapse buttons work
$('.button div').live('mouseenter',function(event) {
    toggleCall($(this).parents(".call").first())
})

//makes the expandables expand/collapse appropriately
//and highlight on hover
$(".expandable").live("click",function (event) {//expand/collapse
    toggleExpandable($(this))
}).live("mouseenter",function (event) {//hover
    $(this).addClass("hover")
}).live("mouseleave",function (event) {
    $(this).removeClass("hover")
})

//makes the tabs switch what is displayed and
//highlight on hover
$('ul.tabs li.other').live('click', function (event) {//switch display
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
}).live("mouseenter",function (event) {//hover
    $(this).addClass("hover")
}).live("mouseleave",function (event) {
    $(this).removeClass("hover")
})
