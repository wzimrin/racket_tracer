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
function makeCell(formShort, formFull, literalForm, cssClass) {
    var TD = element('td')
    TD.addClass(cssClass)
    TD.addClass("cell")

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
    if (literalForm){
        div.addClass("literal")
        div.data("literal",literalForm)
    }
    TD.append(div)
    return TD
}

//Makes the call table: function name, formals, actuals and result in table form
function makeCallTable(node) {
    var row = element('tr')
    var table = element("table")

    //Function name
    var nameTD = element('td')
    nameTD.attr("rowspan")
    nameTD.text(node.name)
    nameTD.addClass("name")
    nameTD.addClass("cell")
    row.append(nameTD)

    //Formals and actuals
    for (var i = 0; i < node.formals.length; i++) {
        //Display in collapsed form if actualsExpanded is undefined or false
        var actual = makeCell(node.actualsShort[i],node.actuals[i],node.formals[i],"arg")
        row.append(actual)
    }

    //Arrow
    var arrow = element('td')
    arrow.text("=>")
    arrow.addClass("arrow")
    arrow.addClass("cell")
    row.append(arrow)

    //Result
    resultTD = makeCell(node.resultShort,node.result,false,"result")
    
    row.append(resultTD)

    table.append(row)
    table.addClass("callTable")
    return table
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

    var upperTable = makeCallTable(traceNode)

    var button = element("div")
    button.text("-")
    button.addClass("button")

    var hidable = []

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
    if (traceNode.children.length!=0)
        div.append(button)
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
    var bodyWrapper = $("#tracerWrapper")
    var bodies = $("#tracer")
    //bodyWrapper.append(bodies)
    var leftScroll = $("#leftScroll")
    leftScroll.addClass('scrollButton')
    var rightScroll = $("#rightScroll")
    rightScroll.addClass('scrollButton')
    var upScroll = $("#upScroll")
    upScroll.addClass('scrollButton')
    var downScroll = $("#downScroll")
    downScroll.addClass('scrollButton')

    var codePane = $("#codePane")
    codePane.text(code) // change to code after merge FA
    var codePaneWidth = 300;


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
        li.data("child",exp)
        bodies.append(exp)
    }

    // ----------------------------------------------------------------------------
    //                                      EVENTS
    // ----------------------------------------------------------------------------

    var moveInc = 2
    var tL;
    $('#leftScroll').bind('mousedown', function(event) {
        var moveLeft = function() {
            newPos = bodies.css('left')
            newPosInt = parseInt(newPos.substring(0, newPos.length-2))+moveInc
            /*if(newPosInt <= codePaneWidth)*/
                bodies.css('left', newPosInt+'px')}
        tL = setInterval(moveLeft, 1)
    }).bind('mouseup', function(event) {
        clearInterval(tL)
    }).bind('mouseleave', function(event) {
        clearInterval(tL)
    })

    var tR;
    $('#rightScroll').bind('mousedown', function(event) {
        var moveRight = function() {
            newPos = bodies.css('left')
            newPosInt = parseInt(newPos.substring(0, newPos.length-2))-moveInc
           /* if(newPosInt > document.body.clientWidth-bodies.width()) */
                bodies.css('left', newPosInt+'px')
            }
        tR = setInterval(moveRight, 1)
    }).bind('mouseup', function(event) {
        clearInterval(tR)
    }).bind('mouseleave', function(event) {
        clearInterval(tR)
    })
    
    var tU;
    $('#upScroll').bind('mousedown', function(event) {
        var moveUp = function() {
            newPos = bodies.css('top')
            newPosInt = parseInt(newPos.substring(0, newPos.length-2))+moveInc
            /*if(newPosInt <= codePaneWidth)*/
                bodies.css('top', newPosInt+'px')}
        tL = setInterval(moveLeft, 1)
    }).bind('mouseup', function(event) {
        clearInterval(tU)
    }).bind('mouseleave', function(event) {
        clearInterval(tU)
    })

    var tD;
    $('#downScroll').bind('mousedown', function(event) {
        var moveDown = function() {
            newPos = bodies.css('top')
            newPosInt = parseInt(newPos.substring(0, newPos.length-2))-moveInc
            /*if(newPosInt <= codePaneWidth)*/
                bodies.css('top', newPosInt+'px')}
        tL = setInterval(moveLeft, 1)
    }).bind('mouseup', function(event) {
        clearInterval(tD)
    }).bind('mouseleave', function(event) {
        clearInterval(tD)
    })


    //makes the expand/collapse buttons work
    $('.button').bind('mouseenter',function(event) {
        toggleCall($(this).parents(".call").first())
    })

    //makes the expandables expand/collapse appropriately
    //and highlight on hover
    $(".expandable").bind("click",function (event) {//expand/collapse
        toggleExpandable($(this))
    })/*.live("mouseenter",function (event) {//hover
        $(this).addClass("hover")
    }).live("mouseleave",function (event) {
        $(this).removeClass("hover")
    })*/

    //makes the tabs switch what is displayed and
    //highlight on hover
    $('ul.tabs li.other').bind('click', function (event) {//switch display
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
        $(window).scrollLeft(0)
    }).live("mouseenter",function (event) {//hover
        $(this).addClass("hover")
    }).live("mouseleave",function (event) {
        $(this).removeClass("hover")
    })

    //makes the tabbar scroll with me
    $(window).scroll(function (event) {
        //pageXOffset
        $("#tabbar").animate({display: 'none',
                              marginLeft: $(window).scrollLeft() + "px"}, 
                              40,
                              'linear',
                              function(){})
    })

    $(".literal").tooltip({
        bodyHandler:function () {
            return $(this).data("literal")
        },
        fade:250
    })

    first.trigger("click")
})

