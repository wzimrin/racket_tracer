//William Zimrin and Jeanette Miranda
//tracer.js	6/02/2011

//----- GENERAL HELPERS -----

function toInt(cssString) {
    return parseInt(cssString.substring(0, cssString.length - 2))
}

//creates a dom element of the type tag
function element(tag) {
    return $("<"+tag+'/>')
}

//----- CODE PANE HELPERS -----

function clearHighlight(el) {
    $(".highlight").children().unwrap()
}

//Highlight the span of text beginning at idx in el
function highlightSpan(el,idx,span) {
    clearHighlight(el)
    var hi = element("span")
    hi.addClass("highlight")
    el.children().slice(idx-codeOffset,span+idx-codeOffset).wrapAll(hi)
}

//----- EXPANDABLE HELPERS -----

//Choose to display the full or short version of an expandable argument
function updateExpandable(html) {
    if (html.data("expanded"))
        html.text(html.data("full"))
    else
        html.text(html.data("short"))
}

//Toggle between the full and short version of an expandable argument
function toggleExpandable(html) {
    html.data("expanded",!html.data("expanded"))
    updateExpandable(html)
}

//----- CALL TABLE HELPERS -----

//Makes a cell on the call table, either an actual or a result
function makeCell(formShort, formFull, cssClass) {
    var TD = element("td")
    TD.addClass(cssClass + " cell")

    var div = element("div")
    if (formFull.type=="image") {
        var el = element("img")
        el.attr("src",formFull.src)
        div.append(el)
    } else {
        //If a shortened form exists form exists
        if (formShort.value != formFull.value) {
            div.addClass("expandable")
            div.data({short: formShort.value, full: formFull.value, expanded: false})
            updateExpandable(div)
        }
        else {
            div.text(formFull.value)
        }
    }
    TD.append(div)
    return TD
}

//Formats function name, actuals and result into table form
function makeCallTable(node) {
    var table = element("table")
    table.addClass("callTable")
    var row = element("tr")

    //Function name
    var nameTD = element("td")
    nameTD.text(node.name)
    nameTD.addClass("name cell")
    nameTD.data({idx: node.idx, span: node.span, linum: node.linum})
    if(!(node.idx == 0 && node.span == 0))
        nameTD.addClass("hasSource")
    row.append(nameTD)

    //Formals and actuals
    for (var i = 0; i < node.actuals.length; i++) {
        //Display in collapsed form if actualsExpanded is undefined or false
        var actual = makeCell(node.actualsShort[i],node.actuals[i],"arg")
        row.append(actual)
    }

    //Arrow
    var arrow = element('td')
    arrow.html("&rarr;")
    arrow.addClass("arrow cell")
    row.append(arrow)

    //Result
    resultTD = makeCell(node.resultShort,node.result,false,"result")
    row.append(resultTD)

    table.append(row)
    return table
}

//----- VISIBILITY HELPERS -----

//sets whether obj is hidden
function setHide(obj,hidden,animate) {
    if (hidden)
        obj.hide(animate)
    else
        obj.show(animate)
}

//Makes a call display the appropriate amount of info
function updateCall(html,animate) {
    var expanded = html.data("expanded")
    var hidable = html.data("hidable")
    var button = html.data("button")
    var buttonImg = button.children("img")
    
    for (var i = 0; i < hidable.length; i++) {
        setHide(hidable[i],!expanded,animate)
    }
    
    if (expanded) 
        buttonImg.attr("src", upImageSrc)
    else 
        buttonImg.attr("src", downImageSrc)

}

//Expand/collapses a call
function toggleCall(html,animate) {
    html.data("expanded",!html.data("expanded"))
    updateCall(html,animate)
}

//Bring parent calls with on scroll
function refocusScreen()
{
    //Find all visible calls
    visibleCalls = $("div#tracer").find(".call").filter(":visible")

    //Check the alignment of each visible call
    visibleCalls.each(function(index) {
        var callTable = $(this).children(".callTable").first()
        var buttonRow = $(this).children(".button")
        //var bodyButton = $(this).children(".body-button").first()
        var callTableMarL = toInt(callTable.css('marginLeft'))
        var fromLeft = $(this).position().left
                        + toInt($(this).css('marginLeft'))
                        + callTableMarL
                        - $("div#tracerWrapper").scrollLeft()
        
        //only move callTables that are less wide than the current width
        //of the call (will this condition always be true?)
        if(callTable.width() < $(this).width()) {
            //This call is off the screen to the left
            if(fromLeft < 0) {
                //Want a maximum margin with right edge of callTable aligned
                //to right edge of call
                var shiftBy = Math.min($(this).width()-callTable.width(), 
                                        callTableMarL-fromLeft)
                callTable.css('marginLeft', shiftBy)
                buttonRow.css('marginLeft', shiftBy)
                //bodyButton.css('marginLeft', shiftBy)
            }
            //This call is to the right of the left edge of the screen
            //And not aligned with its left edge
            else if (fromLeft > 0 && callTableMarL > 0) {
                //Want a minimum margin of 3
                var shiftBy = Math.max(3, callTableMarL-fromLeft)
                callTable.css('marginLeft', shiftBy)
                buttonRow.css('marginLeft', shiftBy)
                //bodyButton.css('marginLeft', shiftBy)
            }
        }
    })
}

function addIcon(container, src, srcSel ) {
    var icon = element("img")
    icon.attr("src", src)
    icon.data("otherSrc", srcSel)
    //icon.height(imageSize)
    //icon.width(imageSize)
    container.append(icon)
}

function swapIcon(img)
{
    var src = img.attr("src")
    img.attr("src", img.data("otherSrc"))
    img.data("otherSrc", src)
}

//----- BUILDING CALLS -----

//Makes an expandedCall: delete button, function, formals, actuals and result
//All in their appropriate expanded or unexpanded form
function makeCall(traceNode, parent) {
    parent = $(parent)

    var call = element("div")
    if (parent.hasClass("background1"))
        call.addClass("background2")
    else
        call.addClass("background1")

    var ceButton = element("td")
    if (traceNode.ceIdx) {
        if (traceNode.ceCorrect) {
            call.addClass("passed-ce")
            addIcon(ceButton, correctCEImageSrc, correctCEImageSelSrc)
        }
        else {
            call.addClass("failed-ce")
            addIcon(ceButton, failedCEImageSrc, failedCEImageSelSrc)
        }
        ceButton.addClass("button to-src-button hasSource")
        ceButton.data({idx: traceNode.ceIdx,
                       span: traceNode.ceSpan})
    }
    call.addClass("call")
    
    var callTable = makeCallTable(traceNode)

    var childrenButton = element("td")
    addIcon(childrenButton, upImageSrc, upImageSrc)
    childrenButton.addClass("button ec-button")

    var bodyButton = element('td')
    addIcon(bodyButton, toDefImageSrc, toDefImageSelSrc)
    bodyButton.addClass("to-src-button button")
    if(!(traceNode.srcIdx == 0 && traceNode.srcSpan == 0)) {
        bodyButton.addClass("hasSource")
        bodyButton.data({idx:traceNode.srcIdx,
                     span:traceNode.srcSpan})
    }
    
    var hidable = []
    
    var lowerDiv = element("div")
    lowerDiv.addClass("childTableParent")
    var childTable = element('table')
    hidable.push(lowerDiv)
    childTable.addClass("childTable")
    var lowerRow = element('tr');
    childTable.append(lowerRow)

    for (var i = 0; i < traceNode.children.length; i++) {
        var cell = element('td')
        cell.addClass("childTD")
        collapsedDiv = makeCall(traceNode.children[i],call);
        cell.append(collapsedDiv)
        lowerRow.append(cell);
    }
    
    lowerDiv.append(childTable)
    call.append(callTable)
    var buttonTable = element('table')
    if(bodyButton.hasClass("hasSource")) 
        buttonTable.append(bodyButton)
    if (traceNode.children.length!=0)
        buttonTable.append(childrenButton)
    if (traceNode.ceIdx)
        buttonTable.append(ceButton)
    call.append(buttonTable)
    
    //The source position and span of check-expect, actual and expected are set to 
    //0 for identification. Don't have a definition/body for any of these.
    
    call.append(lowerDiv)
    call.data("expanded",false)
    call.data("hidable",hidable)
    call.data("button",childrenButton)

    updateCall(call)
    return call
}

//----- CREATING PAGE -----

$(document).ready(function () {
    //Check for browser compatibility
    if(!($.browser.webkit || $.browser.mozilla)) {
        alert("The Tracer has not been tested on your browser and may have compatibility issues. We suggest using Firefox, Chrome or Safari.")
    }

    var tabs = $("#tabbar")
    var bodyWrapper = $("#tracerWrapper")
    var bodies = $("#tracer")

    var codePane = $("#codePane")
    var codePaneWrapper = $("#codePaneWrapper")
    var codePaneButton = $("#codePaneButton")
    for (var i = 0; i < code.length; i++) {
        if (code[i].type=="string") {
            for (var j = 0; j < code[i].text.length; j++) {
                var el = element("span")
                el.text(code[i].text[j])
                el.addClass(code[i].color)
                el.addClass("codeChar")
                el.addClass("codeElem")
                codePane.append(el)
            }
        } else if (code[i].type=="image") {
            var el = element("img")
            el.attr("src",code[i].src)
            el.addClass(code[i].color)
            el.addClass("codeImg")
            el.addClass("codeElem")
            codePane.append(el)
        } else if (code[i].type=="html") {
            var el = element("span")
            el.html(code[i].html)
            el.addClass(code[i].color)
            el.addClass("codeChar")
            el.addClass("codeElem")
            codePane.append(el)
        }
    }
    var codePaneWidth

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

    // -------------------------------------------------------------------------
    //                                      EVENTS
    // -------------------------------------------------------------------------
    
    var hideAnimateDuration = 200
    function setCodePaneWidth() {
        codePane.width(codePaneWrapper.width()+codePane.width()-codePane.outerWidth(true))
    }
    
    //Set the width of the code pane to a new value and animate
    function setCodePaneWrapperWidth(newWidth,speed, arrow, onComplete) {
        if (codePaneWidth != newWidth) {
            codePane.addClass("hidden")
            codePaneWidth = newWidth            
            codePaneWrapper.animate({"width":newWidth+"%"},
                                    {duration:speed, 
                                    complete: function() {
                                        setCodePaneWidth()
                                        codePane.removeClass("hidden")
                                        //codePane.animate({"opacity":1, duration: 100})
                                        codePaneButton.html(arrow)
                                        onComplete()}})
            bodyWrapper.animate({"width":(100-newWidth)+"%"},
                                           speed)
        }
        else
            onComplete()
    }

    var collapsedCodePaneWidth = 10
    var expandedCodePaneWidth = 50
    //Expand the code pane
    function expandCodePane(onComplete) {
        //codePane.addClass("hidden")
        //codePane.animate({"opacity":0, duration: 100})
        setCodePaneWrapperWidth(expandedCodePaneWidth, "slow", "&raquo;", 
                        onComplete)
        //onComplete()
    }
    //Collapse the code pane
    function collapseCodePane() {
        //codePane.addClass("hidden")
        //codePane.animate({"opacity":0, duration: 100})
        setCodePaneWrapperWidth(collapsedCodePaneWidth,"fast", "&laquo",
            function(){})
    }
    
    //Expand and collapse codePane on click 
    codePaneButton.click(function () {
        if (codePaneWidth==expandedCodePaneWidth)
            collapseCodePane()
        else
            expandCodePane(function(){})
    })

    codePane.mousedown(false)
    
    codePane.click(function () {
        codePaneButton.click()
        return false;
    })
    
    
    //Animate to move to new highlighted code if necessary
    function showSpan() {
        var span = codePane.find(".highlight")
        var pos = span.position()
        var height = codePane.height()
        var width = codePane.width()
        //If new span is off the displayed portion of the code
        codePane.animate({scrollTop: pos.top-(height/2)+codePane.scrollTop(),
                          scrollLeft: pos.left-(width/2)+codePane.scrollLeft()}, 
                         'slow')
    }

    var lastFunctionHighlighted;
    
    //Function names on click
    $(".hasSource").bind('click', function () {
        if (lastFunctionHighlighted == this) {
            lastFunctionHighlighted = false;
            swapIcon($(".lastHighlighted").children("img"))
            $(".lastHighlighted").removeClass("lastHighlighted")
            clearHighlight(codePane)
            collapseCodePane()
        } else {
            var target = $(this)
            swapIcon($(".lastHighlighted").children("img"))
            $(".lastHighlighted").removeClass("lastHighlighted")
            target.addClass("lastHighlighted")
            swapIcon(target.children("img"))
            highlightSpan(codePane,target.data("idx"),target.data("span"))
            expandCodePane(showSpan)
            //showSpan()
            lastFunctionHighlighted = this;
        }
    })
    
    //makes the expand/collapse buttons work
    $('.ec-button').bind('click',function(event) {
        thisCall = $(this).parents(".call").first()
        toggleCall(thisCall,"fast")
    })

    bodyWrapper.scroll(refocusScreen)
    //makes the expandables expand/collapse appropriately
    //and highlight on hover
    $(".expandable").bind("click",function (event) {//expand/collapse
        toggleExpandable($(this))
    })

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
        $(bodyWrapper).scrollLeft(0)
        swapIcon($(".lastHighlighted").children("img"))
        $(".lastHighlighted").removeClass("lastHighlighted")
        clearHighlight(codePane)
        collapseCodePane()
        refocusScreen()
    })

    first.trigger("click")
    
    function setContentSize() {
        
        $(".column").height($(window).height()-$("div#tabbar").outerHeight()
                            -2*parseInt($(document.body).css("margin-top")))
        
       codePane.height(codePaneWrapper.height()-codePaneButton.outerHeight(true)+codePane.height()-codePane.outerHeight(true))
        setCodePaneWidth()
    }


    setContentSize()
   $(window).trigger("resize") 
    //Begin with a collapsed code pane
    //collapseCodePane()
    $(window).resize(setContentSize)

    function dragHandler(event) {
        var oldX=event.pageX
        var oldY=event.pageY
        var body = $(document.body)
        var target = $(this)
        body.addClass("dragging")
        bodies.addClass("dragging")

        function moveHandler(event) {
            var newTime = new Date().getTime()
            var newX = event.pageX
            var newY = event.pageY
            bodyWrapper.scrollLeft(bodyWrapper.scrollLeft()-newX+oldX)
            bodyWrapper.scrollTop(bodyWrapper.scrollTop()-newY+oldY)
            oldX=newX
            oldY=newY
            return false
        }
        function endHandler(event) {
            var newX = event.pageX
            var newY = event.pageY
            bodyWrapper.scrollLeft(bodyWrapper.scrollLeft()-newX+oldX)
            bodyWrapper.scrollTop(bodyWrapper.scrollTop()-newY+oldY)
            body.unbind("mousemove",moveHandler)
            body.unbind("mouseup",endHandler)
            body.unbind("mouseleave",endHandler)
            $(".dragging").removeClass("dragging")
            return false
        }
        bodies.unbind("mousedown",dragHandler)
        target.mousedown()
        target.mouseup()
        bodies.mousedown(dragHandler)
        body.mousemove(moveHandler)
        body.mouseup(endHandler)
        body.mouseleave(endHandler)
        return false
    }
    
    bodies.mousedown(dragHandler)
})

