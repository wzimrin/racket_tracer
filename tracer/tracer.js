//William Zimrin and Jeanette Miranda
//tracer.js	6/02/2011

"use strict";

//-----------------------------------------------------------------------------
//                              GLOBAL VARIABLES
//-----------------------------------------------------------------------------
//Various divs on the page -- see index.html for detailed description
var tabbar, messagebar, secondTabBar
var wrapper 
var trace, traceWrapper 
var codePane, codePaneWrapper, codePaneButton

//Widths in percentages of total screen width for the code pane
var collapsedCodePaneWidth = 10
var expandedCodePaneWidth = 50
var codePaneWidth

//Length of animation for hiding and showing objects
var hideAnimateDuration = 200

//-----------------------------------------------------------------------------
//                              GENERAL HELPERS 
//-----------------------------------------------------------------------------

//Turns a css width/height with units in px into a number
function toInt(cssString) {
    return parseInt(cssString.substring(0, cssString.length - 2))
}

//creates a dom element of the type tag
function element(tag) {
    return $("<"+tag+"/>")
}

//-----------------------------------------------------------------------------
//                              BUILDING TABS HELPERS 
//-----------------------------------------------------------------------------

//Initializes the tab-state data for a call
function addInitialData(tabLi, htmlForCall) {
    tabLi.data({"child": htmlForCall,
            "scroll": 0,
            "codePaneExpanded": false,
            "codePaneX": 0,
            "codePaneY": 0,
            "lastHighlighted": []})
}

//Adds a special tab to the top level (big-bang or check-expect)
//Sets the tabs name, data about the ul for it's sub/second tabbar and
//returns the top tab
function addSpecialTopTab(topLevelClass, name, tabsList) {
    var topTabLi = element("li").newAddClass(["other", topLevelClass])
    topTabLi.text(name)

    var secondTabsList = createTabsList("secondTabBar", secondTabBar)
    topTabLi.data("secondTabBarUL", secondTabsList)
    tabsList.append(topTabLi)
    secondTabsList.hide()
    return topTabLi;
}

//Initializes and returns a ul for the elements of a tabbar
function createTabsList(barClass, bar) {
    var tabsList = element("ul").newAddClass([barClass, "tabsFormatting"])
    bar.append(tabsList)
    return tabsList;
}
        
//Creates the li to go in a new tab, adds that element to the tabbar
//and adds the child it links to to trace
function createNewTab(classToAdd, node, tabsList, bar) {
    var li = element("li").newAddClass(classToAdd)
    if(node.title.type == "none")
        li.text(node.name)
    else if (node.title.type == "image") {
        var img = element("img").newAddClass("tabBarImage")
        img.attr("src", node.title.src)
        li.append(img)
    } 
    else if (node.title.type == "value") 
        li.text(node.title.value)
    else
        alert("something wrong in title")
    tabsList.append(li)
    if(classToAdd != "check-expect" && classToAdd != "big-bang")
        classToAdd = false
    var exp = makeCall(node, bar, classToAdd).newAddClass("toplevel")
    addInitialData(li, exp)
    trace.append(exp)
    return li;
}

//-----------------------------------------------------------------------------
//                              CODE PANE HELPERS 
//-----------------------------------------------------------------------------

//Initializes the content of the code pane
function initializeCodePane() {
    for (var i = 0; i < code.length; i++) {
        if (code[i].type=="string") {
            for (var j = 0; j < code[i].text.length; j++) {
                var el = element("span")
                el.text(code[i].text[j])
                el.newAddClass([code[i].color, "codeChar", "codeElem"])
                if (el.text() != "\r")
                    codePane.append(el)
            }
        } else if (code[i].type=="image") {
            var el = element("img")
            el.attr("src",code[i].src)
            el.newAddClass([code[i].color, "codeImg", "codeElem"])
            codePane.append(el)
        } else if (code[i].type=="html") {
            var el = element("span")
            el.html(code[i].html)
            el.newAddClass([code[i].color, "codeChar", "codeElem"])
            codePane.append(el)
        }
    }
}

//Clears the highlight from buttons or function names in the trace,
//and the selected/highlighted code on the codePane
function clearHighlight() {
    swapImage($(".lastHighlighted").children("img")) 
    $(".lastHighlighted").newRemoveClass("lastHighlighted")
    $(".highlight").children().unwrap()
}

//Highlight the span of text beginning at idx in el
function highlightSpan(el,toHighlight) {
    clearHighlight()
    
    var idx = toHighlight.data("idx")
    var span = toHighlight.data("span")

    toHighlight.newAddClass("lastHighlighted")
    swapImage(toHighlight.children("img"))

    var hi = element("span")
    hi.newAddClass("highlight")
    el.children().slice(idx-codeOffset,span+idx-codeOffset).wrapAll(hi)
}

//Sets the width of the codePane based on the wrapper
function setCodePaneWidth() {
    codePane.width(codePaneWrapper.width()+codePane.width()-codePane.outerWidth(true))
}

//Set the width of the code pane to a new value and animate
function setCodePaneWrapperWidth(newWidth,speed, arrow, onComplete) {
    var pickedLI = $("ul.tabbar li.picked")
    if(pickedLI.data("secondTabBar"))
        pickedLI.data("secondTabBar").find("li.picked").data("codePaneExpanded", newWidth == expandedCodePaneWidth)

    pickedLI.data("codePaneExpanded", newWidth == expandedCodePaneWidth)
    
    if (codePaneWidth != newWidth) {
        codePane.newAddClass("hidden")
        codePaneWidth = newWidth            
        codePaneWrapper.animate({"width":newWidth+"%"},
            {duration:speed, 
                complete: function() {
                    setCodePaneWidth()
                    codePane.newRemoveClass("hidden")
                    codePaneButton.html(arrow)
                    onComplete()
                }})
        traceWrapper.animate({"width":(100-newWidth)+"%"},
            speed)
    }
    else
        onComplete()
}

//Expand the code pane
function expandCodePane(onComplete) {
    setCodePaneWrapperWidth(expandedCodePaneWidth, "slow", "&raquo;", 
        onComplete)
}

//Collapse the code pane
function collapseCodePane(onComplete) {
    clearHighlight()
    setCodePaneWrapperWidth(collapsedCodePaneWidth,"fast", "&laquo;",
        onComplete)
}

//Animate to move to new highlighted code if necessary
function showSpan() {
    var span = codePane.find(".highlight")
    var pos = span.position()
    if (pos) {
        var height = codePane.height()
        var width = codePane.width()
        //If new span is off the displayed portion of the code
        codePane.animate({scrollTop: pos.top-(height/2)+codePane.scrollTop(),
                scrollLeft: pos.left-(width/2)+codePane.scrollLeft()}, 
            "slow")
    }
}

//-----------------------------------------------------------------------------
//                              EXPANDABLE HELPERS 
//-----------------------------------------------------------------------------

//Choose to display the full or short version of an expandable argument
function updateExpandable(html) {
    var expandButton = html.data("expandButton")
    
    if (html.data("expanded")) {
        html.text(html.data("full")) 
        expandButton.html("&rArr;&lArr;")
    } else {
        html.text(html.data("short"))
        expandButton.html("&lArr;&rArr;")
    }
    //Setting the html removes the handler, so add it back
    expandButton.newRemoveClass("expandButton").newAddClass("expandButton")
    html.append(expandButton)
}

//Toggle between the full and short version of an expandable argument
function toggleExpandable(html) {
    html.data("expanded",!html.data("expanded"))
    updateExpandable(html)
}

//-----------------------------------------------------------------------------
//                              BUILDING CALL HELPERS 
//-----------------------------------------------------------------------------

//Makes a cell on the call table, either an actual or a result
function makeCell(formShort, formFull, cssClass) {
    var cell = element("td").newAddClass([cssClass, "cell"])
    var div = element("div")
    var expandButton //div for button in top left corner of expandable arguments/results
    cell.append(div)
    
    if (formFull.type=="image") {
        var el = element("img")
        el.attr("src",formFull.src)
        div.append(el)
    }
    else if (formFull.type == "error") 
        div.text(formFull.message)
    else {
        //If a shortened form exists 
        if (formShort.value != formFull.value) {
            div.newAddClass("expandableArg")
            expandButton = element("div").newAddClass("expandButton")
            div.data({short: formShort.value, 
                    full: formFull.value, 
                    expanded: false, 
                    expandButton: expandButton})
            updateExpandable(div) 
            div.append(expandButton)
        }
        else 
            div.text(formFull.value)
    }
    return cell
}

//Formats function name, actuals and result into table form
function makeCallTable(node, type) {
    var table = element("table").newAddClass("callTable")
    var row = element("tr")
   
    var buttonTD = element("td").newAddClass("buttonTD")
   //If you want to add a passed check expect or failed check expect button 
    if (node.ceIdx) {
        var ceButton = element("div")
        if (node.ceCorrect) 
            addImages(ceButton, correctCEImageSrc, correctCEImageSelSrc)
        else 
            addImages(ceButton, failedCEImageSrc, failedCEImageSelSrc)
        ceButton.newAddClass(["button", "to-src-button","hasSource"])
        ceButton.data({idx: node.ceIdx,
                span: node.ceSpan})
        buttonTD.append(ceButton)
    }
    //If this node has children and you want to be able to expand and collapse it
    //Top level check-expect and big-bang nodes are permanently expanded so don't
    //want a childrenButton
    if (node.children.length!=0 && type != "check-expect" && type != "big-bang") {
        var childrenButton = element("div")
        addImages(childrenButton, sideImageSrc, sideImageSrc)
        childrenButton.newAddClass(["button", "childrenButton"])
        buttonTD.append(childrenButton)
    }
    if (buttonTD.children().length != 0) //one or more buttons was added
        row.append(buttonTD) 
        

    //Function name
    var nameTD = element("td")
    if (type == "check-expect")
        nameTD.text(node.prefix+": "+node.name)
    else
        nameTD.text(node.name)
    nameTD.newAddClass(["name", "cell"])
   //If this node has information about the definition/src of this function 
    if(!(node.srcIdx == 0 && node.srcSpan == 0)) {
        nameTD.newAddClass("hasSource")
        nameTD.data({idx: node.srcIdx, span: node.srcSpan})
    }
    row.append(nameTD)

    //Don't want to display arguments at top level of check expects
    if (type != "check-expect") {
        //Formals and actuals
        for (var i = 0; i < node.actuals.length; i++) {
            //Display in collapsed form if actualsExpanded is undefined or false
            var actual = makeCell(node.actualsShort[i],node.actuals[i],"arg")
            row.append(actual)
        }

        //Arrow
        var arrow = element("td").html("&rarr;")
        arrow.newAddClass(["arrow", "cell"])
        row.append(arrow)

        //Result
        var resultTD = makeCell(node.resultShort,node.result,"result")
        row.append(resultTD)
        resultTD.data({idx: node.idx, span: node.span})
        //If have info about the call site, make the result clickable
        if(!(node.idx == 0 && node.span == 0))
            resultTD.newAddClass("hasSource")
    }

    table.append(row)
    return table
}

//Makes an expandedCall: delete button, function, actuals and result
//All in their appropriate expanded or unexpanded form
function makeCall(traceNode, parent, type) {
    parent = $(parent)

    var call = element("div").newAddClass("call")
    if (parent.hasClass("background1"))
        call.newAddClass("background2")
    else
        call.newAddClass("background1")

    if (type == "check-expect") 
        call.newAddClass("failed-ce")
    else if (traceNode.ceIdx) {
        if (traceNode.ceCorrect) 
            call.newAddClass("passed-ce")
        else 
            call.newAddClass("failed-ce")
    }
    else if (type == "big-bang")
        call.newAddClass("big-bang")

    var expand = false
    if(traceNode.result.type == "error") {
        console.log("result type was error, setting expand to true")
        call.newAddClass("error")
        expand = true
    }
    
    var callTable = makeCallTable(traceNode, type)
    
    var lowerDiv = element("div") //need this div to make animation of calls smooth
    var childTable = element("table")
    var lowerRow = element("tr")
    childTable.append(lowerRow)
    lowerDiv.append(childTable)
    lowerDiv.hide() //Hide the children initially
    
    call.append(callTable)
    call.append(lowerDiv)
    call.data({expanded: false, 
            hidable: lowerDiv, 
            button: callTable.find(".childrenButton"),
            node: traceNode,
            childrenCreated: false,
            childRow: lowerRow})
   //If it's a check-expect or big-bang, want it to start out expanded 
    if (type == "check-expect" || type == "big-bang" || expand)
        toggleCall(call)
    return call
}

//-----------------------------------------------------------------------------
//                              VISIBILITY HELPERS 
//-----------------------------------------------------------------------------

//toggles whether a call is expanded
//And adds the children if this was called from the childrenButton callback 
function toggleCall(html) {
    html.data("expanded",!html.data("expanded"))
    //If the children haven't bene created yet, and you want to expand the node
    //create the children
    if(html.data("expanded") && !html.data("childrenCreated")) {
        var traceNode = html.data("node")
        var childRow = html.data("childRow")
        for (var i = 0; i < traceNode.children.length; i++) {
            var cell = element("td").newAddClass("childTD")
            var collapsedDiv = makeCall(traceNode.children[i],html, false)
            cell.append(collapsedDiv)
            childRow.append(cell)
        }
        html.data({"childrenCreated": true, "expanded": true})
    }

    var expanded = html.data("expanded")
    var hidable = html.data("hidable")
    var buttonImg = html.data("button").children("img")

    //hide or show the children, and swap the arrow/childrenButton
    if(expanded) {
        hidable.show("fast")
        buttonImg.attr("src", downImageSrc)
    }
    else {
        hidable.hide("fast")
        buttonImg.attr("src", sideImageSrc)
    }
}

//Before a tab change stores the scroll position of the trace
function storePageState(tabLi) {
    if(tabLi.length != 0) {
        if(tabLi.data("secondTabBarUL"))
            tabLi = tabLi.data("secondTabBarUL").find("li.picked") 
        tabLi.data({"scrollX": traceWrapper.scrollLeft(),
                "scrollY": traceWrapper.scrollTop(),
                "codePaneExpanded": codePaneWidth == expandedCodePaneWidth,
                "codePaneX": codePane.scrollLeft(),
                "codePaneY": codePane.scrollTop(),
                "lastHighlighted": $(".lastHighlighted")})
    }
}

//When loading a new tab, restore the prior state of that tab
//trace scroll x and y, code pane scroll x and y, highlights, what
//was picked in the second tab bar
function restorePageState(tabLi) {
    if(tabLi.length != 0) {
        if(tabLi.data("secondTabBar"))
            tabLi = tabLi.data("secondTabBar").children("li.picked") 
        if(tabLi.data("lastHighlighted").length != 0) {
            var toHighlight = tabLi.data("lastHighlighted")
            highlightSpan(codePane, toHighlight)
        }
        //Expand or collapse the codePane, and onComplete, restore the scrolled position
        //Otherwise the scrolling doesn't occur
        if(tabLi.data("codePaneExpanded")) 
            expandCodePane(function(){
                    codePane.scrollLeft(tabLi.data("codePaneX"))
                    codePane.scrollTop(tabLi.data("codePaneY"))
                })
        else 
            collapseCodePane(function() {
                    codePane.scrollLeft(tabLi.data("codePaneX"))
                    codePane.scrollTop(tabLi.data("codePaneY"))
                })
        
        traceWrapper.scrollLeft(tabLi.data("scrollX"))
        traceWrapper.scrollTop(tabLi.data("scrollY"))
    }
}

//Switch from one tab to another and restore the state of the prior tab
function switchTo(tab) {
    $(".toplevel").hide()
    tab.data("child").show()
    clearHighlight()
    restorePageState(tab) 
    refocusScreen()
}

//Bring parent calls with on scroll
function refocusScreen()
{
    //Find all visible calls
    var visibleCalls = trace.find(".call").filter(":visible")

    //Check the alignment of each visible call
    visibleCalls.each(function(index) {
            var callTable = $(this).children(".callTable").first()
            var callTableMarL = toInt(callTable.css("marginLeft"))
            var fromLeft = $(this).position().left
                + toInt($(this).css("marginLeft"))
                + callTableMarL
                - wrapper.scrollLeft()

            //only move callTables that are less wide than the current width
            //of the call (will this condition always be true?)
            if(callTable.width() < $(this).width()) {
                //This call is off the screen to the left
                if(fromLeft < 0) {
                    //Want a maximum margin with right edge of callTable aligned
                    //to right edge of call
                    var shiftBy = Math.min($(this).width()-callTable.width(), 
                        callTableMarL-fromLeft)
                    callTable.css("marginLeft", shiftBy)
                }
                //This call is to the right of the left edge of the screen
                //And not aligned with its left edge
                else if (fromLeft > 0 && callTableMarL > 0) {
                    //Want a minimum margin of 3
                    var shiftBy = Math.max(3, callTableMarL-fromLeft)
                    callTable.css("marginLeft", shiftBy)
                }
            }
        })
}

//Adds images to buttons that link to the source
function addImages(container, src, srcSel ) {
    var image = element("img")
    image.attr("src", src)
    image.data("otherSrc", srcSel)
    container.append(image)
}

//Swaps the image for a button that links to the source
//Highlighted vs. not highlighted
function swapImage(img)
{
    var src = img.attr("src")
    img.attr("src", img.data("otherSrc"))
    img.data("otherSrc", src)
}


//-----------------------------------------------------------------------------
//                              CALLBACKS 
//-----------------------------------------------------------------------------

//Allows the user to click and drag to move the trace
function dragHandler(event) {
    var oldX=event.pageX
    var oldY=event.pageY
    var body = $(document.body)
    var target = $(this)
    body.newAddClass("dragging")
    trace.newAddClass("dragging")

    function moveHandler(event) {
        var newTime = new Date().getTime()
        var newX = event.pageX
        var newY = event.pageY
        traceWrapper.scrollLeft(traceWrapper.scrollLeft()-newX+oldX)
        traceWrapper.scrollTop(traceWrapper.scrollTop()-newY+oldY)
        oldX=newX
        oldY=newY
        return false
    }
    function endHandler(event) {
        var newX = event.pageX
        var newY = event.pageY
        traceWrapper.scrollLeft(traceWrapper.scrollLeft()-newX+oldX)
        traceWrapper.scrollTop(traceWrapper.scrollTop()-newY+oldY)
        body.unbind("mousemove",moveHandler)
        body.unbind("mouseup",endHandler)
        body.unbind("mouseleave",endHandler)
        $(".dragging").newRemoveClass("dragging")
        return false
    }
    trace.unbind("mousedown",dragHandler)
    target.mousedown()
    target.mouseup()
    trace.mousedown(dragHandler)
    body.mousemove(moveHandler)
    body.mouseup(endHandler)
    body.mouseleave(endHandler)
    return false
}

//Callback for buttons that link to source
function hasSourceCallback() {
    var target = $(this)
    if (target.hasClass("lastHighlighted")) {
        clearHighlight()
        collapseCodePane(function(){})
    } else {
        highlightSpan(codePane,target) 
        expandCodePane(showSpan)
    }
}

//makes the expand/collapse buttons work
function childrenButtonCallback() {
    var thisCall = $(this).parents(".call").first()
    toggleCall(thisCall)
    return false
}

//makes the expandables expand/collapse appropriately
//and highlight on hover
function expandButtonCallback() {
    toggleExpandable($($(this).parent()))
    return false
}

//For when li in the second tabbar are clicked
function secondTabBarCallback() {
    return function() {
        var target = $(this)
        if(!target.hasClass("picked")) {
            var oldPicked = target.parent().find("li.picked")

            storePageState(oldPicked)

            oldPicked.newRemoveClass("picked").newAddClass("other")
            target.newRemoveClass("other").newAddClass("picked")
            switchTo(target) 
        }
    }
}

//Hides or shows the appropriate content of a secondTabBar, and the secondTabBar itself
function handleSecondTabBars(oldPickedLi, targetLi) {

    if(oldPickedLi.data("secondTabBarUL") != targetLi.data("secondTabBarUL")) {
        if(oldPickedLi.data("secondTabBarUL")) {
            secondTabBar.children("ul").hide()
            secondTabBar.css("border-bottom-style", "none")
            secondTabBar.css("height", "0px")
        }
        if(targetLi.data("secondTabBarUL")) {
            var newTabUL = targetLi.data("secondTabBarUL")
            newTabUL.show()
            if(targetLi.hasClass("check-expect-top-level"))
                secondTabBar.css("border-bottom-color", "red")
            else
                secondTabBar.css("border-bottom-color", "black")
            secondTabBar.css("border-bottom-style", "solid")
            secondTabBar.css("height", "auto")
        }
        setContentSize()
    }
}

//makes the tabs switch what is displayed and
//highlight on hover
function tabbarCallback() {
    var target = $(this)
    if(!target.hasClass("picked")) {
        var oldPicked = $("ul.tabbar li.picked")
        storePageState(oldPicked)
        oldPicked.newRemoveClass("picked")
        oldPicked.newAddClass("other")
        
        handleSecondTabBars(oldPicked, target)
        target.newAddClass("picked")
        target.newRemoveClass("other")
        if(target.data("secondTabBarUL"))
            switchTo(target.data("secondTabBarUL").find("li.picked")) 
        else
            switchTo(target) 
    }
}

//Adjusts the width of the wrapper (trace and codePane) based on the size of the window
//And then sets the width of the codePane relative to that
//useful for window resize
function setContentWidth() {
    wrapper.width($(window).width()-parseInt(wrapper.css("padding-left"))
        -2*parseInt($(document.body).css("margin-left")))
    setCodePaneWidth()
}

//Adjusts the width and height of the content based on the size of the window
//See comment for setContentWidth()
function setContentSize() {
    setContentWidth() 

    $(".column").height($(window).height()-tabbar.outerHeight() - secondTabBar.outerHeight()
        - messagebar.outerHeight() 
        -2*parseInt($(document.body).css("margin-top")))
    codePane.height(codePaneWrapper.height()-codePaneButton.outerHeight(true)
        +codePane.height()-codePane.outerHeight(true))
}

//Callbacks that correspond with classes, used to bind callbacks when elements
//are added dynamically that need callbacks
var callbacks = {
    "hasSource" : hasSourceCallback,
    "childrenButton" : childrenButtonCallback,
    "expandButton" : expandButtonCallback
}; //don't delete me, i'm an important semicolon

//overwrite addClass and removeClass to bind or unbind callbacks as well, see callbacks
//classes is either a string, or an array of strings
(function($) {
        $.fn.newAddClass = function(classes) {
            if(typeof(classes) == "string")
                classes = [classes]

            for (var i = 0; i < classes.length; i++) {
                if(!this.hasClass(classes[i])) {
                    if(callbacks[classes[i]])
                        this.click(callbacks[classes[i]])
                    this.addClass(classes[i])
                }
            }
            return this
        }
        $.fn.newRemoveClass = function(classes) {
            if(typeof(classes) == "string")
                classes = [classes]

            for (var i = 0; i < classes.length; i++) {
                if(callbacks[classes[i]])
                    this.unbind("click", callbacks[classes[i]])
                this.removeClass(classes[i])
            }
            return this
        }
    })(jQuery)

//-----------------------------------------------------------------------------
//                              CREATING PAGE
//-----------------------------------------------------------------------------

$(document).ready(function () {
    
    //Check for browser compatibility
    if(!($.browser.webkit || $.browser.mozilla)) {
        alert("The Tracer has not been tested on your browser and may have compatibility issues. We suggest using Firefox, Chrome or Safari.")
    }

    //Initialize global variables
    //See GLOBAL VARIABLES for description
    tabbar = $("#tabbar").newAddClass("tabsDiv")
    secondTabBar = $("#secondTabBar").newAddClass("tabsDiv")
    messagebar = $("#messagebar")
    wrapper = $("#wrapper")
    trace = $("#trace")
    traceWrapper = $("#traceWrapper")
    codePane = $("#codePane")
    codePaneWrapper = $("#codePaneWrapper")
    codePaneButton = $("#codePaneButton")

    initializeCodePane() //add the code to the codePane
    
    //Top Level tabbar
    var tabsList = createTabsList("tabbar", tabbar)
    
    //Adding top level calls to the tabbar
    var first = false
    for (var i = 0; i < theTrace.children.length; i++) {
        var tabLi = createNewTab("other", theTrace.children[i], tabsList, tabbar)
        
        if(errored && theTrace.children[i].result.type == "error") {
            first = tabLi
            messagebar.text(" Your program generated an error")
        }
        else if (!first) {first = tabLi}
    }
    
    //Big bang to tabbar, and creating bigbangbar
    var errorInBigBang = false
    for (var l = 0; l < bigBangTrace.children.length; l++) {
        var topBigBangTabLi = addSpecialTopTab("big-bang-top-level", "big-bang", tabsList)
        var bigBangList =  topBigBangTabLi.data("secondTabBarUL")       

        for(var k = 0; k < bigBangTrace.children[l].children.length; k++) {
            var bigBangTabLi = createNewTab("big-bang", bigBangTrace.children[l].children[k], bigBangList, secondTabBar)

            if(errored && bigBangTrace.children[l].children[k].result.type == error) {
                first = topBigBangTabLi
                errorInBigBang = bigBangTabLi
                messagebar.text("Your program generated an error")
                bigBangTabLi.newAddClass("picked")
            } 
            else
                bigBangTabLi.newAddClass("other")
        }
        if (!first) {first = topBigBangTabLi}
        //One of the children needs to be picked. If there was an error, one already is
        //If not, make the first one picked
        if(!errorInBigBang)
            bigBangList.children().first().newRemoveClass("other").newAddClass("picked")
        
    }
    
    //check-expect to tabbar and creating cebar
    var errorInCE = false
    if (ceTrace.children.length > 0) {
        var topCeTabLi = addSpecialTopTab("check-expect-top-level", "check-expect", tabsList)
        var ceList =  topCeTabLi.data("secondTabBarUL")
        
        for(var j = 0; j < ceTrace.children.length; j++) {
            var ceTabLi = createNewTab("check-expect", ceTrace.children[j], ceList, secondTabBar) 
            
            if(errored && 
                (ceTrace.children[j].children[0].result.type == "error" 
                    || ceTrace.children[j].children[1].result.type == "error")) {
                first = topCeTabLi
                errorInCE = ceTabLi
                messagebar.text(" Your program generated an error")
                ceTabLi.newAddClass("picked")
            } else
                ceTabLi.newAddClass("other")
            
            first = topCeTabLi
        }
        if(!errorInCE)
            ceList.children().first().newRemoveClass("other").newAddClass("picked")
    }


    
    // -------------------------------------------------------------------------
    //                          BINDING CALLBACKS
    // -------------------------------------------------------------------------
    
    //Expand and collapse codePane on click 
    codePaneButton.click(function () {
        if (codePaneWidth==expandedCodePaneWidth)
            collapseCodePane(function(){})
        else
            expandCodePane(function(){})
    })
    codePane.mousedown(false) //can't highlight text on the codepane
   
    //on single click expand/collapse the code pane
    codePane.click(function () {
        codePaneButton.click()
        return false
    })
   
    //when the messagebar is clicked, open to where the error is
    messagebar.bind("click", function() {
            first.trigger("click")
            if(errorInBigBang)
                errorInBigBang.trigger("click")
            else if(errorInCE)
                errorInCE.trigger("click")
        })
 
    //Initially the second tabbar is hidden
    secondTabBar.css({"height": "0px", "border-bottom-style":"none"})
    secondTabBar.find("li").bind("click", secondTabBarCallback())
    //Callbacks for the li in the top level tab bar
    $("ul.tabbar li").bind("click", tabbarCallback) 
       
    //Click on what the first element should be. In order of importance
    //A caught error, failed check-expects, the first element
    first.trigger("click")

    setContentSize()

    //Bind handlers for window resize, scroll in the tracer, and click and drag in the trace
    $(window).resize(setContentSize)
    traceWrapper.scroll(refocusScreen)
    trace.mousedown(dragHandler)
})

