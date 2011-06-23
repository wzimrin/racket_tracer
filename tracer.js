//William Zimrin and Jeanette Miranda
//tracer.js	6/02/2011

function highlightSpan(el,idx,span) {
    if (el.data("text")) {
        el.empty()
        el.text(el.data("text"))
        el.data("text",false)
    }
    var text = el.text()
    var startIdx = idx-1
    var endIdx = idx+span-1
    var beginText = text.substring(0,startIdx)
    var highlightedText = text.substring(startIdx,endIdx)
    var endText = text.substring(endIdx)
    el.data("text",text)
    el.empty()
    var hi = element("span")
    hi.addClass("highlight")
    hi.text(highlightedText)
    el.append(beginText,hi,endText)
}

function toInt(cssString)
{
    return parseInt(cssString.substring(0, cssString.length - 2))
}

function findPosX(obj) {
    var curleft = 0;
    if(obj.offsetParent)
        while(1) 
        {
          curleft += obj.offsetLeft;
          if(!obj.offsetParent)
            break;
          obj = obj.offsetParent;
        }
    else if(obj.x)
        curleft += obj.x;
    return curleft;
}

function findPosY(obj) {
    var curtop = 0;
    if(obj.offsetParent)
        while(1)
        {
          curtop += obj.offsetTop;
          if(!obj.offsetParent)
            break;
          obj = obj.offsetParent;
        }
    else if(obj.y)
        curtop += obj.y;
    return curtop;
}


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
    nameTD.data("idx",node.idx)
    nameTD.data("span",node.span)
    nameTD.data("linum",node.linum)
    row.append(nameTD)

    //Formals and actuals
    for (var i = 0; i < node.actuals.length; i++) {
        //Display in collapsed form if actualsExpanded is undefined or false
        var actual = makeCell(node.actualsShort[i],node.actuals[i],node.formals[i],"arg")
       /* if()
            actual.css('background', 'red')
        else
            actual.css('background', 'blue')*/
        row.append(actual)
    }

    //Arrow
    var arrow = element('td')
    arrow.html("&rarr;")
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

function refocusScreen()
{
    //Find all visible calls
    visibleCalls = $("div#tracer").find(".call").filter(":visible")

    //Check the alignment of each visible call
    visibleCalls.each(function(index) {
        var callTable = $(this).children(".callTable").first()
        var button = $(this).children(".button").first()
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
                callTable.animate({marginLeft: callTableMarL-fromLeft}, 'slow')
                button.animate({marginLeft: callTableMarL-fromLeft}, 'slow')
            }
            //This screen is to the right of the left edge of the screen
            //And not aligned with its left edge
            else if (fromLeft > 0 && callTableMarL > 0) {
                callTable.animate({marginLeft: 3}, 'slow')
                button.animate({marginLeft: 3}, 'slow')
            }
        }
    })
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

    // -------------------------------------------------------------------------
    //                                      EVENTS
    // -------------------------------------------------------------------------

    function expandCodePane() {
        setCodePaneWidth(50)
    }
    
    function setCodePaneWidth(newWidth) {
        if (codePaneWidth != newWidth) {
            codePaneWidth = newWidth
            $("div#codePane").animate({"width":newWidth+"%"},
                                      {duration:'slow'})
            
            $("div#tracerWrapper").animate({"width":(100-newWidth)+"%"},
                                           {duration:'slow'})
        }
    }
    
    $("div#codePane").click(function () {
        if (codePaneWidth==50)
            setCodePaneWidth(10)
        else
            setCodePaneWidth(50)
    })
    
    setCodePaneWidth(10)
    
    function showSpan() {
        var pane = $("div#codePane")
        var span = pane.find("span")
        var pos = span.position()
        var height = pane.height()
        var width = pane.width()
        if (pos.top < 0 || pos.top > (height-span.height()) 
            || pos.left < 0 || pos.left > (width - span.width()))
            pane.animate({scrollTop: pos.top-(height/2)+pane.scrollTop(),
                          scrollLeft: pos.left-(width/2)+pane.scrollLeft()}, 
                          'slow');
    }
    
    $("td.name").click(function () {
        var target = $(this)
        highlightSpan($("div#codePane"),target.data("idx"),target.data("span"))
        expandCodePane()
        showSpan()
    })
    
    var originalMoveInc = 30

    function scrollHelper(button,pred,max,dir,mult) {
        var t;
        var moveInc
        $(button).bind("mousedown",function () {
            function move() {
                newPos = bodies.css(dir)
                newPosInt = (parseInt(newPos.substring(0,newPos.length-2))+
                             (mult*Math.floor(moveInc)))
                if (pred(newPosInt)){
                    bodies.css(dir,newPosInt+"px")
                } else {
                    var m = max()
                    bodies.css(dir,m+"px")
                }
                moveInc = moveInc * 1.01
            }
            moveInc = originalMoveInc
            t = setInterval(move,50)
        }).bind("mouseup",function () {
            clearInterval(t)
        }).bind("mouseleave",function () {
            clearInterval(t)
        })
    }

    scrollHelper("#leftScroll",
                 function (newPosInt) {return newPosInt <= 0},
                 function () {return 0},
                 "left",1)
    scrollHelper("#rightScroll",
                 function (newPosInt) {
                     return newPosInt + bodies.width() >= bodyWrapper.width()
                 },
                 function () {return bodyWrapper.width() - bodies.width()},
                 "left",-1)
    scrollHelper("#upScroll",
                 function (newPosInt) {return newPosInt <= 0},
                 function () {return 0},
                 "top",1)
    scrollHelper("#downScroll",
                 function (newPosInt) {
                     return newPosInt + bodies.height() >= bodyWrapper.height()
                 },
                 function () {return bodyWrapper.height() - bodies.height()},
                 "top",-1)


    //makes the expand/collapse buttons work
    $('.button').bind('click',function(event) {
        thisCall = $(this).parents(".call").first()
        toggleCall(thisCall)
        if (thisCall.data("expanded")) {
            //console.log(thisCall)
            var offset = thisCall.offset()
            var wrapperOffset = bodyWrapper.offset()
            var tracerOffset = bodies.offset()
            var posX = offset.left - wrapperOffset.left
            var posY = offset.top - wrapperOffset.top
            var paneWidth = bodyWrapper.width()
            var paneHeight = bodyWrapper.height()
            var callWidth = thisCall.outerWidth()
            var callHeight = thisCall.outerHeight()
            var newX,newY
            if (posX+callWidth > paneWidth) {
                if (callWidth > paneWidth) {
                    newX = offset.left - tracerOffset.left
                } else {
                    newX = offset.left - tracerOffset.left - paneWidth + callWidth
                }
            }
            if (posY+callHeight > paneHeight) {
                if (callHeight > paneHeight) {
                    newY = offset.top - tracerOffset.top
                } else {
                    newY = offset.top - tracerOffset.top - paneHeight + callHeight
                }
            }
            if (newX)
                bodyWrapper.scrollLeft(newX)
            if (newY)
                bodyWrapper.scrollTop(newY)
        }
        refocusScreen()
        
        /*
        //want to adjust LR position of screen only if expanding
        if(thisCall.data('expanded')) { 
            posX = pos.left
            //offLeft = bodies.css('left')
            offLeftInt = bodyWrapper.scrollLeft()//parseInt(offLeft.substring(0, offLeft.length-2))
            callWidth = thisCall.width()
            widthPane = $("div#tracerWrapper").width()
            //Amount of the current call that is off the screen to the right
            callOffRight = callWidth - (widthPane-(posX+offLeftInt))
            //Call goes off the screen on the right border
            if((posX + offLeftInt) + callWidth >= widthPane) {
                //and is not as wide as the tracerWindow
                if(callWidth <= widthPane) {
                    bodyWrapper.animate({scrollLeft: -1*(offLeftInt-callOffRight-15)}, 
                                        'fast')
                }
                else if (callWidth > widthPane) {
                    bodyWrapper.animate({scrollLeft: posX}, 'fast')
                }
            }
        }
        //Want to adjust TB position of screen on both expand and collapse  
        posY = pos.top
        //offTop = bodies.css('top')
        offTopInt = bodyWrapper.scrollTop()//parseInt(offTop.substring(0, offTop.length-2))
        callHeight = thisCall.height()
        heightPane = $("div#tracerWrapper").height()
        callOffBottom = callHeight - (heightPane-(posY+offTopInt))
        //Call goes off the screen on the bottom
        if((posY + offTopInt) + callHeight >= heightPane) {
            if(callHeight <= heightPane) {
                bodyWrapper.animate({scrollTop: -1*(offTopInt-callOffBottom-15)}, 
                                    'fast')
            }
            else if (callHeight > heightPane) {
                bodyWrapper.animate({scrollTop: posY}, 'fast')
            }
        }
        else if (callOffBottom < 0 && $("div#tracer").height() >= heightPane) {
            bodyWrapper.animate({scrollTop: Math.max(0, -1*(offTopInt-callOffBottom))},
                                'slow')
        }*/
    })

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
        //target.removeClass("hover")
        $(window).scrollLeft(0)
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
        top: 5,
        left: 5,
        fade:250
    })

    first.trigger("click")
    
    
    refocusScreen(bodies, bodyWrapper)

    function setColumnHeight() {
        $(".column").height($(window).height()-$("div#tabbar").height()
                            -2*parseInt($(document.body).css("margin-top")))
    }

    setColumnHeight()
    
    $(window).resize(setColumnHeight)

})

