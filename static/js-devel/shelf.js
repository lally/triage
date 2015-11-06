// Some logic to avoid fetching info on nodes when the shelf is closed.

var shelf_opened = false;
var inspected_node = undefined;

function setShelfContent(node) {
    if (node && node !== undefined && node !== null) {
        var path = node.startPath;
        var url = window.location.origin + "/doc/"+path;
        console.log("Loading " + node);
        if (path) {
            document.getElementById("inspector-shelf-content").src = url;
            // strip off the "SHA/" on the front
            d3.select("#inspector-shelf-url").html("<tt>" + path.slice(41) + "</tt>");
        }
    } else {
        d3.select("#inspector-shelf-content").innerHtml = "<b>No node selected.</b>"
    }
}

function toggle(selector) {
  document.querySelector(selector).toggle();
  // crazy hacky
  if (selector == "#shelf") {
      shelf_opened = !shelf_opened;
      if (shelf_opened) {
          setShelfContent(node);
      }
  }
}

document.querySelector('template[is=dom-bind]').isExpanded = function(opened) {
  return String(opened);
};

function inspect(node) {
    if (shelf_opened && node !== undefined && node !== null) {
        if (node.kind != "directory") {
            setShelfContent(node);
        } else {
            console.log("Skipping directory at " + node.path);
        }
    }
    inspected_node = node;
}

function dropShelf(event) { console.log("dropShelf-new-(",event,")"); }

function onClick(event) { console.log("onClick-new-(",event,")"); }
function onMouseDown(event) { console.log("onMouseDown-new-(",event,")"); }
function onMouseUp(event) { console.log("onMouseUp-new-(",event,")"); }
function onMouseOver(event) { console.log("onMouseOver-new-(",event,")"); }
function onMouseMove(event) { console.log("onMouseMove-new-(",event,")"); }
function onMouseOut(event) { console.log("onMouseOut-new-(",event,")"); }

/*
click
Occurs when the pointing device button is clicked over an element. A click is defined as a mousedown and mouseup over the same screen location. The sequence of these events is: mousedown, mouseup, click. If multiple clicks occur at the same screen location, the sequence repeats with the detail attribute incrementing with each repetition.
(same)	MouseEvent	onclick
mousedown
Occurs when the pointing device button is pressed over an element.
(same)	MouseEvent	onmousedown
mouseup
Occurs when the pointing device button is released over an element.
(same)	MouseEvent	onmouseup
mouseover
Occurs when the pointing device is moved onto an element.
(same)	MouseEvent	onmouseover
mousemove
Occurs when the pointing device is moved while it is over an element.
(same)	MouseEvent	onmousemove
mouseout
Occurs when the pointing device is moved away from an element.
(same)	MouseEvent	onmouseout
*/

function makeShelf(fullAreaWidth) {
    var width = fullAreaWidth / 2
    var shelfWidth = width / 2;
    var inspectorWidth = width / 2;
    var height = 300;
    var divId = "#inspector-shelf-content";
    var dragListener = d3.behavior.drag();

    // Size the top-level container.
    d3.select("#shelfArea")
        .attr("style", "width: " + width);
    d3.select("div#shelfArea > button.heading")
        .attr("style", "width: " + width);

    // Size the individual elements.
    if (shelfWidth > 0) {
        var baseSvg = d3.select("#shelf-container").append("svg")
            .attr("width", shelfWidth)
            .attr("height", height)
            .attr("class", "shelfSvg")
            .attr("ondragover", "dropShelf(event)")
            .attr("onclick", "onClick(event)")
            .attr("onmousedown", "onMouseDown(event)")
            .attr("onmouseup", "onMouseUp(event)")
            .attr("onmouseover", "onMouseOver(event)")
            .attr("onmousemove", "onMouseMove(event)")
            .attr("onmouseout", "onMouseOut(event)")

    }
    if (inspectorWidth > 0) {
        d3.select("#inspector")
            .attr("style",
                  "width: " + inspectorWidth +
                  "; height: " +  height +
                  "; left: " + shelfWidth);
        d3.select("#inspector-shelf-url")
            .attr("style",
                  "width: " + inspectorWidth +
                  "; height: " + 20);
        d3.select("#inspector-shelf-content")
            .attr("style",
                  "width: " + inspectorWidth +
                  "; height: " +  (height-20));
    }
    setShelfContent(null);
}

