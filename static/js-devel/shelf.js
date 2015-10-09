function toggle(selector) {
  document.querySelector(selector).toggle();
}

document.querySelector('template[is=dom-bind]').isExpanded = function(opened) {
  return String(opened);
};

function inspect(node) {
    if (node.kind != "directory") {
        var url = window.location.origin + "/doc/"+node.path;
        console.log("Loading " + url);
        document.getElementById("inspector-shelf-content").src = url;
//        document.getElementById("shelf-url").innerHtml = "<pre>" + url + "</pre>";
        d3.select("#inspector-shelf-url").html("<tt>" + node.path.slice(41) + "</tt>");
//        $("#shelf-content").src = url
    } else {
        console.log("Skipping directory at " + node.path);
    }
}

function makeShelf(fullAreaWidth) {
    var width = fullAreaWidth / 2
    var shelfWidth = 0; //width / 2;
    var inspectorWidth = width; //  / 2;
    var height = 300;
    var divId = "#inspector-shelf-content";
    var dragListener = d3.behavior.drag();

    // Size the top-level container.
    d3.select("#shelfArea")
        .attr("style", "width: " + width);
    d3.select("div#shelfArea > button.heading")
        .attr("style", "width: " + width);

    // Size the individual elements.
    if (inspectorWidth > 0) {
        d3.select("#inspector-shelf-url")
            .attr("style", "width: " + inspectorWidth);
        d3.select("#inspector-shelf-content")
            .attr("style",
                  "width: " + inspectorWidth +
                  "; height: " +  height +
                  "; left: " + shelfWidth);
    }
    if (shelfWidth > 0) {
        var baseSvg = d3.select("#shelf-container").append("svg")
            .attr("width", shelfWidth)
            .attr("height", height)
            .attr("class", "shelfSvg");
    }
}

