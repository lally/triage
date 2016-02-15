var updateSeqNo = 1;
var updateEpsilon = 0.0000000001;

var treeRoot;
var prioRegex = /P[0-9]/;
var dateRegex = /<(\d+)-(\d+)-(\d+)[^>]*>:?/
var childHeight = 24;
var childWidth = 225;
var slotHeight = 75;

function gotoUrl(url) {
    window.location = url;
}

function makeNodeSvg(d,i) {
    var enter = d3.select(this);
    var prio = '';
    // Pull out P1,P2,P3 and make them classes
    for (var i = 0; i < d.tags.length; i++) {
        if (prioRegex.test(d.tags[i])) {
            prio = d.tags[i];
            d.tags.splice(i, 1);
        }
    }

    // Pull out any date/time stamps in there.
    var dateMatch = dateRegex.exec(d.name);
    if (dateMatch == null) {
        if (d["date"] === undefined)
            d.date = null;
    } else {
        d.date = (dateMatch[2]/ 1) + "/" + (dateMatch[3]) + "/" +
            (dateMatch[1] - 2000);
        var oldname = d.name;
        d.name = d.name.split(dateMatch[0]).join("");
    }

    var clipRgn;
    var xoff = 0;
    var childrenClassification = null;
    if (d.children && d.children.length > 0) {
        childrenClassification = "childrenExpanded";
    } else if (d._children && d._children.length > 0) {
        childrenClassification = "childrenClosed";
    } else {
        childrenClassification = "noChildren";
    }
    if (d.kind == "directory") {
        xoff = 8;
        enter.append("rect")
            .attr('class', function(d) {
                return (d.kind ? 'nodeRect ' + d.kind : 'nodeRect') + ' ' +
                    prio + ' ' + childrenClassification;
            })
            .attr("width", childWidth)
            .attr("height", childHeight)
            .attr("rx", 2).attr("ry", 2)
            .attr("x", -(childWidth / 2))
            .attr("y", -(childHeight / 2));
        clipRgn = enter.append("g")
            .attr("transform", "translate(" + -(childWidth / 2) + ",0)");
        clipRgn.append("g")
            .attr("clip-path", "url(#nodeClip)")
          .append("text")
            .attr("x", "12")
            .attr("dy", ".35em")
            .attr('class', 'nodeText ' + prio)
            .attr("value",  d.name )
            .style("fill-opacity", 0);
    } else if (d.kind == "org") { // org
        xoff = 12;
        enter.append("rect")
            .attr('class', function(d) {
                return (d.kind ? 'nodeRect ' + d.kind : 'nodeRect') + ' ' + 
                    prio + ' ' + childrenClassification
            })
            .attr("width", childWidth)
            .attr("height", childHeight)
            .attr("rx", 4).attr("ry", childHeight / 2)
            .attr("x", -(childWidth / 2))
            .attr("y", -(childHeight / 2));
        clipRgn = enter.append("g")
            .attr("transform", "translate(" + -(childWidth / 2) + ",0)");
        clipRgn.append("g")
            .attr("clip-path", "url(#nodeClip)")
          .append("text")
            .attr("x", "16")
            .attr("dy", ".35em")
            .attr('class', 'nodeText ' + prio)
            .attr("value", d.name )
            .style("fill-opacity", 0);

    } else { // issue?
        xoff=12;
        var box = enter.append("g")
                    .attr("transform",
                          "translate(-"+(childWidth/2)+",-1040)");
        box.append("path")
          .attr("class", 'ticketBox ' + prio + ' ' + childrenClassification)
            .attr("d","m 4.1745156,1028.3331 c -0.9932588,0.8752 -2.4937368,1.5954 -4.28771162,2.0836 l 0,20.2272 c 1.74209712,0.4738 3.20153382,1.1815 4.19382012,2.0227 l 141.7448559,0 c 0.99365,-0.8756 2.49279,-1.5954 4.28772,-2.0835 l 0,-20.1817 c -1.79172,-0.4847 -3.25888,-1.1977 -4.25642,-2.0683 l -141.6822644,0 z")
          .attr("transform", "scale(1.5,1)");

        clipRgn = enter.append("g")
            .attr("transform", "translate(" + -(childWidth/2) + ",0)");

        var clippedRgn = clipRgn.append("g")
            .attr("clip-path", "url(#nodeClip)");
        clippedRgn.append("text")
            .attr("x", 16)
            .attr("dy", ".35em")
            .attr('class', 'ticketText ' + prio)
            .text( d.name )
            .style("fill-opacity", 0);

        // Put an origin/num identifier up top.
        clippedRgn.append("text")
            .attr("class", "issueOrigin " + prio)
            .attr("text-anchor", "end")
            .attr("x", childWidth - 8)
            .attr("y", -7)
            .text(d.value.type + ":" + d.value.origin + "#" + d.value.number);

    }
    // Put any Open/TODO tags on the left.
    if (d.state.length > 0) {
        var box = clipRgn.append("g").attr("transform",
                                           "translate(" + xoff + ",25)");
        box.append("rect")
            .attr("class", "state " + d.state)
            .attr("x", "-8")
            .attr("y", "-36")
            .attr("width","10px")
            .attr("height", "22px")
            .style("fill: red");

        box.append("text")
            .attr("transform","matrix(0,-1,1,0,0,0)")
            .attr("class", "state " + d.state)
            .attr("x", "16px")
            .attr("y", 0)
            .text(d.state);
    }
    // Put a date annotation on the box.
    if (d.date != null) {
        var box = clipRgn.append("g").attr("transform",
                                           "translate(" + xoff + ", 0)");
        box.append("rect")
            .attr("class", "dueDate")
            .attr("x", "-12")
            .attr("y", "-20")
            .attr("width","40");

        box.append("text")
            .attr("class", "dueDate")
            .attr("x", "-8")
            .attr("y", "-10")
            .text(d.date);
    }

    // Put tags on the bottom.
    var prev_offset = 4;
    for (var i in d.tags) {
        if (prev_offset < childWidth) {
            var lastGrp = clipRgn.append("g");
            var lastRect = lastGrp.append("rect")
                .attr("class", "ticketTag")
                .attr("height", "6px")
                .attr("x", "-1px")
                .attr("y", "-6px");

            var last = lastGrp.append("text")
                .attr("class", "ticketTag " + d.tags[i])
                .attr("y", "-1px")
                .text(d.tags[i]);

            var cur_width = last.node().getBBox().width + 2;
            var cur_offset = prev_offset + cur_width;
            lastRect.attr("width", cur_width);
            lastGrp.attr("transform", "translate(" +
                         (childWidth - cur_offset) + "," +
                         childHeight/2 + ")");
            if (childWidth - cur_offset < 0) {
                last.attr("style", "display: none");
                lastRect.attr("style", "display: none");
            }
            prev_offset += cur_width + 1;
        }
    }

}

function extractPath(url) {
    if (url == null) return 0;
    var afterHash = url.split("#");
    if (afterHash.length > 1 && afterHash[1].length > 0) {
        var pathElems = afterhash[1].split(",");
        if (pathElems[pathElems.length-1].length > 0) {
            return parseInt(pathElems[pathElems.length-1]);
        }
    }
    return 0;
}

function prefixPath(url) {
    if (url == null) return 0;
    var afterHash = url.split("#");
    if (afterHash.length > 1 && afterHash[1].length > 0) {
        var pathElems = afterhash[1].split(",");
        if (pathElems[pathElems.length-1].length > 0) {
            pathElems = pathElems.splice(pathElems.length -1,1);
        }
        return afterHash + "#" + pathElems.join(",");
    }
    return url;
}

function nextEpsilon() {
    updateSeqNo++;
    return updateSeqNo * updateEpsilon;
}

function compareNodes(a, b) {
    return comparePaths(b.path,a.path);
}

function comparePaths(a, b) {
    if (a.split("=")[1].length < 1 &&
        b.split("=")[1].length < 1) {
        return a < b;
    }
    var apathElems = a.split("=")[1].split(",");
    var bpathElems = b.split("=")[1].split(",");
    for (var i = 0; i < apathElems.length; i++) {
        if (apathElems[i] != bpathElems[i]) {
            var result = parseInt(apathElems[i]) < parseInt(bpathElems[i]);
            return result;
        }
    }
    return false;
}

function addChildBetween(prev, next) {
    var prev_nr = extractPath(prev);
    var next_nr = extractPath(next);
    var midpoint = (prev_nr + next_nr) / 2 + nextEpsilon();
    var prefix;
    if (prev == null) {
        prefix = prefixPath(next);
    } else if (next == null) {
        prefix = prefixPath(prev);
    } else if (prefixPath(prev) != prefixPath(next)) {
        throw ("Invalid prefix paths! ["+prev+"] vs ["+next+"]");
    }
    return prefix + "," + midpoint;
}

function addChildAfter(parent) {
/*    if (parent.children || parent._children) {
        throw ("Node has children, yet calling addChildAfter!");
    } */
    return parent + "," + nextEpsilon();
}

// A recursive helper function for performing some setup by
// walking through all nodes
function visit(parent, visitFn, childrenFn, num) {
    if (num === undefined) {
        num = 1;
    }

    if (!parent) return;

    visitFn(parent, num);

    var children = childrenFn(parent, num);
    if (children) {
        var count = children.length;
        for (var i = 0; i < count; i++) {
            visit(children[i], visitFn, childrenFn, num+1);
        }
    }
}

function getDeltas() {
    var resultList = []
    visit(treeRoot, function(d) {
        if (d.startPath != d.path) {
            resultList.push([d.startPath, d.path]);
        }},
          function(d) {
              if (d.children && d.children.length > 0) {
                  return d.children;
              } else if (d._children && d._children.length > 0) {
                  return d._children;
              } else {
                  return null;
              }
          });
    return resultList;
}

// Processes JSON data for the graph.
function setupTree(error, treeData) {
    // Calculate total nodes, max label length
    var totalNodes = 0;
    var maxLabelLength = 0;
    // variables for drag/drop
    var selectedNode = null;
    var draggingNode = null;
    // panning variables
    var panSpeed = 200;
    var panBoundary = 20; // Within 20px from edges will pan when dragging.
    // Misc. variables
    var i = 0;
    var duration = 750;
    var root;

    // size of the diagram
    var viewerWidth = $(document).width();
    var viewerHeight = $(document).height() - 64;  // take off the top-bar.

    makeShelf(viewerWidth);

    var tree = d3.layout.tree()
        .size([viewerHeight, viewerWidth]);

    treeRoot = treeData;

    // define a d3 diagonal projection for use by the node paths later on.
    // This still arcs them the wrong way.
    var diagonal = d3.svg.diagonal()
            .source(function(d) { return {"x":d.source.x,// + (childWidth /2),
                                          "y":d.source.y + (childWidth / 2)}; })
            .target(function(d) { return {"x":d.target.x, // - (childWidth / 2),
                                          "y":d.target.y - (childWidth /2)}; })
            .projection(function(d) { return [d.y, d.x]; });

    // Save the original path here.
    visit(treeData, function(d, num) {
        d.startPath = d.path;
        d.depth = num;
    }, function(d) {
        if (d.children && d.children.length > 0) {
            return d.children;
        } else if (d._children && d._children.length > 0) {
            return d._children;
        } else {
            return null; 
        }
    });

    // Call visit to close nodes with many children, or nodes that are
    // DONE or CLOSED
    visit(treeData, function(d, n) {
        if (n == 1 && d.children && d.children.length > 6) {
            // Special case for root: just show children.
            if (d.children && d.children.length > 0) {
                for (var i = 0; i < d.children.length; i++) {
                    collapse(d.children[i]);
                }
            }
        } else if (( n >= 4 && d.children && d.children.length > 6)
                   || d.state == "DONE" || d.state == "CLOSED"
                   || d.tags.indexOf("ARCHIVE") >= 0 || d.tags.indexOf("IGNORE") >= 0
                   || n > 6) {
            collapse(d);
        }
    }, function(d) {
        return d.children && d.children.length > 0 ? d.children : null;
    });

    // Call visit function to establish maxLabelLength
    visit(treeData, function(d) {
        totalNodes++;
        maxLabelLength = Math.max(d.name.length, maxLabelLength);
    }, function(d) {
        return d.children && d.children.length > 0 ? d.children : null;
    });


    // sort the tree according to the node names
    function sortSubtree(subtree) {
        visit(subtree, function(d) {
            if (d.children) {
                d.children.sort(compareNodes);
            }
        }, function(d) {
            return d.children && d.children.length > 0 ? d.children : null;
        });
    }

    function sortTree() {
        sortSubtree(treeData);
    }

    // Sort the tree initially incase the JSON isn't in a sorted order.
    sortTree();

    // TODO: Pan function, can be better implemented.
    function pan(domNode, direction) {
        var speed = panSpeed;
        if (panTimer) {
            clearTimeout(panTimer);
            // tCs = translateCoords
            tCs = d3.transform(svgGroup.attr("transform"));
            if (direction == 'left' || direction == 'right') {
                tX = direction == 'left' ? 
                    tCs.translate[0] + speed : tCs.translate[0] - speed;
                tY = tCs.translate[1];
            } else if (direction == 'up' || direction == 'down') {
                tX = tCs.translate[0];
                tY = direction == 'up' ? 
                    tCs.translate[1] + speed : tCs.translate[1] - speed;
            }
            scaleX = tCs.scale[0];
            scaleY = tCs.scale[1];
            scale = zoomListener.scale();
            svgGroup.transition().attr("transform", "translate(" + tX + "," + tY + 
                                       ")scale(" + scale + ")");
            d3.select(domNode).select('g.node').attr("transform", "translate(" + tX + 
                                                     "," + tY + ")");
            zoomListener.scale(zoomListener.scale());
            zoomListener.translate([tX, tY]);
            panTimer = setTimeout(function() {
                pan(domNode, speed, direction);
            }, 50);
        }
    }

    // Define the zoom function for the zoomable tree
    function zoom() {
        svgGroup.attr("transform", "translate(" + d3.event.translate +
                      ")scale(" + d3.event.scale + ")");
    }


    // define the zoomListener which calls the zoom function on the
    // "zoom" event constrained within the scaleExtents
    var zoomListener = d3.behavior.zoom().scaleExtent([0.1, 3]).on("zoom", zoom);

    function initiateDrag(d, domNode) {
        draggingNode = d;
        d3.select(domNode).select('.ghostCircle').attr('pointer-events', 'none');
        d3.selectAll('.ghostCircle').attr('class', 'ghostCircle show');
        d3.select(domNode).attr('class', 'node activeDrag');

        // select the parent and sort the path's
        svgGroup.selectAll("g.node").sort(function(a, b) {
            // a is not the hovered element, send "a" to the back
            if (a.id != draggingNode.id) return 1;
            else return -1; // a is the hovered element, bring "a" to the front
        });
        // if nodes has children, remove the links and nodes
        if (nodes.length > 1) {
            // remove link paths
            links = tree.links(nodes);
            nodePaths = svgGroup.selectAll("path.link")
                .data(links, function(d) {
                    return d.target.id;
                }).remove();
            // remove child nodes
            nodesExit = svgGroup.selectAll("g.node")
                .data(nodes, function(d) {
                    return d.id;
                }).filter(function(d, i) {
                    if (d.id == draggingNode.id) {
                        return false;
                    }
                    return true;
                }).remove();
        }

        // remove parent link
        parentLink = tree.links(tree.nodes(draggingNode.parent));
        svgGroup.selectAll('path.link').filter(function(d, i) {
            if (d.target.id == draggingNode.id) {
                return true;
            }
            return false;
        }).remove();

        dragStarted = null;
    }

    // define the baseSvg, attaching a class for styling and the zoomListener
    var topSvg = d3.select("#tree-container").append("svg")
        .attr("width", viewerWidth)
        .attr("height", viewerHeight)
        .attr("class", "overlay")
        .call(zoomListener);
    var shelfTopLevelContainer = topSvg.append("g").attr("id", "shelf-container");

    // Append a group which holds all nodes and which the zoom Listener can act upon.
    var svgGroup = topSvg.append("g");

    var clip = topSvg.append("svg:clipPath")
          .attr("id", "nodeClip")
        .append("svg:rect")
          .attr('x', -1).attr('y',-12)
          .attr('width', childWidth - 6)
          .attr('height', childHeight - 4 );

    // Define the drag listeners for drag/drop behaviour of nodes.
    dragListener = d3.behavior.drag()
        .on("dragstart", function(d) {
            if (d == root) {
                return;
            }
            dragStarted = true;
            nodes = tree.nodes(d);
            d3.event.sourceEvent.stopPropagation();
            // it's important that we suppress the mouseover event on
            // the node being dragged. Otherwise it will absorb the
            // mouseover event and the underlying node will not detect
            // it d3.select(this).attr('pointer-events', 'none');
        })
        .on("drag", function(d) {
            if (d == root) {
                return;
            }
            if (dragStarted) {
                domNode = this;
                initiateDrag(d, domNode);
            }

            // get coords of mouseEvent relative to svg container to allow for panning
            relCoords = d3.mouse($('svg').get(0));
            d.x0 += d3.event.dy;
            d.y0 += d3.event.dx;
            var node = d3.select(this);
            node.attr("transform", "translate(" + d.y0 + "," + d.x0 + ")");
            updateTempConnector();
        }).on("dragend", function(d) {
            if (d == root) {
                return;
            }
            domNode = this;
            if (selectedNode) {
                // now remove the element from the parent, and insert
                // it into the new elements children
                var index = draggingNode.parent.children.indexOf(draggingNode);
                if (index > -1) {
                    draggingNode.parent.children.splice(index, 1);
                }
                if (typeof selectedNode.children !== 'undefined' ||
                    typeof selectedNode._children !== 'undefined') {
                    if (typeof selectedNode.children !== 'undefined') {
                        if (selectedNode.children.length > 0) {
                            draggingNode.path =
                                addChildBetween(
                                    selectedNode.children[
                                        selectedNode.children.length-1].path,
                                    null);
                        } else {
                            draggingNode.path = addChildAfter(selectedNode.path);
                        }
                        selectedNode.children.push(draggingNode);
                    } else {
                        if (selectedNode._children.length > 0) {
                            draggingNode.path =
                                addChildBetween(
                                    selectedNode._children[
                                        selectedNode._children.length-1].path,
                                    null);
                        } else {
                            draggingNode.path = addChildAfter(selectedNode.path);
                        }
                        selectedNode._children.push(draggingNode);
                    }
                } else {
                    draggingNode.path = addChildAfter(selectedNode.path);
                    selectedNode.children = [];
                    selectedNode.children.push(draggingNode);
                }
                // Make sure that the node being added to is expanded so user
                // can see added node is correctly moved
                expand(selectedNode);
                sortTree();
                endDrag();
            } else {
                endDrag();
            }
        });

    function endDrag() {
        selectedNode = null;
        d3.selectAll('.ghostCircle').attr('class', 'ghostCircle');
        d3.select(domNode).attr('class', 'node');
        // now restore the mouseover event or we won't be able to drag a 2nd time
        d3.select(domNode).select('.ghostCircle').attr('pointer-events', '');
        updateTempConnector();
        if (draggingNode !== null) {
            update(root);
            centerNode(draggingNode);
            draggingNode = null;
        }
    }

    // Helper functions for collapsing and expanding nodes.
    function collapse(d) {
        if (d.children) {
            d._children = d.children;
            d._children.forEach(collapse);
            d.children = null;
        }
    }

    function expand(d) {
        if (d._children) {
            d.children = d._children;
            sortSubtree(d);
            d.children.forEach(expand);
            d._children = null;
        }
    }

    var overCircle = function(d) {
        selectedNode = d;
        updateTempConnector();
    };
    var outCircle = function(d) {
        selectedNode = null;
        updateTempConnector();
    };

    // Function to update the temporary connector indicating dragging affiliation
    var updateTempConnector = function() {
        var data = [];
        if (draggingNode !== null && selectedNode !== null) {
            // have to flip the source coordinates since we did this for the
            // existing connectors on the original tree
            data = [{
                source: {
                    x: selectedNode.y0,
                    y: selectedNode.x0
                },
                target: {
                    x: draggingNode.y0,
                    y: draggingNode.x0
                }
            }];
        }
        var link = svgGroup.selectAll(".templink").data(data);

        link.enter().append("path")
            .attr("class", "templink")
            .attr("d", d3.svg.diagonal())
            .attr('pointer-events', 'none');

        link.attr("d", d3.svg.diagonal());

        link.exit().remove();
    };

    // Function to center node when clicked/dropped so node doesn't
    // get lost when collapsing/moving with large amount of children.
    function centerNode(source) {
        scale = zoomListener.scale();
        x = -source.y0;
        y = -source.x0;
        x = x * scale + viewerWidth / 2;
        y = y * scale + viewerHeight / 2;
        d3.select('g').transition()
            .duration(duration)
            .attr("transform", "translate(" + x + "," + y + ")scale(" + scale + ")");
        zoomListener.scale(scale);
        zoomListener.translate([x, y]);
    }

    // Function to move node to vertical center of left side.
    function leftCenterNode(source) {
        scale = zoomListener.scale();
        console.log("leftCenterNode: scale=" + JSON.stringify (scale) + 
                    ", source.0s=[" + source.x0 + ", " + source.y0 + "]");
        x = -source.y0;
        y = -source.x0;
        x = (x * scale) + childWidth / 2 + 20;
        y = (y * scale) + viewerHeight / 2;
        d3.select('g').transition()
            .duration(duration)
            .attr("transform", "translate(" + x + "," + y + ")scale(" + scale + ")");
        console.log("leftCenterNode: translate("+x+","+y+"), scale("+scale+")");
        zoomListener.scale(scale);
        zoomListener.translate([x, y]);
    }

    // Toggle children function
    function toggleChildren(d) {
        if (d.children) {
            d._children = d.children;
            d.children = null;
        } else if (d._children) {
            d.children = d._children;
            d._children = null;
            sortSubtree(d);
        }
        return d;
    }

    // Toggle children on click.
    function click(d) {
        if (d3.event.defaultPrevented) return; // click suppressed
        d = toggleChildren(d);
        inspect(d);
        update(d);
    }

    function update(source) {
        // Compute the new height, function counts total children of
        // root node and sets tree height accordingly.  This prevents
        // the layout looking squashed when new nodes are made visible
        // or looking sparse when nodes are removed. This makes the
        // layout more consistent.
        var levelWidth = [1];
        var childCount = function(level, n) {
            if (n.children && n.children.length > 0) {
                if (levelWidth.length <= level + 1) levelWidth.push(0);

                levelWidth[level + 1] += n.children.length;
                n.children.forEach(function(d) {
                    childCount(level + 1, d);
                });
            }
        };
        childCount(0, root);
        var newHeight = d3.max(levelWidth) * slotHeight;
        tree = tree.size([newHeight, viewerWidth]);

        // TODO: update the 'class' attr of 'source' in terms of
        // childrenClassification.  Probably just pull out the logic of
        // determining the node's class attr into a funciton and invoke it
        // here.

        // Compute the new tree layout.
        var nodes = tree.nodes(root).reverse(),
            links = tree.links(nodes);

        // Set widths between levels based on maxLabelLength.
        nodes.forEach(function(d) {
            // d.y = (d.depth * (maxLabelLength * 10)); //maxLabelLength * 10px
            // alternatively to keep a fixed scale one can set a fixed depth per level
            // Normalize for fixed-depth by commenting out below line
            d.y = (d.depth * (1.5 * childWidth)); //500px per level.
        });

        // Update the nodes…
        node = svgGroup.selectAll("g.node")
            .data(nodes, function(d) {
                return d.id || (d.id = ++i);
            });

        // Enter any new nodes at the parent's previous position.
        var nodeEnter = node.enter().append("g")
            .call(dragListener)
            .attr("class", "node")
            .attr("transform", function(d) {
                return "translate(" + source.y0 + "," + source.x0 + ")";
            })
            .on('click', click);

        // TODO(lally): The selection object here will be 'this' in
        // either call() or each() (I think the latter).  Look at the
        // type inolved and format it differently.
        nodeEnter.each(makeNodeSvg);

        // Set widths between levels based on maxLabelLength.
        nodes.forEach(function(d) {
            // d.y = (d.depth * (maxLabelLength * 10)); //maxLabelLength * 10px
            // alternatively to keep a fixed scale one can set a fixed depth per level
            // Normalize for fixed-depth by commenting out below line
            d.y = (d.depth * (1.5 * childWidth)); //500px per level.
        });


        // phantom node to give us mouseover in a radius around it
        nodeEnter.append("circle")
            .attr('class', 'ghostCircle')
            .attr("r", 30)
            .attr("opacity", 0.2) // change this to zero to hide the target area
        .style("fill", "red")
            .attr('pointer-events', 'mouseover')
            .on("mouseover", function(node) {
                overCircle(node);
            })
            .on("mouseout", function(node) {
                outCircle(node);
            });

        // Update the text to reflect whether node has children or not.
        node.select('text')
            .text(function(d) {
                return d.name;
            });

        // Change the circle fill depending on whether it has children and is collapsed
        node.select("circle.nodeCircle")
            .attr("r", 4.5)
            .style("fill", function(d) {
                return d._children ? "lightsteelblue" : "#fff";
            });

        // Transition nodes to their new position.
        var nodeUpdate = node.transition()
            .duration(duration)
            .attr("transform", function(d) {
                return "translate(" + d.y + "," + d.x + ")";
            });

        // Fade the text in
        nodeUpdate.select("text")
            .style("fill-opacity", 1);

        // Transition exiting nodes to the parent's new position.
        var nodeExit = node.exit().transition()
            .duration(duration)
            .attr("transform", function(d) {
                return "translate(" + source.y + "," + source.x + ")";
            })
            .remove();

        nodeExit.select("circle")
            .attr("r", 0);

        nodeExit.select("rect")
            .attr("width", 0)
            .attr("height", 0);

        nodeExit.select("text")
            .style("fill-opacity", 0);

        // Update the links…
        var link = svgGroup.selectAll("path.link")
            .data(links, function(d) {
                return d.target.id;
            });


        // Enter any new links at the parent's previous position.
        link.enter().insert("path", "g")
            .attr("class", "link")
            .attr("d", function(d) {
                var o = {
                    x: source.x0,
                    y: source.y0
                };
                return diagonal({
                    source: o,
                    target: o
                });
            });


        // Transition links to their new position.
        link.transition()
            .duration(duration)
            .attr("d", diagonal);

        // Transition exiting nodes to the parent's new position.
        link.exit().transition()
            .duration(duration)
            .attr("d", function(d) {
                var o = {
                    x: source.x,
                    y: source.y
                };
                return diagonal({
                    source: o,
                    target: o
                });
            })
            .remove();

        // Stash the old positions for transition.
        nodes.forEach(function(d) {
            d.x0 = d.x;
            d.y0 = d.y;
        });
    }

    // Define the root
    root = treeData;
    root.x0 = viewerHeight / 2;
    root.y0 = 0;

    // Layout the tree initially and center on the root node.
    update(root);
    leftCenterNode(root);
};

