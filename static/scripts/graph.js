
define(
        ["scripts/lib/d3.js","jquery"],
        function(d3, $) {
        return {
        fdg: function(el) {

    // Add and remove elements on the graph object
    this.addNode = function (node) {
        if (!findNode(node.name)) {
        node.id = node.name;
        nodes.push(node);
        update();
        }
    }

    this.removeNode = function (id) {
        var i = 0;
        var n = findNode(id);
        while (i < links.length) {
            if ((links[i]['source'] === n)||(links[i]['target'] == n)) links.splice(i,1);
            else i++;
        }
        var index = findNodeIndex(id);
        if(index !== undefined) {
            nodes.splice(index, 1);
            update();
        }
    }

    this.addLink = function (link) {
        var sourceNode = findNode(link.source);
        var targetNode = findNode(link.target);

        if((sourceNode !== undefined) && (targetNode !== undefined)) {
            links.push({"source": sourceNode, "target": targetNode});
            update();
        }
    }

    var findNode = function (id) {
        for (var i=0; i < nodes.length; i++) {
            if (nodes[i].id === id)
                return nodes[i]
        };
        return false;
    }

    var findNodeIndex = function (id) {
        for (var i=0; i < nodes.length; i++) {
            if (nodes[i].id === id)
                return i
        };
    }

    // set up the D3 visualisation in the specified element
    var w = d3.select("svg").attr("width"),
        h = d3.select("svg").attr("height");

    var vis = this.vis = d3.select(el).append("g")
        .attr("width", w)
        .attr("height", h);

    var links = vis.append("g").attr("id", "links");
    var nodes = vis.append("g").attr("id", "nodes");

    var force = d3.layout.force()
        //.gravity(.05)
        //.distance(50)
        .charge(-120)
        .linkDistance(30)
        .size([w, h]);


    var nodes = force.nodes(),
        links = force.links();

    var update = function () {

        var link = vis.select("#links").selectAll("line.link")
            .data(links, function(d) { return d.source.id + "-" + d.target.id; });

        link.enter().insert("line")
            .attr("class", "link");

        link.exit().remove();

        var node = vis.select("#nodes").selectAll("g.node")
            .data(nodes, function(d) { return d.id;});

        var drag = force.drag()
                .on("dragstart", function() { d3.event.sourceEvent.stopPropagation(); });

        var nodeEnter = node.enter().append("g")
            .attr("class", "node")
            .call(drag);

        nodeEnter.append("text")
            .attr("class", "nodetext")
            .attr("dx", 6)
            .attr("dy", ".35em")
            .text(function(d) {return d.name});
        
        nodeEnter.append("circle")
                .attr("class", "node")
                .attr("class", "handle")
      .attr("r", 5);

//        nodeEnter.append("image")
//            .attr("class", "circle")
//            .attr("xlink:href", "https://d3nwyuy0nl342s.cloudfront.net/images/icons/public.png")
//            .attr("x", "-8px")
//            .attr("y", "-8px")
//            .attr("width", "16px")
//            .attr("height", "16px");


        node.exit().remove();

        force.on("tick", function() {
          link.attr("x1", function(d) { return d.source.x; })
              .attr("y1", function(d) { return d.source.y; })
              .attr("x2", function(d) { return d.target.x; })
              .attr("y2", function(d) { return d.target.y; });

          node.attr("transform", function(d) { return "translate(" + d.x + "," + d.y + ")"; });
        });

        // Restart the force layout.
        force.start();
    }

    // Make it all go
    update();
    }
  }
});
