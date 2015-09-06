define(["scripts/lib/d3.js","jquery", "underscore"],
        function(d3, $, _) {
        return {
        fdg: function(el) {

    // Add and remove elements on the graph object
    this.addNode = function (node) {
        if (!this.findNode(node.name)) {
            node.id = node.name;
            node.children = [];
            nodes.push(node);
            update();
        }
    }

    this.updateNode = function (id, newinfo) {
        var n = this.findNode(id);
        if (!n) {
            console.log("Can't update "+ id);
            return;
        }
        _.extend(n, newinfo);
    }

    this.removeNode = function (id) {
        var i = 0;
        var n = this.findNode(id);
        while (i < links.length) {
            if ((links[i]['source'] === n)||(links[i]['target'] == n)) links.splice(i,1);
            else i++;
        }
        var index = this.findNodeIndex(id);
        if(index !== undefined) {
            nodes.splice(index, 1);
            update();
        }
    }

    this.addLink = function (link) {
        var sourceNode = this.findNode(link.source.name);
        var targetNode = this.findNode(link.target.name);

        if ((sourceNode === false) && (targetNode !== undefined)) {
            this.addChild(targetNode, link.source);
            sourceNode = this.findNode(link.source.name);
        }

        if ((sourceNode !== undefined) && (targetNode === false)) {
            this.addChild(sourceNode, link.target);
            targetNode = this.findNode(link.target.name);
        }
        var link = {"source":sourceNode, "target": targetNode};

        if (this.findLink(link)) {
            return;
        }

        if((sourceNode !== undefined) && (targetNode !== undefined)) {
            links.push({"source": sourceNode, "target": targetNode});
            update();
        }
    }

    this.addChild = function(parentNode, childInfo) {
            var newNode = _.defaults({
                  parent: parentNode,
                  x: parentNode.x + 5,
                  y: parentNode.y - (10*parentNode.children.length),
                  charge: 0}, childInfo);
            this.addNode(newNode);
            parentNode.children.push(newNode);
    }

    this.findLink = function(link) {
        for (var i=0; i < links.length; i++) {
            if ((links[i].source.id === link.source.id
                    && links[i].target.id === link.target.id) ||
                (links[i].target.id === link.source.id && links[i].source.id === link.target.id)) {
                return links[i];
            }
        }
        return false;
    }

    this.findNode = function (id) {
        for (var i=0; i < nodes.length; i++) {
            if (nodes[i].id === id)
                return nodes[i];
        }
        return false;
    }

    this.findNodeIndex = function (id) {
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

    vis.append("g").attr("id", "links");
    vis.append("g").attr("id", "nodes");

    var maxCharge = -120;

    var chargeFunc = function (d) {
            if (d.charge === undefined) {
                return maxCharge;
            }

            if (d.charge > maxCharge) {
               d.charge = d.charge - 5;
               return d.charge;
            } else if (d.charge < maxCharge) {
               d.charge = maxCharge;
            }
            return d.charge;
        };

    var force = d3.layout.force()
        //.gravity(.05)
        //.distance(50)
        .charge(chargeFunc)
        .linkDistance(30)
        .size([w, h]);

    this.force = force;


    var nodes = this.nodes = force.nodes(),
        links = this.links = force.links();

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


        var xFunc = function(d) {
            if (d.parent !== undefined) {
                return d.parent.x;
            }
            return 0;
        }
        var nodeEnter = node.enter().append("g")
            .attr("class", "node")
   //         .attr("x", xFunc)
     //       .attr("y", function(d) { if (d.parent !== undefined) return d.parent.y;})

            .call(drag);
        nodeEnter.append("svg:image")
           .attr('x',-9)
           .attr('y',-12)
           .attr('width', 20)
           .attr('height', 24)
           .attr("class", "node-image")
           .attr("xlink:href", function (d) {return d.pic || "https://s3.amazonaws.com/venmo/no-image.gif"});


        nodeEnter.append("text")
            .attr("class", "nodetext")
            .attr("dx", -8)
            .attr("dy", 14)
            .text(function(d) {return d.fullname || d.name});


        var nodeUpdate = node;

        nodeUpdate.selectAll("text")
            .attr("class", "nodetext")
            .attr("dx", -8)
            .attr("dy", 14)
            .text(function(d) {return d.fullname || d.name});

        nodeUpdate.selectAll(".node-image")
            .attr('x',-9)
           .attr('y',-12)
           .attr('width', 20)
           .attr('height', 24)
           .attr("xlink:href", function (d) {return d.pic || "https://s3.amazonaws.com/venmo/no-image.gif"})
//
//        nodeEnter.append("circle")
//                .attr("class", "node")
//                .attr("class", "handle")
//      .attr("r", 5);

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
