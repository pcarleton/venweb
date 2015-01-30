require.config({
    paths: {
        underscore: 'http://underscorejs.org/underscore',
        jquery: 'http://code.jquery.com/jquery-1.11.2.min'
    },
    shim: {
        "underscore": {
            exports: "_"
        }
    }
});


require(
        ["scripts/lib/d3.js", "underscore", "jquery"],
        function(d3, _, $) {

var width = 600,
    height = 600;

var color = d3.scale.category20();

var force = d3.layout.force()
    .charge(-120)
    .linkDistance(30)
    .size([width, height]);

var svg = d3.select("body").append("svg")
    .attr("width", width)
    .attr("height", height);

var query = window.location.search.substring(1);
var pieces = query.split("=");

window.fetchfn = function() {
   d3.xhr("/expand")
    .header("Content-Type", "application/json")
    .post(
        JSON.stringify(window.graphson),
        function(err, rawData){
            console.log("got response", rawData);
            var data = JSON.parse(rawData.response);
            renderFn({}, data);
            window.graphson = data;
        }
    );
};

var graphson = {};

function myGraph(el) {

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
        var sourceNode = nodes[link.source];
        var targetNode = nodes[link.target];

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
    var w = $(el).innerWidth(),
        h = $(el).innerHeight();

    var vis = this.vis = d3.select(el).append("svg:svg")
        .attr("width", w)
        .attr("height", h);

    var force = d3.layout.force()
        .gravity(.05)
        .distance(100)
        .charge(-100)
        .size([w, h]);

    var nodes = force.nodes(),
        links = force.links();

    var update = function () {

        var link = vis.selectAll("line.link")
            .data(links, function(d) { return d.source.id + "-" + d.target.id; });

        link.enter().insert("line")
            .attr("class", "link");

        link.exit().remove();

        var node = vis.selectAll("g.node")
            .data(nodes, function(d) { return d.id;});

        var nodeEnter = node.enter().append("g")
            .attr("class", "node")
            .call(force.drag);
        
        nodeEnter.append("circle")
                .attr("class", "node")
      .attr("r", 5);

//        nodeEnter.append("image")
//            .attr("class", "circle")
//            .attr("xlink:href", "https://d3nwyuy0nl342s.cloudfront.net/images/icons/public.png")
//            .attr("x", "-8px")
//            .attr("y", "-8px")
//            .attr("width", "16px")
//            .attr("height", "16px");

        nodeEnter.append("text")
            .attr("class", "nodetext")
            .attr("dx", 12)
            .attr("dy", ".35em")
            .text(function(d) {return d.name});

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

window.graph = new myGraph("svg");

var renderFn = function(error, graphData) {
  window.graphson = {nodes:_.map(graphData.nodes, _.clone), links: _.map(graphData.links, _.clone)};
  _.each(graphData.nodes, window.graph.addNode);
  _.each(graphData.links, window.graph.addLink);

};
if (pieces.length > 1) {
 var name = pieces[1];

d3.json("/nodes/" + name, renderFn); 
}
});

