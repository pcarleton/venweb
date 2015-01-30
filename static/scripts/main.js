require.config({
    paths: {
        underscore: 'http://underscorejs.org/underscore'
    },
    shim: {
        "underscore": {
            exports: "_"
        }
    }
});


require(
        ["scripts/lib/d3.js", "underscore"],
        function(d3, _) {

var width = 300,
    height = 300;

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
            var data = JSON.parse(rawData);
            console.log("got response", data);
        }
    );
};

var graphson = {};


var renderFn = function(error, graph) {
  window.graphson = { nodes: _.map(graph.nodes, _.clone), links: _.map(graph.links, _.clone)}; 
  force
      .nodes(graph.nodes)
      .links(graph.links)
      .start();


  var link = svg.selectAll(".link")
      .data(graph.links)
    .enter().append("line")
      .attr("class", "link")
      .style("stroke-width", function(d) { return 1; });

  var node = svg.selectAll(".node")
      .data(graph.nodes)
    .enter().append("circle")
      .attr("class", "node")
      .attr("r", 5)
      //.style("fill", function(d) { return color(d.group); })
      .call(force.drag);

  node.append("title")
      .text(function(d) { return d.name; });

  var texts = svg.selectAll('labels')
		.data(graph.nodes)
		.enter().append("text")
		.attr("dx", 4)
		.attr("dy", 4)
		.attr("class", "nodetext")
  		.text(function(d) { return d.name; })
		.call(force.drag);


  force.on("tick", function() {
    link.attr("x1", function(d) { return d.source.x; })
        .attr("y1", function(d) { return d.source.y; })
        .attr("x2", function(d) { return d.target.x; })
        .attr("y2", function(d) { return d.target.y; });

    node.attr("cx", function(d) { return d.x; })
        .attr("cy", function(d) { return d.y; });

	texts.attr("x", function(d) { return d.x;})
		 .attr("y", function(d) { return d.y;});
  });
};
if (pieces.length > 1) {
 var name = pieces[1];

d3.json("/nodes/" + name, renderFn); 
}
});

