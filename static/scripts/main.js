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
        ["scripts/lib/d3.js",
        "underscore",
        "jquery",
        "graph"],
        function(d3, _, $, grapher) {

var width = 600,
    height = 600;

var zoom = d3.behavior.zoom()
    .scaleExtent([0.1, 20])
        .on("zoom", zoomed);

var svg = d3.select("body").append("svg")
    .attr("width", width)
    .attr("height", height)
    .append("g")
    .attr("transform", "translate(" + 5 + "," +5 + ")")
    .attr("width", width)
    .attr("height", height)
    .call(zoom);

var rect = svg.append("rect")
        .attr("width", width)
        .attr("height", height)
        .attr("fill", "none")
        .style("pointer-events", "all");


window.graph = new grapher.fdg("svg g");


var container = d3.select("svg").select("g").select("g");

function zoomed() {
  container.attr("transform", "translate(" + d3.event.translate + ")scale(" + d3.event.scale + ")");

  container.selectAll(".handle").style("stroke-width", 2/d3.event.scale);
  //container.selectAll(".nodetext").style("font-size", 8/(d3.event.scale/1.5));
  }

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


var renderFn = function(error, graphData) {
  window.graphson = {nodes:_.map(graphData.nodes, _.clone), links: _.map(graphData.links, _.clone)};
  _.each(graphData.nodes, window.graph.addNode);
  _.each(_.map(graphData.links,
                 function (l) { 
                        return {source: graphData.nodes[l.source].name,
                                target: graphData.nodes[l.target].name};
                 }),
                window.graph.addLink);

};
if (pieces.length > 1) {
 var name = pieces[1];

d3.json("/nodes/" + name, renderFn); 
}
});

