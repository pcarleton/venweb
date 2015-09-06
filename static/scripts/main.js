require.config({
    paths: {
        underscore: 'lib/underscore',
        jquery: 'lib/jquery',
        backbone: 'lib/backbone'
    },
    shim: {
        "underscore": {
            exports: "_"
        },
        'backbone': {
            deps: ['underscore', 'jquery'],
            exports: 'Backbone'
        }
    }
});


require(
        ["scripts/lib/d3.js",
        "underscore",
        "jquery",
        "views/graph",
        "models/graph",
        "views/edge-list"],
        function(d3, _, $, GraphView, GraphModel, EdgeListView) {
        var graphModel = new GraphModel();
        var graphView = new GraphView({model: graphModel});
        var edgeListView = new EdgeListView({model: graphModel});

        graphModel.fetchGraph("Paul-Carleton");

        window.graphModel = graphModel;
        window.graphView = graphView;


//
//
//var query = window.location.search.substring(1);
//var pieces = query.split("=");
//
//window.fetchfn = function() {
//   d3.xhr("/expand")
//    .header("Content-Type", "application/json")
//    .post(
//        JSON.stringify(window.graphson),
//        function(err, rawData){
//            console.log("got response", rawData);
//            var data = JSON.parse(rawData.response);
//            renderFn({}, data);
//            window.graphson = data;
//        }
//    );
//};
//
//var graphson = {};
//
//
//var renderFn = function(error, graphData) {
//  window.graphson = {nodes:_.map(graphData.nodes, _.clone), links: _.map(graphData.links, _.clone)};
//  _.each(graphData.nodes, window.graph.addNode);
//  _.each(_.map(graphData.links,
//                 function (l) {
//                        return {source: graphData.nodes[l.source].name,
//                                target: graphData.nodes[l.target].name};
//                 }),
//                window.graph.addLink);
//
//};
//if (pieces.length > 1) {
// var name = pieces[1];
//
//d3.json("/nodes/" + name, renderFn);
//}
});

