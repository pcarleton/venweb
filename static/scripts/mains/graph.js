require(
        ["underscore",
        "jquery",
        "views/graph",
        "models/graph",
        "views/edge-list"],
        function(_, $, GraphView, GraphModel, EdgeListView) {
        var graphModel = new GraphModel();
        var graphView = new GraphView({model: graphModel});
        var edgeListView = new EdgeListView({model: graphModel});

        var username = window.location.pathname.split("/")[2];
        graphModel.fetchGraph(username);

        window.graphModel = graphModel;
        window.graphView = graphView;
});

