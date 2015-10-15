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

        var autoExpand = function() {
            if ((graphModel.get("degree") >= 3) || graphModel.get("cancelled")) {
                cancelExpand();
                return;
            }
            if (!graphModel.get("readyToFetch")) {
                return;
            }
            if (!graphModel.isFetching() && graphModel.get("degree") >= 1) {
                edgeListView.expandAll();
            }
        }

        var interval = setInterval(autoExpand, 1000);

        var cancelExpand = function() {
            clearInterval(interval);
        }
});

