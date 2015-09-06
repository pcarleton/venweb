define(["scripts/lib/d3.js",
        "underscore",
        "jquery",
        "backbone",
        "graph"],
        function(d3, _, $, Backbone, grapher) {

        var EdgeListView = Backbone.View.extend({
            el: $('#edge-list'),
            events: {
                "click .expand-all-button": "expandAll",
            },
            expandAll: function() {
                console.log("Expanding!")
                //this.$(".expand-all-button").attr("disabled", "true");
                this.model.expandEdges();
            }
        });

        return EdgeListView;
});
