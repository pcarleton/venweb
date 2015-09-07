define(["underscore",
        "jquery",
        "backbone",
        "graph"],
        function(_, $, Backbone, grapher) {

        var EdgeListView = Backbone.View.extend({
            el: $('#edge-list'),
            events: {
                "click .expand-all-button": "expandAll",
            },
            expandAll: function() {
                //this.$(".expand-all-button").attr("disabled", "true");
                this.model.expandEdges();
            }
        });

        return EdgeListView;
});
