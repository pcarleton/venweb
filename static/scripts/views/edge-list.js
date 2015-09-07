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
            initialize: function(options) {
                this.model.bind('change:nodes', this.setNumNodes, this);
                this.model.bind('change:degree', this.setDegree, this);
            },
            expandAll: function() {
                //this.$(".expand-all-button").attr("disabled", "true");
                this.model.expandEdges();
            },
            setNumNodes: function() {
               this.$("#node-count-span").html(this.model.get("nodes").length);
            },
            setDegree: function() {
                this.$("#degree-span").html(this.model.get("degree"));
            }
        });

        return EdgeListView;
});
