define(["underscore",
        "jquery",
        "backbone",
        "graph",
        "text!templates/expand_button.html",
        "text!templates/cancel.html",
        ],
        function(_, $, Backbone, grapher, expandTmpl, cancelTmpl) {

        var EdgeListView = Backbone.View.extend({
            el: $('#edge-list'),
            events: {
                "click .expand-all-button": "expandAll",
                "click .cancel-button": "cancel",
            },
            initialize: function(options) {
                this.model.bind('change:nodes', this.setNumNodes, this);
                this.model.bind('change:degree', this.setDegree, this);
            },
            expandAll: function() {
                //this.$(".expand-all-button").attr("disabled", "true");
                ga('send', 'event', 'button', 'click', 'expand',
                   this.model.get("degree"));
                this.model.set("cancelled", false);
                this.model.expandEdges();
                this.$(".expand-all-button").prop('disabled', true);
                this.$(".control").html(cancelTmpl);


            },
            cancel: function() {
                ga('send', 'event', 'button', 'click', 'cancel',
                   this.model.get("degree"));
                this.model.set("cancelled", true);
                this.$(".control").html(expandTmpl);
            },
            setNumNodes: function() {
               this.$("#node-count-span").html(this.model.get("nodes").length);
            },
            setDegree: function() {
                this.$("#degree-span").html(this.model.get("degree"));
                this.$(".expand-all-button").prop('disabled', false);
            }
        });

        return EdgeListView;
});
