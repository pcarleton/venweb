define(["underscore",
        "jquery",
        "backbone",
        "graph"],
function(_, $, Backbone) {
   var GraphModel = Backbone.Model.extend({
   defaults: {
    nodes: [],
    links: [],
    indLinks: [],
    },
    initialize: function(options) {
        Backbone.Model.prototype.initialize.call(this, options);
        _.bindAll(this, 'namesForLink')
    },
    fetchGraph: function(name) {
        this.set("nodes", [{"name": "/" + name}]);
        // Fetch from server

        $.get("/nodes/" + name, _.bind(function(data) {
            this.set({
                "links": _.map(data.links, _.bind(this.namesForLink, this, data.nodes))
            });
           this.set("nodes", _.union(this.get("nodes"), data.nodes));
           this.set("indLinks", data.links);
        }, this), "json")
    },
    namesForLink: function(nodes, l) {
        return {"source": nodes[l.source].name, "target": nodes[l.target].name};
    },

    expandGraph: function() {
        $.post("/expand", JSON.stringify({nodes: this.get("nodes"), links: this.get("indLinks")}), _.bind(function(data) {
            this.set({
                "links": _.map(data.links, _.bind(this.namesForLink, this, data.nodes))
            });
           this.set("nodes", _.union(this.get("nodes"), data.nodes));
           this.set("indLinks", data.links);

        }, this), "json")
    }
   });

   return GraphModel;
});