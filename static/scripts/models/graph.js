define(["underscore",
        "jquery",
        "backbone",
        "graph"],
function(_, $, Backbone) {
   var GraphModel = Backbone.Model.extend({
   defaults: {
    nodes: [],
    transactions: [],
    indLinks: [],
    },
    initialize: function(options) {
        Backbone.Model.prototype.initialize.call(this, options);
        _.bindAll(this, 'namesForLink', "nodesFromTransactions")
    },
    fetchGraph: function(name) {
        this.set("root", {"name": name});
        // Fetch from server

        $.get("/nodes/" + name, _.bind(function(data) {


            var transNodes = this.nodesFromTransactions();

            // Update root node with more info.
            var moreInfo = this.nodeByName(transNodes, this.get("root").name);
            _.extend(this.get("root"), moreInfo);

            this.set({
                "transactions": data,
            });
            //this.set("edgeQueue", _.difference(this.get("nodes"), transNodes));

            //this.set("nodes", _.union(transNodes, this.get("nodes")));
           //this.set("nodes", _.union(this.get("nodes"), data.nodes));
           //this.set("indLinks", data.links);
        }, this), "json")
    },

    rootNode: function() {
        return this.nodeByName(this.get("root"), this.get("nodes"));
    },

    nodeByName: function(nodes, nodeName) {
        console.log(nodes);
        return _.find(nodes, function (node) {
            console.log(node);
            return node.name == nodeName;
        });
    },

    nodesFromTransactions: function() {
        var sources = _.pluck(this.get("transactions"), "source");
        var targets = _.pluck(this.get("transactions"), "target");

        return _.union(sources, targets);
    },
    namesForLink: function(nodes, l) {
        console.log("Nodes: " + nodes);
        console.log("L: " + l);
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
