define(["underscore",
        "jquery",
        "backbone",
        "graph"],
function(_, $, Backbone) {
   var AJAX_CONCURRENCY = 2;
   var GraphModel = Backbone.Model.extend({
   defaults: {
    nodes: [],
    transactions: [],
    indLinks: [],
    degree: 0,
    },
    initialize: function(options) {
        Backbone.Model.prototype.initialize.call(this, options);
        this.runningRequests = 0;
        _.bindAll(this, 'namesForLink', "nodesFromTransactions")
    },
    fetchGraph: function(name) {
        this.set("root", {"name": name});
        // Fetch from server

        $.get("/nodes/" + name, _.bind(function(data) {


            var transNodes = this.nodesFromTransactions(data);

            // Update root node with more info.
            var moreInfo = this.nodeByName(transNodes, this.get("root").name);


            _.defaults(this.get("root"), moreInfo);
            this.trigger("change:root", this);

            this.set({
                "transactions": data,
            });

            this.set("degree", 1);

            this.set("edgeQueue",
                _.difference(_.pluck(transNodes, "name"),
                             [this.get('root').name]));

            this.set("nodes", _.union(transNodes, this.get("nodes")));
        }, this), "json")
    },

    rootNode: function() {
        return this.nodeByName(this.get("nodes"), this.get("root").name);
    },

    nodeByName: function(nodes, nodeName) {
        return _.find(nodes, function (node) {
            return node.name == nodeName;
        });
    },

    nodesFromTransactions: function(transactions) {
        var sources = _.pluck(transactions, "source");
        var targets = _.pluck(transactions, "target");

        var both = _.union(sources, targets);
        return _.uniq(both, function (node) { return node.name});
    },
    namesForLink: function(nodes, l) {
        console.log("Nodes: " + nodes);
        console.log("L: " + l);
        return {"source": nodes[l.source].name, "target": nodes[l.target].name};
    },
    expandEdges: function() {

        this.expanding = _.clone(this.get("edgeQueue"));

        while (this.runningRequests < AJAX_CONCURRENCY) {
            this.runningRequests++;
            this.expandNext();
        }

    },

    expandNext: function() {
        if (this.expanding.length > 0) {
            this.expandName(this.expanding.pop(),
                            _.bind(this.expandNext, this))
        } else {
            this.runningRequests--;
            if (!this.isFetching()) {
                this.set("degree", this.get("degree") + 1);
            }
        }
    },

    isFetching: function() {
        return this.expanding && this.expanding.length > 0;
    },

    expandName: function(name, cb) {
        this.runningRequests++;
        $.get("/nodes/" + name, _.bind(function(data) {
            var transNodes = this.nodesFromTransactions(data);

            this.set({
                "transactions": _.union(this.get("transactions"), data),
            });

            var startingEdgeQueue = this.get("edgeQueue");

            startingEdgeQueue = _.reject(startingEdgeQueue, function (uname) {
                return uname == name;
            });

            this.set("edgeQueue",
                _.union(_.difference(_.pluck(transNodes, "name"),
                                     _.pluck(this.get("nodes"), "name")),
                        startingEdgeQueue));

            this.set("nodes", _.union(transNodes, this.get("nodes")));
        }, this), "json").fail(_.bind(function(msg) {
            console.log("failed on " + name);
            console.log(msg);

            // Still remove from edge list
            var startingEdgeQueue = this.get("edgeQueue");

            startingEdgeQueue = _.reject(startingEdgeQueue, function (uname) {
                return uname == name;
            });

            this.set("edgeQueue", _.clone(startingEdgeQueue));
        }, this)).always(_.bind(function() {
            this.runningRequests--;
            cb();
        }, this));
    }
   });

   return GraphModel;
});
