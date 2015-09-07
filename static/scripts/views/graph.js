define(["d3",
        "underscore",
        "jquery",
        "backbone",
        "graph"],
        function(d3, _, $, Backbone, grapher) {

        var GraphView = Backbone.View.extend({
            width: 600,
            height: 600,
            el: $('#container'),
            initialize: function(options) {
//                Backbone.Model.prototype.initialize.call(this, options);

                 _.bindAll(this, 'addNodes', 'render');
                this.model.bind('change:transactions', this.addNodes);
                this.model.bind('change:root', this.updateRoot, this);

                this.render();
            },
            updateRoot: function() {
                var rootNode = this.model.get("root");

                if (this.d3graph.findNode(rootNode.name)) {
                    this.d3graph.updateNode(rootNode.name, rootNode);
                } else {
                    this.d3graph.addNode(_.clone(rootNode));
                }
            },
            addNodes: function() {
                _.each(this.model.get("transactions"), _.bind(function(l) {
                    this.d3graph.addLink(l);
                }, this));
            },
            render: function(){
                var width = 600,
                    height = 600;

                var zoom = d3.behavior.zoom()
                    .scaleExtent([0.1, 20])
                        .on("zoom", zoomed);

                var svg = d3.select("#container").append("svg")
                    .attr("width", width)
                    .attr("height", height)
                    .append("g")
                    .attr("transform", "translate(" + 5 + "," +5 + ")")
                    .attr("width", width)
                    .attr("height", height)
                    .call(zoom);

                var defs = svg.append("defs").attr("id", "imgdefs")

                var clipPath = defs.append('clipPath').attr('id', 'clip-circle')
                    .append("circle")
                    .attr("r", 9);

                var rect = svg.append("rect")
                        .attr("width", width)
                        .attr("height", height)
                        .attr("fill", "none")
                        .style("pointer-events", "all");


                this.d3graph = new grapher.fdg("svg g");

                var container = d3.select("svg").select("g").select("g");

                function zoomed() {
                  container.attr("transform", "translate(" + d3.event.translate + ")scale(" + d3.event.scale + ")");

                  container.selectAll(".handle").style("stroke-width", 2/d3.event.scale);
                  //container.selectAll(".nodetext").style("font-size", 8/(d3.event.scale/1.5));
                }
            }
        });
  // Our module now returns our view
  return GraphView;
});
