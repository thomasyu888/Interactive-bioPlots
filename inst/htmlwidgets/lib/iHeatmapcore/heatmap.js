function heatmapdraw(selector,data,options) {

    var el = d3.select(selector);

    var bbox = el.node().getBoundingClientRect();

    var Controller = function() {
        this._events = d3.dispatch("datapoint_hover","transform");
        this._datapoint_hover = {x: null, y: null, value: null};
        this._transform = null;
    };

    (function() {
        this.datapoint_hover = function(_) {
            if (!arguments.length) return this._datapoint_hover;
            this._datapoint_hover = _;
            this._events.datapoint_hover.call(this, _);
        };

        this.transform = function(_) {
            if (!arguments.length) return this._transform;
            this._transform = _;
            this._events.transform.call(this, _);
        };

        this.on = function(evt, callback) {
            this._events.on(evt, callback);
        };
    }).call(Controller.prototype);


    var mainDat = data.matrix,
        //Global annotations variables
        colAnnote = data.colMeta,
        rowAnnote = data.rowMeta,
        colMeta = colAnnote.data,
        rowMeta = rowAnnote.data,
        colHead = colAnnote.header,
        rowHead = rowAnnote.header,
        extra = data.addon,
        addon = extra.data,
        addonHead = extra.header;


    var controller = new Controller();

    var opts = {}
    options = options || {};
    opts.width = options.width || bbox.width;
    opts.height = options.height || bbox.height;
    opts.xclust_height = options.xclust_height || opts.height * 0.12;
    opts.yclust_width = options.yclust_width || opts.width * 0.12;
    opts.xaxis_height = options.xaxis_height || 100;
    opts.yaxis_width = options.yaxis_width || 100;
    opts.legend_width = options.legend_width || 50;
    opts.legend_height = options.legend_height || 200;
    opts.annote_pad = options.annote_pad || 7;
    opts.xAnnote_width = (colHead== null) ? 0:colHead.length*opts.annote_pad;
    opts.yAnnote_height = (rowHead == null) ? 0:rowHead.length*opts.annote_pad;
    opts.showHeat = options.showHeat
    opts.anim_duration = options.anim_duration;
    opts.font_size = options.font_size;
    opts.padding = 20;

    var colormapBounds = {
        position: "absolute",
        left: opts.yclust_width+2*opts.yAnnote_height,
        top: opts.xclust_height+2*opts.xAnnote_width,
        width: (mainDat.data==null) ? 0 : opts.width - opts.yclust_width - opts.yaxis_width-(2*opts.yAnnote_height)-opts.legend_width,
        height:(mainDat.data==null) ? 0 : opts.height - opts.xclust_height - opts.xaxis_height - (2*opts.xAnnote_width)
    };

    var colDendBounds = {
        position: "absolute",
        left: colormapBounds.left,
        top: 0,
        width: (mainDat.data==null) ? (opts.width - opts.yaxis_width) : colormapBounds.width,
        height: (mainDat.data==null) ? (opts.height-2*opts.xAnnote_width-opts.xaxis_height) : opts.xclust_height
    };
    var rowDendBounds = {
        position: "absolute",
        left: 0,
        top: colormapBounds.top,
        width: opts.yclust_width,
        height: colormapBounds.height
    };
    //NEED to fix these
    var colABounds = {
        position: "absolute",
        top: colDendBounds.height+1,
        left: colormapBounds.left,
        width: (mainDat.data==null) ? (opts.width - opts.yaxis_width) : colormapBounds.width,
        height: opts.xAnnote_width
    }
    var rowABounds = {
        position: "absolute",
        top: colormapBounds.top,
        left: rowDendBounds.width+1,
        width: opts.yAnnote_height,
        height: colormapBounds.height
    }
    ///
    var yaxisBounds = {
        position: "absolute",
        top: colormapBounds.top,
        left: colormapBounds.left + colormapBounds.width,
        width: opts.yaxis_width,
        height: colormapBounds.height
    };
    var xaxisBounds = {
        position: "absolute",
        top: (mainDat.data==null) ? (colABounds.top+opts.xAnnote_width) : (colormapBounds.top + colormapBounds.height),
        left: colormapBounds.left,
        width: (mainDat.data==null) ? (opts.width-opts.yaxis_width) : colormapBounds.width,
        height: opts.xaxis_height
    };

    var colLegendBounds = {
        position: "absolute",
        top: 0,
        left: colormapBounds.left+colDendBounds.width+opts.yaxis_width,
        width: opts.legend_width,
        height: opts.legend_height
    };

    var rowLegendBounds = {
        position: "absolute",
        top: colLegendBounds.height,
        left: colormapBounds.left+colormapBounds.width+opts.yaxis_width,
        width: opts.legend_width,
        height: opts.legend_height
    };

    var heatLegendBounds = {
        position: "absolute",
        top: 5,
        //The -99 is because 99 is half the width of the heatLegend, This will center the legend
        left: 5,
        width: opts.yclust_width-5,
        height: colormapBounds.top-5
    };
    function cssify(styles) {
        return {
            position: styles.position,
            top: styles.top + "px",
            left: styles.left + "px",
            width: styles.width + "px",
            height: styles.height + "px"
        };
    }

    (function() {
        var inner = el.append("div").classed("inner", true);
        //colDend is xDend, rowDend is yDend, colmap is heatmap
        var colDend = inner.append("svg").classed("colDend", true).style(cssify(colDendBounds));
        var rowDend = inner.append("svg").classed("rowDend", true).style(cssify(rowDendBounds));
        var colmap = inner.append("svg").classed("colormap", true).style(cssify(colormapBounds));
        var colAnnote = inner.append("svg").classed("colAnnote",true).style(cssify(colABounds));
        var rowAnnote = inner.append("svg").classed("rowAnnote",true).style(cssify(rowABounds));
        var xAxis = inner.append("svg").classed("xAxis",true).style(cssify(xaxisBounds));
        var yAxis = inner.append("svg").classed("yAxis",true).style(cssify(yaxisBounds));
        var colLegend = (colMeta == null | mainDat.data == null ) ? 0 : inner.append("svg").classed("colLegend",true).style(cssify(colLegendBounds));
        var rowLegend = (rowMeta == null) ? 0 : inner.append("svg").classed("rowLegend",true).style(cssify(rowLegendBounds));
        var heatLegend = (mainDat.data == null) ? 0 : inner.append("svg").classed("heatLegend",true).style(cssify(heatLegendBounds));
    })();

    //Creates everything for the heatmap
    var row = (data.rows ==null) ? 0 : dendrogram(el.select('svg.rowDend'), data.rows, false, rowDendBounds.width,rowDendBounds.height);
    var col = (data.cols ==null) ? 0 : dendrogram(el.select('svg.colDend'), data.cols, true, colDendBounds.width, colDendBounds.height);
    var heatmap = heatmapGrid(el.select('svg.colormap'), mainDat, colormapBounds.width,colormapBounds.height);
    var colAnnots = (colMeta == null) ? 0 : drawAnnotate(el.select('svg.colAnnote'),colAnnote, true, colABounds.width,colABounds.height);
    var rowAnnots = (rowMeta == null) ? 0: drawAnnotate(el.select('svg.rowAnnote'),rowAnnote, false,rowABounds.width,rowABounds.height);
    var xLabel = axis(el.select('svg.xAxis'),data.matrix.cols,true,xaxisBounds.width,opts.xaxis_height)
    var yLabel = (mainDat.data==null) ? 0 : axis(el.select('svg.yAxis'),data.matrix.rows,false, opts.yaxis_width, yaxisBounds.height)
    var colALegend = (colMeta == null | mainDat.data == null ) ? 0 : legend(el.select('svg.colLegend'),colAnnots,true)
    var rowALegend = (rowMeta == null) ? 0 : legend(el.select('svg.rowLegend'),rowAnnots,true)
    var heatmapLegend = (mainDat.data == null) ? 0 : legend(el.select('svg.heatLegend'),heatmap,false,heatLegendBounds.width-20)

    function heatmapGrid(svg, data, width, height) {
        // Check for no data
        if (data.data == null)
            return function() {};
        //Heatmap colors
        var color = d3.scale.linear()
            .domain(mainDat.domain)
            .range(mainDat.colors);
        var min = Math.min.apply(Math, mainDat.domain)
        var max = Math.max.apply(Math, mainDat.domain)

        var cols = data.dim[1];
        var rows = data.dim[0];

        var merged = data.merged;
        var x = d3.scale.linear()
            .domain([0, cols])
            .range([0, width]);

        var y = d3.scale.linear()
            .domain([0, rows])
            .range([0, height]);

        var tip = d3.tip()
            .attr('class', 'd3heatmap-tip')
            .html(function(d) { return d; })
            .direction("nw")
            .style("position", "fixed")

        var brush = d3.svg.brush()
            .x(x)
            .y(y)
            .clamp([true, true])
            .on('brush', function() {
                var extent = brush.extent();
                extent[0][0] = Math.round(extent[0][0]);
                extent[0][1] = Math.round(extent[0][1]);
                extent[1][0] = Math.round(extent[1][0]);
                extent[1][1] = Math.round(extent[1][1]);
                d3.select(this).call(brush.extent(extent));
            })
            .on('brushend', function() {
                if (brush.empty()) {
                    controller.transform({
                        scale: [1,1],
                        translate: [0,0],
                        extent: [[0,0],[cols,rows]]
                    });
                } else {
                    var tf = controller.transform();
                    var ex = brush.extent();
                    var scale = [
                        cols / (ex[1][0] - ex[0][0]),
                        rows / (ex[1][1] - ex[0][1])
                    ];
                    var translate = [
                        ex[0][0] * (width / cols) * scale[0] * -1,
                        ex[0][1] * (height / rows) * scale[1] * -1
                    ];
                    controller.transform({scale: scale, translate: translate, extent: ex});
                }
                brush.clear();
                d3.select(this).call(brush).select(".brush .extent")
                    .style({fill: "steelblue", stroke: "blue", opacity: 0.5});
            });

        svg = svg
            .attr("width", width)
            .attr("height", height);

        var rect = svg.selectAll("rect").data(merged);
            rect.enter().append("rect").classed("datapt", true)
            .property("colIndex", function(d, i) { return i % cols; })
            .property("rowIndex", function(d, i) { return Math.floor(i / cols); })
            .property("value", function(d, i) { return d; })
            .attr("fill", function(d) {
              if (!d.color) {
                return "transparent";
              } 
              return d.color;

            })

        rect.exit().remove();
        rect.append("title")
            .text(function(d, i) { return (d === null) ? "NA" : d + ""; })
        rect.call(tip);

        function draw(selection) {
            selection
                .attr("x", function(d, i) {
                    return x(i % cols);
                })
                .attr("y", function(d, i) {
                    return y(Math.floor(i/cols));
                })
                .attr("width", (x(1) - x(0)))
                .attr("height", (y(1) - y(0)))
        }

        draw(rect);

        controller.on('transform.colormap', function(_) {
            x.range([_.translate[0], width * _.scale[0] + _.translate[0]]);
            y.range([_.translate[1], height * _.scale[1] + _.translate[1]]);
            draw(rect.transition().duration(opts.anim_duration).ease("linear"));
        });

        var brushG = svg.append("g")
            .attr('class', 'brush')
            .call(brush)
            .call(brush.event);

        brushG.select("rect.background")
            .on("mouseenter", function() {
                tip.style("display", "block");
            })
            .on("mousemove", function() {
                var col = Math.floor(x.invert(d3.event.offsetX));
                var row = Math.floor(y.invert(d3.event.offsetY));

                var value = Math.round(merged[row*cols + col].label*100)/100;

                var output = '<strong>Row Feature Name: </strong>'+ data.rows[row]+'<br>Column Feature Name: '+ data.cols[col] +'<br>Value: '+value+'<br>Annotations:'
                //Get all the metadata
                if (colMeta != null) {
                    for (k=0; k<colHead.length;k++) {
                        output += '<br>- ' + colHead[k] + ': ' + colMeta[col+(k*cols)]
                    }
                }
                if (rowMeta != null) {
                    for (k=0; k<rowHead.length; k++) {
                        output += '<br>- '+rowHead[k] + ': ' + rowMeta[row+(k*rows)]
                    }
                }
                tip.show(output)
                    .style({
                    top: d3.event.clientY  +15 + "px",
                    left: d3.event.clientX +15+ "px",
                    opacity: 0.9
                })
                //controller.datapoint_hover({col:col, row:row, value:value});
            })
            .on("mouseleave", function() {
                tip.hide().style("display","none")
                controller.datapoint_hover(null);
            });

        return color;
    }

    function axis(svg, data, rotated,width,height) {
        svg = svg.attr("width", width)
            .attr('height',height)
            .append('g')

        // Define scale, axis
        var scale = d3.scale.ordinal()
            .domain(data)
            .rangeBands([0, rotated ? width : height]);

        var axis = d3.svg.axis()
            .scale(scale)
            .orient(rotated ? "bottom" : "right")
            .outerTickSize(0)
            //.tickPadding(3)
            .tickValues(data)

        var fontsize = Math.min(opts.font_size, Math.max(9, scale.rangeBand() - (rotated ? 11: 8))) + "px";
        // Create the actual axis
        var axisNodes = svg.append("g")
            .call(axis);
            axisNodes.selectAll("text")
            .style("text-anchor", "start")
            .style("font-size", fontsize);

        function text(select,length) {
            select.style("opacity", function() {
                if (length <= (rotated ? 100: 40)) {
                    return 1;
                } else {
                    return 0;
                }
            })
        }

        text(axisNodes,data.length)

        controller.on('transform.axis-' + (rotated ? 'x' : 'y'), function(_) {
            var dim = rotated ? 0 : 1;
            var rb = [_.translate[dim], (rotated ? width : height) * _.scale[dim] + _.translate[dim]];
            scale.rangeBands(rb);
            var tAxisNodes = axisNodes.transition().duration(opts.anim_duration).ease('linear');
            tAxisNodes.call(axis);
            // Set text-anchor on the non-transitioned node to prevent jumpiness
            // in RStudio Viewer pane
            axisNodes.selectAll("text").style("text-anchor", "start");

            tAxisNodes
                .selectAll("text")
                .style("text-anchor", "start");

            text(tAxisNodes, (_.extent[1][dim] - _.extent[0][dim]));
        });


    }


    function dendrogram(svg, data, rotated, width, height) {
        var x = d3.scale.linear();
        var y = d3.scale.linear()
            .domain([0, height])
            .range([0, height]);

        var dscale = d3.scale.linear()
            .domain([0, rotated ?  mainDat.dim[1] : mainDat.dim[0]])
            .range([0, rotated ? width : height])

        var tip = d3.tip()
            .attr('class', 'Dend-tip')
            .html(function(d) { 
                return "<strong>" + (rotated ? "Column feature: " : "Row feature: ") + "</strong>" + d; 
            })
            .direction("sw")
            .style("position", "fixed")


        var cluster = d3.layout.cluster()
            .separation(function(a, b) { return 1; })
            .size([rotated ? width : height, (rotated ? height : width)]);

        var transform = "translate(1,0)";

        if (rotated) {
            // Flip dendrogram vertically
            x.range([1, 0]);
            // Rotate
            transform = "rotate(-90) translate(-2,0)";
        }
/////////////
        var brush = d3.svg.brush()
            .x(dscale)
            .y(dscale)
            .clamp([true, true])
            .on('brush', function() {
                var extent = brush.extent();
                extent[0][0] = Math.round(extent[0][0]);
                extent[0][1] = Math.round(extent[0][1]);
                extent[1][0] = Math.round(extent[1][0]);
                extent[1][1] = Math.round(extent[1][1]);
                d3.select(this).call(brush.extent(extent));
            })
            .on('brushend', function() {
                if (brush.empty()) {
                    controller.transform({
                        scale: [1,1],
                        translate: [0,0],
                        extent: [[0,0],[mainDat.dim[1],mainDat.dim[0]]]
                    });
                } else {
                    var tf = controller.transform();
                    var ex = brush.extent();
                    //Have to change the ex, because dendrogram zoom will cause the text to show when it shouldn't.
                    //When rotated make the dimensions of the height not change, when not rotated, the dimensions
                    //Of the width should not change.
                    rotated ? ex[1][0] = ex[1][0] : ex[1][0] = mainDat.dim[1]
                    rotated ? ex[0][0] = ex[0][0] : ex[0][0] = 0
                    rotated ? ex[1][1] = mainDat.dim[0] : ex[1][1] = ex[1][1]
                    rotated ? ex[0][1] = 0 : ex[0][1] = ex[0][1]
                    var scale = [
                        //rotated ? mainDat.dim[1] / (ex[1][0] - ex[0][0]) :1,
                        mainDat.dim[1] / (ex[1][0] - ex[0][0]),
                        //rotated ? 1: mainDat.dim[0] / (ex[1][1] - ex[0][1])
                        mainDat.dim[0] / (ex[1][1] - ex[0][1])
                    ];
                    var translate = [
                        //rotated ? ex[0][0] * (width / mainDat.dim[1]) * scale[0] * -1 :0 ,
                        ex[0][0] * (width / mainDat.dim[1]) * scale[0] * -1,
                        //rotated ? 0 : ex[0][1] * (height / mainDat.dim[0]) * scale[1] * -1
                        ex[0][1] * (height / mainDat.dim[0]) * scale[1] * -1
                    ];
                    controller.transform({scale: scale, translate: translate, extent: ex});
                }
                brush.clear();
                d3.select(this).call(brush).select(".brush .extent")
                    .style({fill: "steelblue", stroke: "steelblue",opacity: 0.5});
            });

//////////

        var dendrG = svg
            .attr("width", width)
            .attr("height", height)
            .append("g")
            .attr("transform", transform)

        var nodes = cluster.nodes(data),
            links = cluster.links(nodes);

        // I'm not sure why, but after the heatmap loads the "links"
        // array mutates to much smaller values. I can't figure out
        // what's doing it, so instead we just make a deep copy of
        // the parts we want.
        var links1 = links.map(function(link, i) {
            return {
                source: {x: link.source.x, y: link.source.y},
                target: {x: link.target.x, y: link.target.y}
            };
        });

        var lines = dendrG.selectAll("polyline").data(links1);
        lines
            .enter().append("polyline")
            .attr("class", "link")
            //.call(tip)

        function draw(selection) {
            function elbow(d, i) {
                return x(d.source.y) + "," + y(d.source.x) + " " +
                    x(d.source.y) + "," + y(d.target.x) + " " +
                    x(d.target.y) + "," + y(d.target.x);
            }
            selection
                .attr("points", elbow)
                .call(tip)
                  //Set the max tooltip width to 200px
            tipwidth = tip.style("width").replace(/[^0-9.]+/g, '')
            tipwidth = parseInt(tipwidth)
            if ((tipwidth)>200) {
                tip.style("width","200px")
            }
        }
        draw(lines);


        controller.on('transform.dendr-' + (rotated ? 'x' : 'y'), function(_) {
            var scaleBy = _.scale[rotated ? 0 : 1];
            var translateBy = _.translate[rotated ? 0 : 1];
            y.range([translateBy, height * scaleBy + translateBy]);
            dscale.range([translateBy, (rotated ? width : height) * scaleBy + translateBy])
            draw(lines.transition().duration(opts.anim_duration).ease("linear"));
        });

        var brushG = svg.append("g")
            .attr('class', 'brush')
            .call(brush)
            .call(brush.event);

        brushG.select("rect.background")
            .on("mouseenter", function() {
                tip.style("display", "block");
            })
            .on("mousemove", function() {
                var col = Math.floor(dscale.invert(d3.event.offsetX));
                var row = Math.floor(dscale.invert(d3.event.offsetY))
                //Get all the metadata
                var output = rotated ? mainDat.cols[col] : mainDat.rows[row];
                var value = output
                if (addon != null && mainDat.data == null) {
                      value += ': '+addon[0][0][col] //Fix this later <-
                //    for (k=0; k<addonHead.length;k++) {
                //        output += '<br> ' + addonHead[k] + ': ' + addon[col]
                //    }
                }
                if (colMeta != null &&mainDat.data == null) {
                    for (k=0; k<colHead.length;k++) {
                        output += '<br> ' + colHead[k] + ': ' + colMeta[col]
                    }
                }


                tip.show(output).style({
                    top: d3.event.clientY +15 + "px",
                    left: d3.event.clientX +15 + "px",
                    opacity: 0.9
                });//Need to fix this.. Array in an Array in an Array ....T.T
                if (addon !=null) {
                    controller.datapoint_hover({row:value, col:colMeta[col], value:addon[1][0][col]});
                }
            })
            .on("mouseleave", function() {
                tip.hide().style("display","none")
                controller.datapoint_hover(null);
            });

    }

    //Legend for the annotations for annotations! width=> interactive width for heatmap legend
    function legend(svg, scales,annotations,width) {
        var leg = svg.selectAll('.legend')
            .data(scales.domain().reverse())
            .enter()
            .append('g')
            .attr('transform', function(d,i) {
                //The +5 is so that the text for the heatmap legend is fixed
                return annotations ? 'translate(0,' + i*8+')' : 'translate(' +(5+i*width/100) +',0)';
            });
        leg.append('rect')
            .attr('width',annotations ? 5 : width/100)
            .attr('height',annotations ? 5 : 35)
            .style('fill',scales)
            .style('stroke',scales)

        leg.append('text')
            .attr('x',annotations ? 6 : -5)
            .attr('y',annotations ? 5 : 45)
            .text(function(d,i) {
                if (annotations) {
                    return d;
                } else if (i==0 || i==49 || i==99) {
                    //return  Math.round(d*100)/100;
                    return d.toFixed(2);
                }
            })
            .style("font-size","7px");
       }


////ONLY ACCEPTS CATEGORICAL ANNOTATIONS, IF VALUES SUCH AS WEIGHT
//PUT INTO BINS FIRST SO 100 - 110 POUNDS IS ONE CATEGORY...
    function drawAnnotate(svg,datum, rotated,width,height) {
        
        var tip = d3.tip()
            .attr('class', 'annote-tip')
            .html(function(d) { 
                return "<strong>Annotation: </strong>" + d; 
            })
            .direction("nw")
            .style("position", "fixed")

        var brush = d3.svg.brush()  
        svg.attr("width",width).attr("height",height)

        var scaling = d3.scale.category10()
        var length = datum.data.length/datum.header.length

        var x = d3.scale.linear()
            .domain([0, length])
            .range([0, width]);

        var y = d3.scale.linear()
            .domain([0, length])
            .range([0, height]);

        //Annotation svg
        var annotation = svg.selectAll('.annotate').data(datum.data);
            annotation.enter().append('svg:rect').classed("annotate",true)
            .style('fill',function(d,i) {
                //Fix color schemes of entire document
                return scaling(d);
            });
            annotation.exit().remove();
            annotation.append("title")
                .text(function(d,i) { return (d===null) ? "NA" : d + "";})
            annotation.call(tip)

        function draw(selection) {
            selection
                .attr('x' , function(d,i) {
                    //This is to account for 2 or more sets of annotation data
                    return (rotated ? x(i%length) : 5*Math.floor(i/length));
                })
                .attr('y', function(d,i) { return (rotated? 5*Math.floor(i/length) : y(i%length)); })
                .attr('width' , (rotated ? x(1)-x(0) :  opts.annote_pad) )
                .attr('height', (rotated ?  opts.annote_pad : y(1)-y(0)) )
                .on("mouseover",tip.show)
                .on("mouseout",tip.hide)
        }

        draw(annotation);

        controller.on('transform.annotation-' + (rotated ? 'x' : 'y'), function(_) {
            if (rotated) {
                x.range([_.translate[0], width * _.scale[0] + _.translate[0]])
            } else {
                y.range([_.translate[1], height * _.scale[1] + _.translate[1]])
            }

            draw(annotation.transition().duration(opts.anim_duration).ease("linear"));
        });

        return scaling;
    };


  var dispatcher = d3.dispatch('hover');

  controller.on("datapoint_hover", function(_) {
    dispatcher.hover({data: _});
  });

  return {
    on: function(type, listener) {
      dispatcher.on(type, listener);
      return this;
    }
  };
};
