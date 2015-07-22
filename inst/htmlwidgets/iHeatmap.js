HTMLWidgets.widget({

  name: 'iHeatmap',

  type: 'output',

  initialize: function(el, width, height) {

    return {
      lastValue: null
    };

  },

  renderValue: function(el, x, instance) {
    this.doRenderValue(el,x,instance)
  },
  
  doRenderValue: function(el, x, instance) {
    var self = this;

    instance.lastValue = x;

    el.innerHTML = "";
    var hm = heatmapdraw(el, x, x.options);

    if (window.Shiny) {
      var id = this.getId(el);

      hm.on('hover', function(e) {
        Shiny.onInputChange(id + '_hover', !e.data ? e.data : {
          title: e.data.row,
          abstract: e.data.value,
          cluster: e.data.col
        });
      });
      //hm.on('click', function(e) {
      //  Shiny.onInputChange(id + '_click', !e.data ? e.data : {
      //    value: e.data.value,
      //    row: e.data.row + 1,
    //      col: e.data.col + 1
      //  });
      //});
    }
  },


  resize: function(el, width, height, instance) {
    if (instance.lastValue) {
      this.doRenderValue(el, instance.lastValue, instance);
    }
  }

});
