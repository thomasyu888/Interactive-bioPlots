HTMLWidgets.widget({

  name: 'iHeatmap',

  type: 'output',

  initialize: function(el, width, height) {

    return {
      lastValue: null
    };

  },

  renderValue: function(el, x, instance) {
    instance.lastValue = x;

    el.innerHTML = "";
    var hm = heatmapdraw(el, x, x.options);
console.log(hm)
console.log(x)
console.log(x.options)
console.log(instance)
    if (window.Shiny) {
      var id = this.getId(el);

      //hm.on('hover', function(e) {
      //  Shiny.onInputChange(id + '_hover', !e.data ? e.data : {
      //    value: e.data.value,
      //    row: e.data.row + 1,
      //    col: e.data.col + 1
      //  });
      //});
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
      this.renderValue(el, instance.lastValue, instance);
    }
  }

});
