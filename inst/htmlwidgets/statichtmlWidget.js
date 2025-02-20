HTMLWidgets.widget({

  name: 'statichtmlWidget',

  type: 'output',

  factory: function(el, width, height) {

    return {

      renderValue: function(x) {

        el.innerHTML = x.html;
        window.setTimeout(function() {
          HTMLWidgets.staticRender();
          $(":is(.plotly):is(.html-widget)").each(function() {
            Plotly.update(
              this,
              {},
              {
                height: this.clientHeight,
                width: this.clientWidth
              })
          });
          $(".statichtmlWidget").css({'height': ''})
        }, 50);
      },

/*      resize: function(width, height) {
        console.log("Resize to " + width + " x " + height)
        debugger
        // TODO: code to re-render the widget with a new size
//$(".dataTable").dataTable().resize()
      }
*/
    };
  }
});
