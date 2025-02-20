window.in_iframe = true; // https://www.w3schools.com/js/js_modules.asp wont work with file:// because of CORS
function togglePlotlyWindowZoom() {} // TODO: send messages between iframe and parent
window.onerror = function(e) {
  // e is masked because of same CORS, analyze, if the well-known plotly error had occurred
  var well_known = false;
  try {
    Plotly.redraw($(".plotly")[0])
  } catch(e) {
    if (e == "Error: Something went wrong with axis scaling") {
      well_known = true
    }
  }
  if (well_known) {
    window.reload_needed = true;
    return true;
  } else {
    return false;
  }
}
window.onresize = function(e) { // if we had the well_known error, do a reload
  if (window.reload_needed) {
    window.reload_needed = false;
    window.location.reload()
  }
}

window.onload = function(){
  $("body").contextmenu((event) => {
      event.preventDefault();
      var coords = {
        offsetX: event.offsetX,
        offsetY: event.offsetY,
        nm: $("#nm").attr("data-nm")
      };
      window.sendMsgContextMenu(coords);
  });
};

function togglePlotlyWindowZoom(gd) {
      var figureInfo = {
        nm: $("#nm").attr("data-nm")
      };
      window.sendMsgToggleHandle(figureInfo);
}

function dlResult(fileName) {
  try {
    var py = $(document).find(".js-plotly-plot")[0]
    Plotly.toImage(py, {format: 'svg'}).then(function(url) {
      download(fileName + ".svg", url)
    })
    return ;
  } catch(e) {
    console.log(e)
  }
  try {
    if ($(document).find("img")[0].src.startsWith("data:image/svg+xml;base64")) {
      var ext = ".svg"
    } else {
      var ext = ".png"
    }
    download(fileName + ext,
        $(document).find("img")[0].src)
  } catch (e) {
    console.log(e)
  }
}

const min_pixels_per_tick = 40;
const max_pixels_per_tick = 100;
const min_width_in_em = 55;
const min_height_in_em = 15;
const def_height_in_vh = 55;
const def_width_in_vh = 70;
const default_plotlyScreenFit = false;

var em = 10;

$(function() {
  var div = $('<div style="width: 1em;"></div>').appendTo('body');
  em = div.width();
})

function reportInitialSize() {
  return ;
    // if the iframe has been initilaized already (also, if a reload of the
    // iframe happend), do not re-initialize. This won't survive
    // a tab close nor a reload of the top level document (for the latter
    // see script_toplevel.js and look for "reset initialSizeReported")
    if (sessionStorage.getItem("initialSizeReported." +
      $("#nm").attr("data-nm")) == "true") {
//      return ; but then, after a reload, we miss the initial size estimate;
//      however, size handle actions will not presist
    }//click on + sometimes wrongly restored?!
    var py = $(".plotly")[0]
    var my_max_pixels_per_tick = max_pixels_per_tick ;
    var my_min_pixels_per_tick = min_pixels_per_tick ;
    var fkt = $("#functionName").attr("data-functionName");
    var already = false;
    var w;
    var h;
    if (typeof sizing_hints == "object" &&
        typeof sizing_hints.figure_type_id == "object" &&
        sizing_hints.figure_type_id.length == 1 &&
        sizing_hints.figure_type_id[0] == "bar_chart") {
      if (
        typeof sizing_hints.w == "object" &&
        typeof sizing_hints.h == "object" &&
        sizing_hints.w.length == 1 &&
        sizing_hints.h.length == 1 &&
        typeof sizing_hints.w[0] == "number" ||
        typeof sizing_hints.w[0] == "string" &&
         typeof sizing_hints.h[0] == "number" ||
        typeof sizing_hints.h[0] == "string"
      ) {
        if (typeof sizing_hints.w[0] == "number" ) {
           w = sizing_hints.w[0]
          } else {
            w = window.screen.availWidth * parseFloat(sizing_hints.w[0]);
          }
        if (typeof sizing_hints.h[0] == "number" ) {
           h = sizing_hints.h[0]
          } else {
            h = window.screen.availHeight * parseFloat(sizing_hints.h[0]);
          }
        already = true;
      } else {
        window.alert("Internal Error: Invalid bar_chart object. Sorry. Please report.")
      }
 } else if (typeof sizing_hints == "object" &&
        typeof sizing_hints.figure_type_id == "object" &&
        sizing_hints.figure_type_id.length == 1 &&
        sizing_hints.figure_type_id[0] == "dot_mat") {
        if (
        typeof sizing_hints.w == "object" &&
        typeof sizing_hints.h == "object" &&
        sizing_hints.w.length == 1 &&
        sizing_hints.h.length == 1 &&
        typeof sizing_hints.w[0] == "number" ||
        typeof sizing_hints.w[0] == "string" &&
         typeof sizing_hints.h[0] == "number" ||
        typeof sizing_hints.h[0] == "string"
        ) {
         if (typeof sizing_hints.w[0] == "number" ) {
           w = sizing_hints.w[0]
          } else {
            w = window.screen.availWidth * parseFloat(sizing_hints.w[0]);
          }
        if (typeof sizing_hints.h[0] == "number" ) {
           h = sizing_hints.h[0]
          } else {
            h = window.screen.availHeight * parseFloat(sizing_hints.h[0]);
          }
          already = true;
        } else {
          window.alert("Internal Error: Invalid dot_mat object. Sorry. Please report.")
        }
} else if (typeof sizing_hints == "object" &&
        typeof sizing_hints.figure_type_id == "object" &&
        sizing_hints.figure_type_id.length == 1 &&
        sizing_hints.figure_type_id[0] == "scatt_plot") {
      if (
         typeof sizing_hints.w == "object" &&
        typeof sizing_hints.h == "object" &&
        sizing_hints.w.length == 1 &&
        sizing_hints.h.length == 1 &&
        typeof sizing_hints.w[0] == "number" ||
        typeof sizing_hints.w[0] == "string" &&
         typeof sizing_hints.h[0] == "number" ||
        typeof sizing_hints.h[0] == "string"
      ) {
         if (typeof sizing_hints.w[0] == "number" ) {
           w = sizing_hints.w[0]
          } else {
            w = window.screen.availWidth * parseFloat(sizing_hints.w[0]);
          }
        if (typeof sizing_hints.h[0] == "number" ) {
           h = sizing_hints.h[0]
          } else {
            h = window.screen.availHeight * parseFloat(sizing_hints.h[0]);
          }
        already = true;
      } else {
        window.alert("Internal Error: Invalid scatt_plot object. Sorry. Please report.")
      }
} else if (
  typeof sizing_hints == "object" &&
        typeof sizing_hints.figure_type_id == "object" &&
        sizing_hints.figure_type_id.length == 1 &&
        sizing_hints.figure_type_id[0] == "bar_limit") {
        if (
         typeof sizing_hints.w == "object" &&
        typeof sizing_hints.h == "object" &&
        sizing_hints.w.length == 1 &&
        sizing_hints.h.length == 1 &&
        typeof sizing_hints.w[0] == "number" ||
        typeof sizing_hints.w[0] == "string" &&
         typeof sizing_hints.h[0] == "number" ||
        typeof sizing_hints.h[0] == "string"
        ) {
        if (typeof sizing_hints.w[0] == "number" ) {
           w = sizing_hints.w[0]
          } else {
            w = window.screen.availWidth * parseFloat(sizing_hints.w[0]);
          }
        if (typeof sizing_hints.h[0] == "number" ) {
           h = sizing_hints.h[0]
          } else {
            h = window.screen.availHeight * parseFloat(sizing_hints.h[0]);
          }
          already = true;
        } else {
          window.alert("Internal Error: Invalid bar_limit object. Sorry. Please report.")
        }
        //new part
  } else if (typeof sizing_hints == "object" &&
        typeof sizing_hints.figure_type_id == "object" &&
        sizing_hints.figure_type_id.length == 1 &&
        sizing_hints.figure_type_id[0] == "dot_loess") {
      if (
        typeof sizing_hints.w == "object" &&
        typeof sizing_hints.h == "object" &&
        sizing_hints.w.length == 1 &&
        sizing_hints.h.length == 1 &&
        typeof sizing_hints.w[0] == "number" ||
        typeof sizing_hints.w[0] == "string" &&
        typeof sizing_hints.h[0] == "number" ||
        typeof sizing_hints.h[0] == "string"
      ) {
        if (typeof sizing_hints.w[0] == "number" ) {
           w = sizing_hints.w[0]
          } else {
            w = window.screen.availWidth * parseFloat(sizing_hints.w[0]);
          }
        if (typeof sizing_hints.h[0] == "number" ) {
           h = sizing_hints.h[0]
          } else {
            h = window.screen.availHeight * parseFloat(sizing_hints.h[0]);
          }
        already = true;
      } else {
        window.alert("Internal Error: Invalid dot_loess object. Sorry. Please report.")
      }
//
  } else if (typeof sizing_hints == "object" &&
        typeof sizing_hints.figure_type_id == "object" &&
        sizing_hints.figure_type_id.length == 1 &&
        sizing_hints.figure_type_id[0] == "marg_plot") {
      if (
        typeof sizing_hints.w == "object" &&
        typeof sizing_hints.h == "object" &&
        sizing_hints.w.length == 1 &&
        sizing_hints.h.length == 1 &&
        typeof sizing_hints.w[0] == "number" ||
        typeof sizing_hints.w[0] == "string" &&
        typeof sizing_hints.h[0] == "number" ||
        typeof sizing_hints.h[0] == "string"
      ) {
        if (typeof sizing_hints.w[0] == "number" ) {
           w = sizing_hints.w[0]
          } else {
            w = window.screen.availWidth * parseFloat(sizing_hints.w[0]);
          }
        if (typeof sizing_hints.h[0] == "number" ) {
           h = sizing_hints.h[0]
          } else {
            h = window.screen.availHeight * parseFloat(sizing_hints.h[0]);
          }
        already = true;
      } else {
        window.alert("Internal Error: Invalid marg_plot object. Sorry. Please report.")
      }
//
        } /*else if (fkt == "acc_loess") {
      my_max_pixels_per_tick *= 3 ;
      my_min_pixels_per_tick *= 3 ;
    } else if (fkt == "acc_margins") {
      my_max_pixels_per_tick *= 2 ;
      my_min_pixels_per_tick *= 2 ;
    } else if (fkt == "acc_univariate_outlier") {
      my_max_pixels_per_tick *= 1.5 ;
      my_min_pixels_per_tick *= 2 ;
    } else if (fkt == "int_datatype_matrix") {
      my_max_pixels_per_tick *= 1.5 ;
      my_min_pixels_per_tick *= 2 ;*/
    /*} else if (fkt == "com_item_missingness") {
      my_max_pixels_per_tick *= 1.65 ;
      my_min_pixels_per_tick *= 1.3 ;
    } else if (fkt == "acc_distributions_loc") {
      my_max_pixels_per_tick *= 1.5;
      my_min_pixels_per_tick *= 2;
    } else if (fkt == "com_segment_missingness") {
      my_max_pixels_per_tick *= 1.5;
      my_min_pixels_per_tick *= 2;
    } else if (fkt == "con_contradictions_redcap") {
      my_max_pixels_per_tick *= 1.5;
      my_min_pixels_per_tick *= 1.5;
    } else if (fkt == "con_limit_deviations") {
      my_max_pixels_per_tick *= 2.5;
      my_min_pixels_per_tick *= 2.5;
      //Table of variables is bugged?
    } else if (fkt == "acc_distributions_loc_ecdf") {
      my_max_pixels_per_tick *= 1.5;
      my_min_pixels_per_tick *= 2;
    } else if (fkt == "acc_distributions_only_ecdf") {
      my_max_pixels_per_tick *= 1.5;
      my_min_pixels_per_tick *= 2.5;
    } */else if (fkt == "acc_multivariate_outlier") {  //add constant
      my_max_pixels_per_tick *= 2;
      my_min_pixels_per_tick *= 3;
    }
    if (JSON.parse(Plotly.Plots.graphJson(py)).layout.hasOwnProperty("xaxis") &&
        JSON.parse(Plotly.Plots.graphJson(py)).layout.hasOwnProperty("yaxis") &&
        JSON.parse(Plotly.Plots.graphJson(py)).layout.xaxis.hasOwnProperty("tickvals") &&
        JSON.parse(Plotly.Plots.graphJson(py)).layout.yaxis.hasOwnProperty("tickvals")) {
      var x = JSON.parse(Plotly.Plots.graphJson(py)).layout.xaxis.tickvals.length;
      var y = JSON.parse(Plotly.Plots.graphJson(py)).layout.yaxis.tickvals.length;
      if (JSON.parse(Plotly.Plots.graphJson(py)).layout.hasOwnProperty("xaxis2") &&
          JSON.parse(Plotly.Plots.graphJson(py)).layout.xaxis2.hasOwnProperty("tickvals")) {
        var x2 = JSON.parse(Plotly.Plots.graphJson(py)).layout.xaxis2.tickvals.length;
        x = x + x2;
      }
      if (JSON.parse(Plotly.Plots.graphJson(py)).layout.hasOwnProperty("yaxis2") &&
          JSON.parse(Plotly.Plots.graphJson(py)).layout.yaxis2.hasOwnProperty("tickvals")) {
        var y2 = JSON.parse(Plotly.Plots.graphJson(py)).layout.yaxis2.tickvals.length;
        y = y + y2;
      }
    } else { // no x or y axis found -- likely a pie chart
      if (py._fullData[0].type == "pie") {
       // var x = 3;
        //var y = 3;
        w = 100;
        h = 100;
        already = true;
      } else {
        var x = 4;
        var y = 4;
      }
    }
    if (!already) {
      h = Math.floor(window.screen.height / 10) ; // 10% of screen height
      w = Math.floor(window.screen.width / 10) ; // and width minimum
      if (my_min_pixels_per_tick * x > w) {
        w = my_min_pixels_per_tick * x;
      }
      if (my_max_pixels_per_tick * x < w) {
        w = my_max_pixels_per_tick * x;
      }
      if (min_width_in_em*em > w) {
        w = min_width_in_em * em;
      }
      if (my_min_pixels_per_tick * y > h) {
        h = my_min_pixels_per_tick * y;
      }
      if (my_max_pixels_per_tick * y < h) {
        h = my_max_pixels_per_tick * y;
      }
      if (min_height_in_em*em > h) {
        h = min_height_in_em * em;
      }
    }
    if (JSON.parse(Plotly.Plots.graphJson(py)).layout.hasOwnProperty("xaxis") &&
        JSON.parse(Plotly.Plots.graphJson(py)).layout.xaxis.hasOwnProperty("ticktext")) {
      var max_xlab = Math.max.apply(Math, JSON.parse(Plotly.Plots.graphJson(py)).layout.xaxis.ticktext.map(x => x.length))
    } else { // no x or y axis found -- likely a pie chart
      if (py._fullData[0].type == "pie") {
        var max_xlab = 3
      } else {
        var max_xlab = 4
      }
    }
    if (JSON.parse(Plotly.Plots.graphJson(py)).layout.hasOwnProperty("yaxis") &&
        JSON.parse(Plotly.Plots.graphJson(py)).layout.yaxis.hasOwnProperty("ticktext")) {
      var max_ylab = Math.max.apply(Math, JSON.parse(Plotly.Plots.graphJson(py)).layout.yaxis.ticktext.map(x => x.length))
    } else { // no x or y axis found -- likely a pie chart
      if (py._fullData[0].type == "pie") {
        var max_ylab = 3
      } else {
        var max_ylab = 4
      }
    }
    if (JSON.parse(Plotly.Plots.graphJson(py)).layout.hasOwnProperty("xaxis2") &&
        JSON.parse(Plotly.Plots.graphJson(py)).layout.xaxis2.hasOwnProperty("ticktext")) {
      var max_xlab2 = Math.max.apply(Math, JSON.parse(Plotly.Plots.graphJson(py)).layout.xaxis2.ticktext.map(x => x.length))
      max_xlab = Math.max(max_xlab, max_xlab2)
    }
    if (JSON.parse(Plotly.Plots.graphJson(py)).layout.hasOwnProperty("yaxis2") &&
        JSON.parse(Plotly.Plots.graphJson(py)).layout.yaxis2.hasOwnProperty("ticktext")) {
      var max_ylab2 = Math.max.apply(Math, JSON.parse(Plotly.Plots.graphJson(py)).layout.yaxis2.ticktext.map(x => x.length))
      max_ylab = Math.max(max_ylab, max_ylab2)
    }
    w = w //+ em * max_ylab;
    h = h //+ em * max_xlab;
    window.sendMsgInitialSizeEstimate({
      w: w,
      h: h,
      nm: $("#nm").attr("data-nm")
    })
    sessionStorage.setItem("initialSizeReported." + $("#nm").attr("data-nm"),
      "true");
  }

$(reportInitialSize)
