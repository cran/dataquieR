window.in_iframe = true;

// https://www.w3schools.com/js/js_modules.asp wont work with file:// because of CORS

//runs imidiatly
(() => {
  initPlotlyErrorCorection();
})()

//runs after dom is loaded
$(() => {
  initCustomContextMenu();
})

//#region general init functions


/**
 * Registers a custom context menu on the document body and sends its position
 * and metadata to external handlers. Also listens for click/Escape to dismiss it.
 */
function initCustomContextMenu() {
  $("body").contextmenu((event) => {
    event.preventDefault();
    const coords = {
      x: event.clientX,
      y: event.clientY,
      nm: $("#nm").attr("data-nm")
    };
    window.sendMsgContextMenu(coords);
    document.addEventListener("click", function () {
      window.sendMsgDismissContextMenu();
    });
    document.addEventListener("keydown", function (e) {
      if (e.key === "Escape") {
        window.sendMsgDismissContextMenu();
      }
    });
  });
}

//#endregion

//#region Plotly

/**
 * Attempts to detect a known Plotly rendering error and triggers a reload recovery flow.
 * Also hooks window resize to reload the page if recovery was flagged.
 */
function initPlotlyErrorCorection() {
  window.onerror = function (e) {
    // e is masked because of same CORS, analyze, if the well-known plotly error had occurred
    let well_known = false;
    try {
      Plotly.redraw($(".plotly")[0])
    } catch (e) {
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
  window.onresize = function (e) { // if we had the well_known error, do a reload
    if (window.reload_needed) {
      window.reload_needed = false;
      window.location.reload()
    }
  }
}

/**
 * Sends a request to the parent window to toggle the Plotly zoom state
 * for the current figure.
 *
 * Retrieves the figure identifier (`nm`) from the DOM and forwards it
 * via postMessage using `sendMsgToggleHandle`.
 */
function togglePlotlyWindowZoom() {
  const figureInfo = {
    nm: $("#nm").attr("data-nm")
  };
  window.sendMsgToggleHandle(figureInfo);
}

/**
 * Downloads a Plotly chart as SVG if available, otherwise falls back to
 * downloading the first image element as SVG or PNG based on its format.
 *
 * @param {string} fileName - Base name for the downloaded file (without extension)
 */
function dlResult(fileName) {
  try {
    var py = $(document).find(".js-plotly-plot")[0]
    Plotly.toImage(py, { format: 'svg' }).then(function (url) {
      download(fileName + ".svg", url)
    })
    return;
  } catch (e) {
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

//#endregion