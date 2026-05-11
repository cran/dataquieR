
$(function () {
  initIframeMessageBridge();

  escCloseStack = new EscCloseStack();

  overrideLinkBehavior();

  init_dt_utils();

  initDataTablesHeaderFilters();

  addDqUpdateHorizontalOverflowIndicatorEventListener();

  initPlotlyRightClickInterceptor();

  initAutoReload();

  initWidgetRenderingTracker();

})

//#region general init functions

function initDataTablesHeaderFilters() {
  // Keep this ColVis shim. DataTables renders the header filters in the
  // scroll-head clone, so their DOM position is only a visible-column index.
  // After individual Column visibility changes or colvisGroup buttons
  // (All columns, Integrity, ...), we need computeColIndex() to remap the
  // visible filter position back to the data column used by DataTables/export.
  function bindHeaderFilters(dt) {
    if (typeof computeColIndex !== "function") return;

    var wrapper = $(dt.table().container());
    if (!wrapper.length || wrapper.data("dqHeaderFiltersBound")) return;
    wrapper.data("dqHeaderFiltersBound", true);

    var cidx = computeColIndex(dt);
    var filterSelector = ".dataTables_scrollHead thead th input, .dataTables_scrollHead thead td input";

    function dataIndexForInput(input) {
      var $input = $(input);
      var $cell = $input.closest("th,td");
      var visIdx = $cell.parent().children("th,td").index($cell);
      return cidx.dom2data[visIdx];
    }

    function applyHeaderFiltersNow() {
      cidx = computeColIndex(dt);
      dt.columns().search("");

      wrapper.find(filterSelector).each(function () {
        var dataIdx = dataIndexForInput(this);
        if (dataIdx === undefined || dataIdx < 0) return;
        dt.column(dataIdx).search($(this).val());
      });

      dt.draw(false);
    }

    var hfState = { pending: false };
    function scheduleApplyHeaderFilters() {
      if (hfState.pending) return;
      hfState.pending = true;

      requestAnimationFrame(function () {
        requestAnimationFrame(function () {
          hfState.pending = false;
          try { applyHeaderFiltersNow(); } catch (e) {}
        });
      });
    }

    $(dt.table().node()).on("column-visibility.dt.dqHeaderFilters column-reorder.dt.dqHeaderFilters", scheduleApplyHeaderFilters);

    wrapper.on("keyup.dqHeaderFilters search.dqHeaderFilters change.dqHeaderFilters", filterSelector, function () {
      cidx = computeColIndex(dt);
      var dataIdx = dataIndexForInput(this);
      if (dataIdx === undefined || dataIdx < 0) return;
      dt.column(dataIdx).search($(this).val()).draw();
    });
  }

  $(document).on("init.dt", function (e, settings) {
    var api = new $.fn.dataTable.Api(settings);
    setTimeout(function () {
      bindHeaderFilters(api);
    }, 0);
  });

  setTimeout(function () {
    if (!$.fn.dataTable) return;

    $(".dataTables_wrapper").each(function () {
      var id = this.id;
      if (!id || !id.match(/_wrapper$/)) return;

      var tableSelector = "#" + id.replace(/_wrapper$/g, "");
      if (!$.fn.dataTable.isDataTable(tableSelector)) return;
      bindHeaderFilters($(tableSelector).DataTable());
    });
  }, 1000);
}

function initPlotlyRightClickInterceptor() {
  // Keep this for embedded Plotly figures. After a PNG thumbnail is replaced by
  // a Plotly iframe, the inner plot has its own report context menu; stopping
  // right-button down/move events prevents Plotly drag handling from competing
  // with that menu while leaving the contextmenu event itself intact.
  function bindPlot(plotEl) {
    if (plotEl.dataset.dqRightClickInterceptor === "true") return;
    plotEl.dataset.dqRightClickInterceptor = "true";

    plotEl.addEventListener("mousedown", function (e) {
      if (e.button === 2) {
        e.stopImmediatePropagation();
      }
    }, true);

    plotEl.addEventListener("mousemove", function (e) {
      if (e.buttons === 2) {
        e.stopImmediatePropagation();
      }
    }, true);
  }

  function bindPlots() {
    document.querySelectorAll(".js-plotly-plot").forEach(bindPlot);
  }

  bindPlots();
  setTimeout(bindPlots, 1000);
}

/**
 * Initializes a postMessage-based communication bridge between an iframe and its parent window.
 *
 * This function sets up message handlers and helper methods for sending and receiving structured
 * cross-window events such as:
 * - Opening and dismissing context menus
 * - Downloading SVG content
 * - Sending initial size estimates for embedded figures
 * - Toggling figure handles or resetting figure sizes
 *
 * It ensures secure message handling by validating message origins and handling special cases
 * such as local file (`file://`) execution where `window.location.origin` may be null.
 *
 * The bridge adapts behavior depending on whether the script is running inside an iframe or
 * in the top-level window, and updates UI components (e.g., tooltips, scalers, and embedded
 * iframes) in response to incoming messages.
 */
function initIframeMessageBridge() {
  /**
  * Sets origin to "*" for local file usage, otherwise uses the page origin.
  */
  const originToSend =
    (window.location.protocol === "file:") ? "*" : window.location.origin;

  if (window.in_iframe) {
    window.sendMsgContextMenu = function (coords) {
      window.parent.postMessage({
        message: 'contextMenu',
        data: {
          coords: coords
        }
      }, originToSend); // but no relevant disclosure

      // does not work in FF for file:// because origin is null for file://
      // localStorage.setItem('msgContextMenuCoords', JSON.stringify(coords));
      // dataquieRChannel.postMessage('contextMenu');
    }

    window.sendMsgToggleHandle = function (figureInfo) {
      window.parent.postMessage({
        message: 'toggleHandle',
        data: {
          figureInfo: figureInfo
        }
      }, originToSend); // but no relevant disclosure

      // does not work in FF for file:// because origin is null for file://
      // localStorage.setItem('msgContextMenuCoords', JSON.stringify(coords));
      // dataquieRChannel.postMessage('contextMenu');
    }

    window.sendMsgInitialSizeEstimate = function (size) {
      window.parent.postMessage({
        message: 'initialSizeEstimate',
        data: {
          size: size
        }
      }, originToSend); // but no relevant disclosure
    }

    window.sendMsgDismissContextMenu = function () {
      window.parent.postMessage({
        message: "dismissContextMenu",
        data: {}
      }, originToSend);
    };

  } else {
    window.sendMsgDismissContextMenu = function () { // already in top-level
      dismissContextMenu(); // no need to send, just dismiss it
    };


    window.sendMsgDownloadSVG = function (nm) {
      const target = $('div[data-nm="' + nm + '"] iframe')[0]
      target.contentWindow.postMessage({
          message: 'downloadSVG',
          data: {
            nm: nm
          }
        }, originToSend); // but no relevant disclosure

      // does not work in FF for file:// because origin is null for file://
      // localStorage.setItem('msgDownloadSVGNm', JSON.stringify(nm));
      // dataquieRChannel.postMessage('downloadSVG');
    }
  }

  //  dataquieRChannel.onmessage = (messageEvent) => { does not work in FF for file:// because origin is null for file://
  window.onmessage = (event) => {
    if (window.hasOwnProperty("dq_report_by_overview") && window.dq_report_by_overview) {
      return;
    }
    // https://medium.com/@chiragrai3666/exploiting-postmessage-e2b01349c205
    if (event.origin != window.location.origin && // not same origin
      window.location.protocol != "file:"
    ) // file:// likely
      return; // same origin including null


    const msg = event.data.message; // not for datauqieRChannel solution with BroadcastChannel
    const data = event.data.data; // not for datauqieRChannel solution with BroadcastChannel
    if (window.in_iframe) {
      // if (messageEvent.data === 'downloadSVG') { does not work in FF for file:// because origin is null for file://
      if (msg == 'downloadSVG') {
        // var nm = JSON.parse(localStorage.getItem('msgDownloadSVGNm'));  does not work in FF for file:// because origin is null for file://
        const nm = data.nm;
        if ($("#nm").attr("data-nm") == nm) {
          dlResult(nm);
        }
      }
    } else {
      // if (messageEvent.data === 'contextMenu') { does not work in FF for file:// because origin is null for file://
      if (msg === 'contextMenu') {
        $(function () {
          // var coords = JSON.parse(localStorage.getItem('msgContextMenuCoords')); does not work in FF for file:// because origin is null for file://
          const coords = data.coords;
          const $iframe = $('div[data-nm="' + coords.nm + '"] iframe');
          const iframeRect = $iframe[0].getBoundingClientRect();
          const resultContainer = $iframe.parents("div.dataquieR_result")[0];
          const absX = coords.x + iframeRect.left;
          const absY = coords.y + iframeRect.top;

          resultContainer.setAttribute("data-context-area", "plot")

          resultContainer._tippy.setProps({
            followCursor: false,
            getReferenceClientRect: function () {
              return {
                top: absY,
                left: absX,
                bottom: absY,
                right: absX,
                width: 0,
                height: 0,
                x: absX,
                y: absY,
                toJSON: function () { } // required by Popper
              };
            }
          });
          resultContainer._tippy.popper.onclick = function () {
            this._tippy.hide();
          }
          resultContainer._tippy.show()
        })
      } else if (msg === 'dismissContextMenu') {
        dismissContextMenu();
      } else if (msg == 'initialSizeEstimate') {
        const size = data.size;
        $($('div[data-nm="' + size.nm + '"] iframe').parents("div.scaler")[0]).attr("data-initialw", size.w);
        $($('div[data-nm="' + size.nm + '"] iframe').parents("div.scaler")[0]).attr("data-initialh", size.h);
        $(function () {
          sizeIframes(/* forceSetSizeToInit = */ true)
        })
      } else if (msg == 'toggleHandle') {
        const figureInfo = data.figureInfo;
        if (figureInfo && figureInfo.nm &&
          typeof util_scaler_restore_thumbnail_for_nm === "function") {
          if (util_scaler_restore_thumbnail_for_nm(figureInfo.nm)) return;
        }
        // fallback: reset to initial size (legacy behaviour)
        const scaler_div = $('div[data-nm="' + figureInfo.nm + '"] iframe').parent();
        const ih = $(scaler_div).attr("data-initialh").replace(/;+$/g, '');
        const iw = $(scaler_div).attr("data-initialw").replace(/;+$/g, '');
        $(scaler_div).css({ "width": iw })
        $(scaler_div).css({ "height": ih })
        $(scaler_div).height($(scaler_div).height()) // trigger resize events for potly
      }
    }
  }
}

/**
 * Overrides default link navigation behavior by intercepting all anchor (`<a>`) clicks
 * on the document and conditionally preventing navigation based on custom validation logic.
 *
 * This function registers a global click event listener that:
 * - Detects clicks on anchor elements using `closest("a")`
 * - Ignores invalid links, empty hash links (`href="#"`), and links explicitly marked
 *   with `data-no-existance-check`
 * - Delegates navigation validation to `util_checked_navigate()`
 * - Prevents navigation if the validation function returns a falsy result
 *
 * It is primarily used to enforce existence checks or pre-navigation validation logic
 * before allowing the browser to follow a link, ensuring that broken or invalid targets
 * do not result in navigation.
 *
 * The behavior integrates with jQuery for dataset inspection and assumes the presence
 * of a utility function `util_checked_navigate(link, { navigate: false })` that performs
 * the actual validation logic without triggering navigation.
 *
 * @function overrideLinkBehavior
 */
function overrideLinkBehavior() {
  document.addEventListener("click", function (e) {//
    const link = e.target.closest("a");
    if (!link || !link.href) return;
    if (link.getAttribute("href") == "#") return;
    if ($(link).data("no-existance-check")) {
      return;
    }

    // Keep click-check behavior: block navigation if missing
    var ok = util_checked_navigate(link, { navigate: false });
    if (!ok) {
      e.preventDefault();
    }
  });
}

/**
 * Starts an auto-reload watcher for a rendered report.
 *
 * Polls `renderinfo.js` every 5s and reloads the page if:
 * - the reportId changes, or
 * - the render info script fails to load.
 *
 * Uses sessionStorage to track the last known reportId.
 * Skips execution in viewer panes, iframes, or single-result mode.
 *
 */
function initAutoReload() {
  if (window.dataquieR_single_result) {
    return;
  }
  const qs = new URLSearchParams(window.location.search);
  const inViewer = qs.get("viewer_pane") === "1";
  if (inViewer) return; // no auto-reload in RStudio's viewer panel
  const CHECK_MS = 5000;
  let INFO_SRC = "renderinfo.js";
  const KEY = "renderingData";
  let loading = false;
  let RELOAD_URL = "../index.html";

  if (window.by_report) {
    RELOAD_URL = "../../index.html";
    INFO_SRC = "../../.report/renderinfo.js";
  } else if (window.dq_report_by_overview) {
    RELOAD_URL = "index.html";
    INFO_SRC = ".report/renderinfo.js";
  }

  // nur Top-Level-Seite auto-reloaden (keine iframes/dialogs)
  if (window.top && window.top !== window.self) return;

  function getStored() {
    try { return sessionStorage.getItem(KEY) || ""; }
    catch (e) { return ""; }
  }

  function setStored(v) {
    try { sessionStorage.setItem(KEY, v); } catch (e) { }
  }

  function loadRenderInfo(cb) {
    if (loading) return;
    loading = true;

    // Marker leeren → wir sehen sicher, ob Script neu lief
    try { window.renderingData = null; } catch (e) { }

    var s = document.createElement("script");
    s.async = true;
    s.src = INFO_SRC + "?_=" + Date.now(); // Cache-Buster

    s.onload = function () {
      loading = false;
      var data = null;
      try { data = window.renderingData; } catch (e) { }
      cleanup();
      cb(null, data);
    };

    s.onerror = function () {
      loading = false;
      cleanup();
      cb(new Error("renderinfo.js missing"));
    };

    function cleanup() {
      if (s.parentNode) s.parentNode.removeChild(s);
    }

    document.head.appendChild(s);
  }

  function tick() {
    loadRenderInfo(function (err, data) {
      // Datei fehlt → Render offenbar noch nicht fertig
      if (err) {
        location.replace(RELOAD_URL + "?_=" + Date.now());
        return;
      }

      if (!data || !data.reportId) return;

      const rid = String(data.reportId);
      const last = getStored();

      if (last && rid !== last) {
        console.log("Document change detected... reloading the report...")
        // maybe, the report is not in index.html, currently. write content_file to the js. -- renderingInfo maybe stored twice. this causes reload-loops
        setStored(rid);
        location.replace(RELOAD_URL + "?_=" + Date.now());
        return;
      }

      if (!last) setStored(rid);
    });
  }

  setInterval(tick, CHECK_MS);
}

/**
 * initWidgetRenderingTracker
 *
 * Tracks when all DataTables and Plotly widgets have finished their initial render.
 * Once all tracked widgets are complete, it triggers the custom event:
 *   'dq:allWidgetsReady'
 *
 * It also listens for that event to update the UI with rendering time information.
 */
function initWidgetRenderingTracker() {
  let pending = 0;
  let seenAnything = false;
  let finished = false;

  function addPending() {
    pending++;
    seenAnything = true;
  }

  function markDone() {
    if (pending > 0) {
      pending--;
    }
    tryFinish();
  }

  function tryFinish() {
    if (!finished && seenAnything && pending === 0) {
      finished = true;
      $(document).trigger('dq:allWidgetsReady');
    }
  }

  // ----------------------------
  // DataTables
  // ----------------------------
  $(document).on('preInit.dt', function (e, settings) {
    if (settings._dqTracked) {
      return;
    }
    settings._dqTracked = true;
    addPending();
  });

  $(document).on('draw.dt', function (e, settings) {
    if (!settings._dqTracked || settings._dqReady) {
      return;
    }
    settings._dqReady = true;
    markDone();
  });

  // ----------------------------
  // Plotly
  // ----------------------------
  function hookPlotly(el) {
    if (!el || el._dqTracked) {
      return;
    }

    el._dqTracked = true;
    addPending();

    el.on('plotly_afterplot', function () {
      if (el._dqReady) {
        return;
      }
      el._dqReady = true;
      markDone();
    });
  }

  function scanPlotly(root) {
    $(root).find('.js-plotly-plot').addBack('.js-plotly-plot').each(function () {
      hookPlotly(this);
    });
  }

  // already existing Plotly graphs
  scanPlotly(document);

  // newly inserted Plotly graphs
  var mo = new MutationObserver(function (mutations) {
    mutations.forEach(function (m) {
      $(m.addedNodes).each(function () {
        if (this.nodeType === 1) {
          scanPlotly(this);
        }
      });
    });
  });

  $(function () {
    mo.observe(document.body, {
      childList: true,
      subtree: true
    });
  });

  // fallback: if there are no tracked widgets at all
  $(function () {
    setTimeout(tryFinish, 0);
  });


  $(document).on('dq:allWidgetsReady', function () {
    // All DataTables and Plotly widgets finished their initial render.
    var data = window.renderingData
    var rt = "no rendering time available."
    if (data && data.hasOwnProperty("renderingTime")) {
      rt = data.renderingTime
    }
    var update = function () {
      $('[data-content="renderingTime"]').html(rt)
    }
    $(function () {
      setTimeout(update, 0);
    });
  });
}

//#endregion

//#region generalfunction
/**
 * Triggers a client-side download by creating and clicking a temporary anchor element.
 * 
 * https://stackoverflow.com/a/18197341
 *
 * @param {string} filename - The name of the file to be downloaded.
 * @param {string} text - The file content or data URL to be used as the download source.
 *
 * @returns {void}
 */
function download(filename, text) {
  var element = document.createElement('a');
  element.setAttribute('href', text);
  element.setAttribute('download', filename);
  element.setAttribute('target', "_blank"); // firefox displays pngs right here
  element.setAttribute('data-no-existance-check', true)

  element.style.display = 'none';
  document.body.appendChild(element);

  element.click();

  document.body.removeChild(element);
}

/**
 * Displays a modal yes/no confirmation dialog using jQuery UI.
 * 
 * https://stackoverflow.com/a/10753619
 *
 * @param {string} message - The message to display in the dialog (a question mark is appended automatically).
 * @param {Function} yes - Callback executed when the user clicks "Yes".
 * @param {Function} no - Callback executed when the user clicks "No" or closes the dialog.
 *
 * @returns {void}
 */
function yesnoDialog(message, yes, no) {
  sendMsgDismissContextMenu()
  $('<div></div>').appendTo('body')
    .html('<div><h6>' + message + '?</h6></div>')
    .dialog({
      modal: true,
      title: 'Delete message',
      zIndex: 10000,
      autoOpen: true,
      width: 'auto',
      resizable: false,
      buttons: {
        Yes: function () {
          $(this).dialog("close");
          yes()
        },
        No: function () {
          $(this).dialog("close");
          no()
        }
      },
      close: function (event, ui) {
        $(this).remove();
      }
    });
};

/**
 * Capitalizes the first character of a string.
 *
 * @param {string} str - The input string.
 * @returns {string} The string with its first character converted to uppercase.
 */
function ucfirst(str) {
  return str.charAt(0).toUpperCase() + str.slice(1);
}

/**
 * Validates navigation to a URL and blocks it if it is outside the allowed scope or not in `all_ids`.
 *
 * @param {string|URL|HTMLAnchorElement} target - Navigation target.
 * @param {Object} [opts] - Options.
 * @param {boolean} [opts.silent=false] - Suppress alert on failure.
 * @returns {boolean} True if navigation is allowed, false otherwise.
 */
function util_checked_navigate(target, opts) {
  opts = opts || {};
  var currentUrl = new URL(window.location.href);

  // Accept: string URL, URL object, or <a> element
  var targetUrl;
  try {
    if (target && typeof target === "object" && target.tagName === "A") {
      if (!target.href) return true;
      targetUrl = new URL(target.href, currentUrl);
    } else if (target && typeof target === "object" && target.href) {
      // URL-like
      targetUrl = new URL(target.href, currentUrl);
    } else {
      targetUrl = new URL(String(target), currentUrl);
    }
  } catch (e) {
    return true; // if it's not a valid URL, don't block
  }

  var isFileProtocol = currentUrl.protocol === "file:";
  var shouldCheck = false;

  if (isFileProtocol) {
    // file:// -> same directory or below
    var currentDir = currentUrl.pathname.replace(/[^/]+$/, "");
    shouldCheck = targetUrl.pathname.startsWith(currentDir);
  } else {
    // http(s) -> same host + path prefix
    var isSameHost = targetUrl.origin === currentUrl.origin;
    var currentDirHttp = currentUrl.pathname.replace(/[^/]+$/, "");
    var isSameDir = targetUrl.pathname.startsWith(currentDirHttp);
    shouldCheck = isSameHost && isSameDir;
  }

  if (shouldCheck) {
    var baseName = null
    if (window.hasOwnProperty("dq_report_by_overview") && window.dq_report_by_overview) {
      baseName = targetUrl.pathname.split("/").slice(-3).join("/") + targetUrl.hash;
    } else {
      baseName = targetUrl.pathname.split("/").pop() + targetUrl.hash;
    }


    if (!all_ids.all_ids.includes(baseName) &&
      !all_ids.all_ids.includes(decodeURI(baseName))) {
      if (!opts.silent) {
        alert("The requested output does not exist: " + baseName + ". Sorry, this is a bug, please report.");
      }
      return false;
    }
  }

  // If requested, actually navigate
  if (opts.navigate) {
    window.location.href = targetUrl.href;
  }

  return true;
}

//#endregion

//#region Plotly

//#region PlotlyIcons

/**
 * Creates a custom Plotly toolbar icon.
 * 
 * It’s neither red nor a “home”.
 * 
 * Currently used for restore initial size in the Plotly toolbar.
 * 
 * @returns {{name: string, svg: string}} An object containing the icon name and inline SVG markup for use in Plotly modebars.
 */
function PlotlyIconshomeRED() {
  // https://stackoverflow.com/a/5364657
  //  var res = jQuery.extend(true, {}, Plotly.Icons.home);
  //  res.style = "fill:#800100";

  const res = {
    name: 'homeRED',
    svg: [ // chatgpt created this icon 2024-10-25 3 PM, STS
      '<svg width="24" height="24" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg" fill="none">',
      '<rect x="6" y="6" width="12" height="12" rx="2" fill="#E0E0E0" stroke="#333" stroke-width="1.5"/>',
      '<path d="M12 2a10 10 0 1 0 10 10" fill="none" stroke="#333" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"/>',
      '<path d="M16 2h-4v4" fill="none" stroke="#333" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"/>',
      '<circle cx="9" cy="9" r="1.5" fill="#333"/>',
      '</svg>'
    ].join('')
  }
  return res;
}

/**
 * Creates a custom Plotly toolbar icon.
 * 
 * its a printer.
 *
 * @returns {{name: string, svg: string}} Icon metadata with inline SVG for Plotly modebar usage.
 */
function PlotlyIconsprinter() {
  const res = {
    name: 'printer',
    svg: [
      '<svg width="24" height="24" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg" fill="none">',
      '<rect x="6" y="3" width="12" height="5" rx="1" fill="#E0E0E0" stroke="#333" stroke-width="1.5"/>',
      '<rect x="4" y="8" width="16" height="10" rx="2" fill="white" stroke="#333" stroke-width="1.5"/>',
      '<rect x="8" y="14" width="8" height="5" rx="0.5" fill="#F5F5F5" stroke="#333" stroke-width="1.2"/>',
      '<circle cx="17" cy="11" r="0.8" fill="#333"/>',
      '<line x1="9" y1="16" x2="15" y2="16" stroke="#333" stroke-width="1"/>',
      '<line x1="9" y1="18" x2="15" y2="18" stroke="#333" stroke-width="1"/>',
      '</svg>'
    ].join('')
  };
  return res;
}

/**
 * Creates a custom Plotly toolbar icon.
 * 
 * its a PDF icon...
 *
 * @returns {{name: string, svg: string}} An object containing the icon name and inline SVG markup for use in Plotly modebars.
 */
function PlotlyIconspdf() {
  return {
    name: 'pdf',
    svg: [
      '<svg width="24" height="24" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">',
      // Document shape
      '<path d="M6 2h9l5 5v13a2 2 0 0 1-2 2H6a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2z" fill="#ffffff" stroke="#333" stroke-width="1.5"/>',
      // Fold line
      '<path d="M15 2v5h5" fill="none" stroke="#333" stroke-width="1.5"/>',
      // Red PDF badge
      '<rect x="4.5" y="12.5" width="15" height="7" rx="1.8" fill="#D32F2F"/>',
      // PDF text (system font fallback; no embedded font)
      '<text x="12" y="18" text-anchor="middle" font-size="7.5" font-weight="800" fill="#fff" font-family="Arial, Helvetica, sans-serif">PDF</text>',
      '</svg>'
    ].join('')
  };
}

//#endregion

/**
 * Exports a Plotly graph as a PDF file with optional font scaling, cropping, and DPI settings.
 *
 * @param {HTMLElement} gd - The Plotly graph DOM element.
 * @param {Object} [options={}] - Export configuration options.
 * @param {"landscape"|"portrait"} [options.orientation="landscape"] - PDF page orientation.
 * @param {number} [options.dpi=300] - Export resolution in DPI.
 * @param {"auto"|"none"|"square"} [options.crop="auto"] - Cropping mode for the exported image.
 * @param {number} [options.font_scale=1.4] - Scaling factor for text during export.
 * @param {number} [options.min_text_pt=6] - Minimum font size in points.
 * @param {number} [options.font_gamma=1.6] - Gamma correction for non-linear font scaling.
 * @param {string} [options.filename="plot.pdf"] - Output PDF filename.
 *
 * @returns {void}
 */
function downloadPlotlyAsPDF(gd, options = {}) {
  const jsPDF = window.jsPDF || (window.jspdf && window.jspdf.jsPDF);
  if (!jsPDF) {
    console.error("jsPDF not found on window");
    alert("PDF export is not available (jsPDF not loaded). dataquieR needs the package visNetwork for this feature.");
    return;
  }

  const orientation = options.orientation || "landscape";
  const dpi = options.dpi || 300;

  // crop: "auto" (default), "none", "square"
  const cropMode = options.crop || "auto";

  // Font scaling for export only
  const fontScale = (typeof options.font_scale === "number" && isFinite(options.font_scale))
    ? options.font_scale
    : 1.4;

  const minTextPt = (typeof options.min_text_pt === "number" && isFinite(options.min_text_pt))
    ? options.min_text_pt
    : 6;

  // Optional: steepen gradient for arrays (wordcloud-ish); 1 = linear
  const fontGamma = (typeof options.font_gamma === "number" && isFinite(options.font_gamma))
    ? options.font_gamma
    : 1.6;

  // pt -> px (Plotly sizes are usually px)
  const minTextPx = Math.ceil(minTextPt * 96 / 72);

  // IMPORTANT: to keep the *current* DOM-rendered sizing (incl. font gradient),
  // we export using scale only, not width/height (which can re-layout sunburst text).
  const exportScale = dpi / 96;

  let nm = $("#nm");
  if (nm.length === 1) {
    nm = nm.data("nm");
  } else {
    nm = location.pathname
      .split("/")
      .reverse()[0]
      .replace(/^FIG_/, "")
      .replace(/\.html$/, "");
  }

  let filename = options.filename || nm || "plot.pdf";
  if (!filename.toLowerCase().endsWith(".pdf")) filename += ".pdf";

  // A4 in Inch
  const a4in = orientation === "landscape" ? [11.69, 8.27] : [8.27, 11.69];
  const pageWpt = a4in[0] * 72;
  const pageHpt = a4in[1] * 72;

  function traceType0() {
    const fd = gd && (gd._fullData || gd.data);
    return (fd && fd[0] && fd[0].type) ? fd[0].type : null;
  }

  function shouldSquareCrop() {
    if (cropMode === "square") return true;
    if (cropMode === "none") return false;
    const t = traceType0();
    return t === "sunburst" || t === "pie";
  }

  function imageFromDataUrl(dataUrl) {
    return new Promise(function (resolve, reject) {
      const img = new Image();
      img.onload = function () { resolve(img); };
      img.onerror = reject;
      img.src = dataUrl;
    });
  }

  function cropToCenterSquare(dataUrl, w, h) {
    return imageFromDataUrl(dataUrl).then(function (img) {
      const side = Math.min(w, h);
      const sx = Math.floor((w - side) / 2);
      const sy = Math.floor((h - side) / 2);

      const canvas = document.createElement("canvas");
      canvas.width = side;
      canvas.height = side;

      const ctx = canvas.getContext("2d");
      ctx.drawImage(img, sx, sy, side, side, 0, 0, side, side);

      return { dataUrl: canvas.toDataURL("image/png"), w: side, h: side };
    });
  }

  // Temporarily scale + clamp fonts for export, then restore
  function withExportFonts(fnPromise) {
    const t0 = traceType0();

    const origLayoutFontSize =
      gd && gd.layout && gd.layout.font && typeof gd.layout.font.size === "number"
        ? gd.layout.font.size
        : null;

    const origTextfontSize =
      gd && gd.data && gd.data[0] && gd.data[0].textfont
        ? gd.data[0].textfont.size
        : null;

    function scaleOneLinear(n) {
      return Math.max(Math.round(n * fontScale), minTextPx);
    }

    function scaleSizeArray(arr) {
      // Apply min clamp + optional steepening on the existing size distribution
      const nums = arr.map(Number).filter(function (x) { return isFinite(x); });
      if (!nums.length) return arr;

      const lo = Math.min.apply(null, nums);
      const hi = Math.max.apply(null, nums);

      // If no spread, just linear scaling
      if (!(hi > lo + 0.5) || !(fontGamma > 1)) {
        return arr.map(function (v) {
          const n = Number(v);
          if (!isFinite(n)) return v;
          return scaleOneLinear(n);
        });
      }

      // Power-remap in [0..1] then scale
      return arr.map(function (v) {
        const n = Number(v);
        if (!isFinite(n)) return v;

        const t = (n - lo) / (hi - lo);      // 0..1
        const t2 = Math.pow(t, fontGamma);   // steeper for gamma>1
        const n2 = lo + (hi - lo) * t2;

        return Math.max(Math.round(n2 * fontScale), minTextPx);
      });
    }

    function scaleSizeValue(v) {
      if (Array.isArray(v)) return scaleSizeArray(v);
      if (typeof v === "number") return scaleOneLinear(v);
      return v;
    }

    const restore = [];
    const ops = [];

    if (origLayoutFontSize != null) {
      const newLayoutFontSize = scaleOneLinear(origLayoutFontSize);
      ops.push(Plotly.relayout(gd, { "font.size": newLayoutFontSize }));
      restore.push(function () {
        return Plotly.relayout(gd, { "font.size": origLayoutFontSize });
      });
    }

    if (origTextfontSize != null && (t0 === "sunburst" || t0 === "pie" || t0 === "treemap")) {
      const newTextfontSize = scaleSizeValue(origTextfontSize);
      ops.push(Plotly.restyle(gd, { "textfont.size": newTextfontSize }, [0]));
      restore.push(function () {
        return Plotly.restyle(gd, { "textfont.size": origTextfontSize }, [0]);
      });
    }

    function waitAfterplotOnce() {
      return new Promise(function (resolve) {
        if (gd && typeof gd.once === "function") {
          gd.once("plotly_afterplot", function () { resolve(); });
          Plotly.redraw(gd);
        } else {
          // fallback; usually enough
          requestAnimationFrame(function () { resolve(); });
        }
      });
    }

    return Promise.all(ops)
      .then(waitAfterplotOnce)
      .then(fnPromise)
      .finally(function () {
        restore.reduce(function (p, rfn) {
          return p.then(function () { return rfn(); }).catch(function () { return null; });
        }, Promise.resolve());
      });
  }

  withExportFonts(function () {
    // Key point: scale-only export keeps current DOM rendering (incl. font gradient)
    return Plotly.toImage(gd, {
      format: "png",
      scale: exportScale
    });
  }).then(function (dataUrl) {
    if (!dataUrl || dataUrl === "data:,") {
      console.error("Empty image generated.");
      alert("PDF export failed.");
      return null;
    }

    return imageFromDataUrl(dataUrl).then(function (imgEl) {
      const w = imgEl.naturalWidth || imgEl.width;
      const h = imgEl.naturalHeight || imgEl.height;

      if (!shouldSquareCrop()) {
        return { dataUrl: dataUrl, w: w, h: h };
      }
      return cropToCenterSquare(dataUrl, w, h);
    });
  }).then(function (img) {
    if (!img) return;

    const pdfOrientation = orientation === "landscape" ? "l" : "p";
    const pdf = new jsPDF(pdfOrientation, "pt", [pageWpt, pageHpt]);

    const margin = 20;
    const availW = pageWpt - 2 * margin;
    const availH = pageHpt - 2 * margin;

    const aspect = img.w / img.h;

    let drawW = availW;
    let drawH = drawW / aspect;
    if (drawH > availH) {
      drawH = availH;
      drawW = drawH * aspect;
    }

    const x = margin + (availW - drawW) / 2;
    const y = margin + (availH - drawH) / 2;

    pdf.addImage(img.dataUrl, "PNG", x, y, drawW, drawH);
    pdf.save(filename);
  }).catch(function (err) {
    console.error("Export failed:", err);
    alert("Could not export to PDF.");
  });
};


/**
 * EscCloseStack
 * -------------
 * A utility that manages a stack of callback functions that are executed
 * when the user presses the Escape key.
 *
 * Useful for UI patterns like:
 * - closing modals
 * - closing dropdowns
 * - dismissing overlays
 *
 * Behavior:
 * - Functions are stored in a stack (LIFO)
 * - On Escape press, the most recently added function is executed and removed
 */
class EscCloseStack {
  _escCloseStack = [];

  /**
   * Creates the EscCloseStack and attaches a global keydown listener.
   */
  constructor() {
    document.addEventListener('keydown', event => {
      if (event.key === 'Escape' && this._escCloseStack.length > 0) {
        const fn = this._escCloseStack.pop();
        if (typeof fn === 'function') { fn(); }
      }
    });
  }

  /**
   * Push a callback to be executed on Escape key press
   * @param {Function} fn
   */
  pushEscCloseHandler(fn) {
    this._escCloseStack.push(fn);
  }

  /**
  * Remove a specific callback from the stack
  * @param {Function} fn
  */
  popEscCloseHandler(fn) {
    const i = this._escCloseStack.lastIndexOf(fn);
    if (i >= 0) this._escCloseStack.splice(i, 1);
  }


}

/**
 * @type {EscCloseStack}
 */
let escCloseStack;

/**
 * Exports a full Plotly image without permanently altering the current viewport or drilldown state.
 *
 * Temporarily resets interactive view settings (e.g. axis ranges or sunburst level/maxdepth),
 * triggers a Plotly image download, and then restores the original state.
 *
 * @param {Object} gd - The Plotly graph DOM element.
 * @param {Object} plotlydl_args - Arguments passed to Plotly.downloadImage (format, scale, etc.).
 *
 * @returns {Promise<void>|void}
 */
function saveFullImageWithoutLosingView(gd, plotlydl_args) {

  if (typeof Plotly === 'undefined' || !gd) return;

  const layout = (gd && gd.layout) ? gd.layout : {};
  const data = (gd && gd.data) ? gd.data : [];

  // --- helper: safe promise wrapper for relayout/restyle ---
  function dq_p(x) { return Promise.resolve(x); }

  // --- detect sunburst (trace 0 by default) ---
  const ti = 0;
  const t0 = data[ti] || {};
  const isSunburst = (t0.type === 'sunburst') ||
    (gd._fullData && gd._fullData[ti] && gd._fullData[ti].type === 'sunburst');

  if (isSunburst) {
    // Current drilldown state is trace-level, not axis-level.
    // Prefer user-data, fall back to _fullData if needed.
    const full0 = (gd._fullData && gd._fullData[ti]) ? gd._fullData[ti] : {};
    const prevLevel =
      (t0.level !== undefined) ? t0.level :
        (full0.level !== undefined) ? full0.level :
          undefined;

    const prevMaxdepth =
      (t0.maxdepth !== undefined) ? t0.maxdepth :
        (full0.maxdepth !== undefined) ? full0.maxdepth :
          undefined;

    // For "full image": go to root + show all depths.
    // maxdepth: Plotly uses -1 / undefined semantics across versions;
    // setting -1 is usually "no limit".
    const toRoot = { level: '' };
    if (prevMaxdepth !== undefined) toRoot.maxdepth = -1;

    return dq_p(Plotly.restyle(gd, toRoot, [ti]))
      .then(() => Plotly.downloadImage(gd, plotlydl_args))
      .then(() => {
        const restore = {};
        if (prevLevel !== undefined) restore.level = prevLevel;
        else restore.level = null; // remove attribute if it wasn't set

        if (prevMaxdepth !== undefined) restore.maxdepth = prevMaxdepth;
        else if (toRoot.maxdepth !== undefined) restore.maxdepth = null;

        return Plotly.restyle(gd, restore, [ti]);
      })
      .catch((err) => console.error(err));
  }

  // --- cartesian fallback (your original intent), but robust ---
  const hasXY = layout.xaxis && layout.yaxis;

  if (!hasXY) {
    // non-cartesian but not sunburst -> just download
    try { return Plotly.downloadImage(gd, plotlydl_args); } catch (e) { console.error(e); }
    return;
  }

  const currentView = {};
  if (Array.isArray(layout.xaxis.range)) currentView['xaxis.range'] = layout.xaxis.range.slice();
  if (Array.isArray(layout.yaxis.range)) currentView['yaxis.range'] = layout.yaxis.range.slice();
  if (layout.xaxis.autorange !== undefined) currentView['xaxis.autorange'] = layout.xaxis.autorange;
  if (layout.yaxis.autorange !== undefined) currentView['yaxis.autorange'] = layout.yaxis.autorange;

  return dq_p(Plotly.relayout(gd, { 'xaxis.autorange': true, 'yaxis.autorange': true }))
    .then(() => Plotly.downloadImage(gd, plotlydl_args))
    .then(() => Plotly.relayout(gd, currentView))
    .catch((err) => console.error(err));
}

/**
 * Opens a custom export menu for a Plotly graph, allowing the user to download the plot
 * in multiple image formats (SVG, PNG, JPEG, WebP) or open advanced export options.
 *
 * The menu is positioned near the Plotly modebar and automatically closes when clicking
 * outside or pressing Escape.
 * 
 * @param {Object} gd - The Plotly graph DOM element.
 *
 * @returns {void}
 */
function plotlyDL(gd) {
  if (!gd.id) {
    gd.id = 'plotly_' + Math.random().toString(36).substr(2, 9);
  }
  let nm = $("#nm");
  if (nm.length == 1) {
    nm = $("#nm").data("nm");
  } else {
    nm = location.pathname.split('/').reverse()[0].replace(/^FIG_/, "").replace(/\.html$/, "")
  }

  const formats = ['svg', 'png', 'jpeg', 'webp'];
  const menu = document.createElement('div');
  const menuId = 'export_menu_' + gd.id;
  menu.id = menuId;
  menu.style.position = 'absolute';
  menu.style.background = '#fff';
  menu.style.border = '1px solid #ccc';
  menu.style.padding = '5px';
  menu.style.zIndex = 9999;
  menu.style.fontFamily = 'Arial, sans-serif';
  menu.style.boxShadow = '0 4px 8px rgba(0,0,0,0.1)';

  formats.forEach(function (fmt) {
    let dlArgs = {
      format: fmt,
      filename: nm
    };

    const item = document.createElement('div');
    item.style.display = 'flex';
    item.style.justifyContent = 'space-between';
    item.style.alignItems = 'center';
    item.style.cursor = 'pointer';
    item.style.padding = '4px 8px';

    const label = document.createElement('span');
    label.innerText = 'Download as ' + ucfirst(fmt);
    label.style.flex = '1';
    label.onmouseover = () => { item.style.background = '#eee'; };
    label.onmouseout = () => { item.style.background = '#fff'; };
    label.onclick = function () {
      Plotly.downloadImage(gd, dlArgs);
      document.body.removeChild(menu);
    };

    const more = document.createElement('button');
    more.innerText = '\u2026';
    more.title = 'Advanced Options';
    more.style.marginLeft = '8px';
    more.onclick = function (e) {
      e.stopPropagation();
      customPlotlyDownloadDlg(gd, dlArgs);
      document.body.removeChild(menu);
    };

    item.appendChild(label);
    item.appendChild(more);
    menu.appendChild(item);
  });

  const existing = document.getElementById(menuId);
  if (existing) document.body.removeChild(existing);
  document.body.appendChild(menu);
  setTimeout(() => {
    const clickAwayHandler = function (event) {
      if (!menu.contains(event.target)) {
        if (document.body.contains(menu)) {
          document.body.removeChild(menu);
        }
        document.removeEventListener('click', clickAwayHandler);
        escCloseStack.popEscCloseHandler(escMenuHandler);
      }
    };
    document.addEventListener('click', clickAwayHandler);

    const escMenuHandler = function () {
      if (document.body.contains(menu)) {
        document.body.removeChild(menu);
      }
      document.removeEventListener('click', clickAwayHandler);
      escCloseStack.popEscCloseHandler(escMenuHandler);
    };
    escCloseStack.pushEscCloseHandler(escMenuHandler);
  }, 0);

  const bbox = gd._fullLayout._modeBar.element.getBoundingClientRect();
  menu.style.left = (bbox.left + 40) + 'px';
  menu.style.top = (bbox.top + 40) + 'px';
}

/**
 * Opens a custom Plotly export dialog allowing the user to configure image width, height,
 * resolution (DPI), and export method before downloading a figure.
 *
 * The dialog supports exporting via Plotly's native renderer ("Current View") or a full-figure
 * export that preserves and restores the current visualization state.
 * 
 * @param {Object} gd - The Plotly graph DOM element.
 * @param {Object} dlArgs - Base download arguments passed to Plotly.downloadImage (e.g., format, filename).
 *
 * @returns {void}
 */
function customPlotlyDownloadDlg(gd, dlArgs) {

  if (!gd.id) {
    gd.id = 'plotly_' + Math.random().toString(36).substr(2, 9);
  }
  const currentWidth = gd._fullLayout.width || 800;
  const currentHeight = gd._fullLayout.height || 600;

  const isVector = dlArgs.format === "svg";

  const dialog = document.createElement('div');
  const dialogId = 'custom_plotly_dialog_' + gd.id;
  dialog.id = dialogId;

  dialog.style.position = 'fixed';
  dialog.style.right = '20px'; // keep right, next to the download button -- so visible even in hte iframe
  dialog.style.top = '20px'; // Keep on top, next to the download button -- so visible even in the iframe
  dialog.style.background = '#fff';
  dialog.style.padding = '20px';
  dialog.style.border = '1px solid #ccc';
  dialog.style.zIndex = 10000;
  dialog.style.boxShadow = '0 8px 16px rgba(0,0,0,0.2)';
  dialog.style.fontFamily = 'Arial, sans-serif';

  dialog.innerHTML = `
    <h3>Export as ${ucfirst(dlArgs.format)}</h3>
    <label>
      Width (px): <input type="number" id="imgWidth" value="${currentWidth}" style="width: 80px;">
    </label><br><br>
    <label>
      Height (px): <input type="number" id="imgHeight" value="${currentHeight}" style="width: 80px;">
    </label><br><br>
    <div id="dpiSection" style="${isVector ? 'display: none;' : ''}">
      <label>
        Resolution (DPI): <input type="number" id="imgDpi" value="96" style="width: 80px;">
      </label><br><br>
    </div>
    <label>
      Method:
      <select id="methodSelect">
        <option value="plotly">Current View</option>
        <option value="custom">Full Figure</option>
      </select>
    </label><br><br>
    <button id="exportConfirm">Download</button>
    <button id="exportCancel">Cancel</button>
  `;

  document.body.appendChild(dialog);

  setTimeout(function () {
    const clickAwayHandler = function (event) {
      if (!dialog.contains(event.target)) {
        if (document.body.contains(dialog)) {
          document.body.removeChild(dialog);
        }
        document.removeEventListener('click', clickAwayHandler);
        escCloseStack.popEscCloseHandler(escDialogHandler);
      }
    };
    document.addEventListener('click', clickAwayHandler);

    const escDialogHandler = function () {
      if (document.body.contains(dialog)) {
        document.body.removeChild(dialog);
      }
      document.removeEventListener('click', clickAwayHandler);
      escCloseStack.popEscCloseHandler(escDialogHandler);
    };
    escCloseStack.pushEscCloseHandler(escDialogHandler);
  }, 0);

  document.getElementById('exportConfirm').onclick = function () {
    const width = parseInt(document.getElementById('imgWidth').value);
    const height = parseInt(document.getElementById('imgHeight').value);
    const dpi = parseInt(document.getElementById('imgDpi')?.value || 96);
    const method = document.getElementById('methodSelect').value;

    const args = Object.assign({}, dlArgs, {
      width,
      height,
      ...(dlArgs.format !== 'svg' ? { scale: dpi / 96 } : {})
    });

    if (method === 'plotly') {
      Plotly.downloadImage(gd, args);
    } else {
      saveFullImageWithoutLosingView(gd, args);
    }

    document.body.removeChild(dialog);
  };

  document.getElementById('exportCancel').onclick = function () {
    document.body.removeChild(dialog);
  };
}


/**
 * Opens a print dialog for a Plotly graph using a jQuery UI modal and generates a print-ready image.
 *
 * The function:
 * - Determines optimal page orientation based on plot aspect ratio
 * - Lets the user choose A4 orientation and DPI
 * - Renders the Plotly chart to a high-resolution PNG
 * - Opens a new window with print-optimized CSS and triggers the browser print dialog
 *
 * @param {Object} gd - The Plotly graph DOM element.
 *
 * @returns {void}
 */
function plotlyPrint(gd) {
  console.log("plotlyPrint");
  
  // Inject the dialog if it hasn't been added yet
  injectPlotlyPrintDialog();
  

  // Determine plot aspect ratio (default orientation)
  const bbox = gd.getBoundingClientRect();
  const isLandscape = bbox.width >= bbox.height;

  // Set the default value of the orientation selector
  $(document).ready(function () {
    $("#print-orientation").val(isLandscape ? "landscape" : "portrait");
  });

  // Open jQuery UI dialog
  $("#plotly-print-dialog").dialog({
    modal: true,
    buttons: {
      "Print Plot": function () {
        const orientation = $("#print-orientation").val();
        const dpi = parseInt($("#print-dpi").val(), 10);
        $(this).dialog("close");

        // Convert mm → pixels
        const mmToInch = 1 / 25.4;
        const a4mm = orientation === "landscape" ? [297, 210] : [210, 297];
        const widthPx = Math.round(a4mm[0] * mmToInch * dpi);
        const heightPx = Math.round(a4mm[1] * mmToInch * dpi);
        const pageCSS = `@page { size: A4 ${orientation}; margin: 0; }`;

        Plotly.toImage(gd, {
          format: "png",
          width: widthPx,
          height: heightPx,
          scale: 1
        }).then(function (dataUrl) {
          const win = window.open("", "", "width=1000,height=700");
          win.document.write(`
            <html>
              <head>
                <title>Print Plot</title>
                <style>
                  ${pageCSS} /* MUST be top-level */

                  html, body {
                    margin: 0;
                    padding: 0;
                    background: white;
                    height: 100vh;
                    overflow: hidden;
                    text-align: center;
                  }

                  img {
                    width: 99.5%;
                    height: auto;
                    max-height: 100vh;
                    page-break-before: avoid;
                    page-break-after: avoid;
                    page-break-inside: avoid;
                    break-before: avoid;
                    break-after: avoid;
                    break-inside: avoid;
                  }

                  @media print {
                    html, body {
                      margin: 0;
                      padding: 0;
                      background: white;
                      height: 100vh;
                      overflow: hidden;
                    }

                    img {
                      page-break-before: avoid;
                      page-break-after: avoid;
                      page-break-inside: avoid;
                      break-before: avoid;
                      break-after: avoid;
                      break-inside: avoid;
                    }
                  }
                </style>
              </head>
              <body>
                <img src="${dataUrl}" />
              </body>
            </html>
          `);
          win.document.close();
          win.focus();
          setTimeout(() => {
            win.print();
            win.close();
          }, 500);
        });
      },
      Cancel: function () {
        $(this).dialog("close");
      }
    }
  });
}

/**
 * Injects a hidden jQuery UI print dialog into the DOM for Plotly export/printing controls.
 *
 * The dialog is only added once per page load and provides UI fields for selecting
 * page orientation and DPI resolution.
 *
 * @returns {void}
 */
function injectPlotlyPrintDialog() {
  if (document.getElementById("plotly-print-dialog")) return; // Already added

  const dialogHtml = `
    <div id="plotly-print-dialog" title="Print Plot" style="display:none;">
      <form>
        <fieldset>
          <label for="print-orientation">Orientation:</label>
          <select id="print-orientation" name="orientation">
            <option value="portrait">Portrait (A4)</option>
            <option value="landscape" selected>Landscape (A4)</option>
          </select><br><br>

          <label for="print-dpi">Resolution (DPI):</label>
          <select id="print-dpi" name="dpi">
            <option value="150">150 DPI</option>
            <option value="300" selected>300 DPI</option>
            <option value="600">600 DPI</option>
          </select>
        </fieldset>
      </form>
    </div>
  `;

  const container = document.createElement("div");
  container.innerHTML = dialogHtml;
  document.body.appendChild(container.firstElementChild);
}

//#endregion

//#region DataTable stuff

/**  
 * --- util: DT responsive caps + resize adjust --- 
 */
function init_dt_utils() {
  "use strict";

  /**
 * Finds the closest container element for a DataTable.
 * 
 * - Prefers `.table_result` wrapper
 * - Falls back to `.dq-table-cell` in grid layouts
 * - Returns null if not found or on error
 * 
 * @param {object} api - DataTables API instance
 * @returns {jQuery|null}
 */
  function util_dt_find_table_result(api) {
    try {
      const n = api.table().node();
      if (!n) return null;
      // Prefer the classic wrapper used by dataquieR.
      const $tr = $(n).closest(".table_result");
      if ($tr.length) return $tr;

      // In the plot/table grid layout, the DT widget can live directly
      // inside the table cell (without an intermediate .table_result).
      const $cell = $(n).closest(".dq-plot-table-grid .dq-table-cell");
      return $cell.length ? $cell : null;
    } catch (e) {
      return null;
    }
  }

  /**
 * Normalizes a column count value.
 * 
 * - Converts input to number
 * - Defaults to 99 if invalid
 * - Ensures minimum of 1
 * - Caps at 15, otherwise returns 99
 * 
 * @param {number|string} colCount
 * @returns {number}
 */
  function util_dt_prosses_cols_number(colCount) {
    if (Number.isNaN(Number(colCount))) colCount = 99;
    colCount = colCount >= 1 ? Math.ceil(colCount) : 1;
    return colCount <= 15 ? colCount : 99;
  }

  /**
 * Generates a CSS class based on column count.
 * 
 * Format: "dt-cols-{n}"
 * 
 * @param {number|string} colCount
 * @returns {string}
 */
  function util_dt_get_cols_class(colCount) {
    return "dt-cols-" + util_dt_prosses_cols_number(colCount);
  }

  /**
 * Increments a dt-cols-* class safely.
 * 
 * - If invalid format → returns "dt-cols-99"
 * - Otherwise increments column count
 * 
 * @param {string} cls
 * @returns {string}
 */
  function util_dt_bump_cols_class(cls) {
    const front = cls.substr(0, 8);
    if (front != "dt-cols-") return "dt-cols-99";
    const colCount = util_dt_prosses_cols_number((Number)(cls.substr(8)) + 1);
    return "dt-cols-" + colCount;
  }

  /**
 * Applies a dt-cols-* class to a container element.
 * 
 * - Removes all existing dt-cols-* classes
 * - Adds the provided class
 * - No-op if element is missing
 * 
 * @param {jQuery} $tr
 * @param {string} cls
 */
  function util_dt_set_table_result_class($tr, cls) {
    if (!$tr || !$tr.length) return;
    $tr.removeClass("dt-cols-1 dt-cols-2 dt-cols-3 dt-cols-4 dt-cols-5 dt-cols-6 dt-cols-7 dt-cols-8 dt-cols-9 dt-cols-10 dt-cols-11 dt-cols-12 dt-cols-13 dt-cols-14 dt-cols-15 dt-cols-99");
    $tr.addClass(cls);
  }

  /**
 * Checks whether the table has horizontal scrolling.
 * 
 * - Inspects `.dataTables_scrollBody`
 * - Returns true if content overflows horizontally
 * 
 * @param {object} api - DataTables API instance
 * @returns {boolean}
 */
  function util_dt_has_hscroll(api) {
    try {
      const $wrap = $(api.table().container());
      const sb = $wrap.find("div.dataTables_scrollBody").get(0);
      if (!sb) return false;
      return (sb.scrollWidth - sb.clientWidth) > 0;
    } catch (e) {
      return false;
    }
  }

  /**
 * Applies responsive column caps to the table container.
 * 
 * - Sets class based on visible column count
 * - Skips specific table types (fullpage, vertDT, matrixTable)
 * - Iteratively relaxes caps if horizontal scroll exists
 * - Helps prevent unnecessary scrollbars
 * 
 * @param {object} api - DataTables API instance
 */
  function util_dt_apply_responsive_caps(api) {
    if (!api) return;

    let $tr = util_dt_find_table_result(api);
    if (!$tr) return;

    // Base class by VISIBLE columns.
    let visCount = 0;
    if ($(api.nodes()).closest("div.fullpage-table").length > 0 ||
      ($(api.nodes()).closest("table.vertDT").length > 0 && !window.is_svp) ||
      ($(api.nodes()).closest("table.matrixTable").length > 0 && !window.is_svp)) {
      visCount = 0
    } else {
      try { visCount = api.columns(":visible").count(); } catch (e) { visCount = 0; }
    }
    let cls = util_dt_get_cols_class(visCount || 99);
    util_dt_set_table_result_class($tr, cls);

    // If we currently have horizontal scroll but there is spare room, relax the cap stepwise.
    // (This prevents "silly" scrollbars for small tables when the container is wide enough.)
    let loops = 0;
    while (util_dt_has_hscroll(api) && cls !== "dt-cols-99" && loops < 5) {
      cls = util_dt_bump_cols_class(cls);
      util_dt_set_table_result_class($tr, cls);
      try { api.columns.adjust(); } catch (e0) { }
      loops++;
    }
  }

  /**
 * Forces layout recalculation for the table.
 * 
 * - Calls columns.adjust()
 * - Updates FixedColumns and FixedHeader if available
 * - Runs twice (immediate + next frame)
 * 
 * @param {object} api - DataTables API instance
 */
  function util_dt_adjust(api) {
    if (!api) return;

    function run() {
      try { api.columns.adjust(); } catch (e) { }
      try {
        if (api.fixedColumns && typeof api.fixedColumns().relayout === "function") {
          api.fixedColumns().relayout();
        }
      } catch (e2) { }
      try {
        if (api.fixedHeader && typeof api.fixedHeader.adjust === "function") {
          api.fixedHeader.adjust();
        }
      } catch (e3) { }
    }

    run();

    // One more pass after FixedHeader/scroll-head DOM has settled
    if (typeof window.requestAnimationFrame === "function") {
      window.requestAnimationFrame(run);
    } else {
      setTimeout(run, 0);
    }
  }

  /**
   * Main orchestration function for DataTable adjustments.
   * 
   * - Creates API instance from settings
   * - Prevents re-entrant loops via flags/tokens
   * - Applies responsive caps and layout adjustments
   * - Re-runs once after rendering settles
   * 
   * @param {object} settings - DataTables settings object
   */
  function util_dt_apply_all(settings) {
    let api;
    try { api = new $.fn.dataTable.Api(settings); } catch (e) { return; }

    // Prevent feedback loops: columns.adjust()/FixedHeader can trigger draw/column-sizing events again.
    if (settings && settings.__dq_dt_apply_busy) return;
    if (settings) settings.__dq_dt_apply_busy = true;

    // Each call gets a monotonically increasing token; async callbacks from older calls are ignored.
    let token = 0;
    if (settings) {
      settings.__dq_dt_apply_token = (settings.__dq_dt_apply_token || 0) + 1;
      token = settings.__dq_dt_apply_token;
    }

    function done() {
      if (!settings) return;
      if (token && settings.__dq_dt_apply_token !== token) return;
      settings.__dq_dt_apply_busy = false;
    }

    // First set the container cap class, then let DT measure inside that box.
    util_dt_apply_responsive_caps(api);
    util_dt_adjust(api);

    // One more pass after layout/scrollbars/fonts have settled.
    if (typeof window.requestAnimationFrame === "function") {
      window.requestAnimationFrame(function () {
        if (settings && token && settings.__dq_dt_apply_token !== token) return;
        util_dt_apply_responsive_caps(api);
        util_dt_adjust(api);
        done();
      });
    } else {
      setTimeout(function () {
        if (settings && token && settings.__dq_dt_apply_token !== token) return;
        util_dt_apply_responsive_caps(api);
        util_dt_adjust(api);
        done();
      }, 0);
    }
  }

  $(document).on("init.dt", function (e, settings) {
    util_dt_apply_all(settings);
  });

  $(document).on("column-visibility.dt draw.dt", function (e, settings) {
    util_dt_apply_all(settings);
  });

  // When the viewport changes (your 3rd screenshot): re-adjust and re-evaluate caps.
  let util_dt_resize_t = null;
  $(window).on("resize.utilDtCaps", function () {
    if (typeof window.scheduleDtRebuild === "function") return;
    if (util_dt_resize_t) window.clearTimeout(util_dt_resize_t);
    util_dt_resize_t = window.setTimeout(function () {
      $(document).find("table.dataTable").each(function () {
        if (!$.fn.dataTable || !$.fn.dataTable.isDataTable(this)) return;
        const api = $(this).DataTable();
        util_dt_adjust(api);
        util_dt_apply_responsive_caps(api);
      });
    }, 80);
  });

}

/**
 * Adds and updates horizontal overflow shadow indicators for a DataTables scroll container.
 *
 * Shows visual hints when table content overflows horizontally and handles special cases such as
 * FixedColumns layouts, ensuring shadows are correctly positioned and updated on scroll/resize.
 *
 * @param {Object} api - DataTables API instance.
 *
 * @returns {void}
 */
function dqUpdateHorizontalOverflowIndicator(api) {

  const $wrap = $(api.table().container());
  const $scrollBody = $wrap.find('.dataTables_scrollBody');
  if (!$scrollBody.length) return;

  // Put the shadow overlay on the scroll container so it doesn't move with scrollLeft.
  const $scroll = $wrap.find('.dataTables_scroll').first();
  if (!$scroll.length) $scroll = $scrollBody.parent();

  // Ensure the overlay elements exist (one per scroll container)
  // - right shadow: indicates more columns to the right
  // - left shadow: indicates columns hidden *under* fixed-left columns
  const $indR = $scroll.children('.dq-xshadow-ind').first();
  if (!$indR.length) {
    $indR = $('<div class="dq-xshadow-ind" aria-hidden="true"></div>');
    $scroll.append($indR);
  }

  const $indL = $scroll.children('.dq-xshadow-left-ind').first();
  if (!$indL.length) {
    $indL = $('<div class="dq-xshadow-left-ind" aria-hidden="true"></div>');
    $scroll.append($indL);
  }

  const el = $scrollBody.get(0);

  function update() {
    // Position/size to match the visible scrollBody viewport.
    try {
      const top = $scrollBody.position().top;
      const h = $scrollBody.outerHeight();
      const left = $scrollBody.position().left;
      const w = $scrollBody.outerWidth();
      const indW = ($indR && $indR.length) ? $indR.outerWidth() : 24;
      // Align shadow with the visible scrollBody viewport (not the full scroll container):
      // keep it pinned to the right edge of what the user can currently see.
      const shadowLeft = (left + w - indW);
      $indR.css({ top: top + 'px', height: h + 'px', left: shadowLeft + 'px', right: 'auto' });
    } catch (e0) { }

    const hasOverflow = el.scrollWidth > el.clientWidth + 2;
    // Only show the right-edge hint when there are columns hidden to the right.
    // (At the far-right end, the hint must disappear.)
    //var hiddenRight = (el.scrollWidth - (el.scrollLeft + el.clientWidth)) > 2;

    // Keep the old guard: only show for FixedColumns tables.
    const $fixedTh = $scroll.find('th.dtfc-fixed-left').first();
    const fixedLeftW = ($fixedTh.length) ? ($fixedTh.outerWidth() || 0) : 0;
    const hasFixedLeft = fixedLeftW > 0 || $scroll.find('td.dtfc-fixed-left').length > 0;

    // Your special case: the fixed-left column occupies (almost) the whole viewport,
    // so the user may think there are no additional columns.
    const onlyFixedVisible = (fixedLeftW > 0) && (el.clientWidth <= fixedLeftW + 4);

    if (hasOverflow && hasFixedLeft && onlyFixedVisible) $scroll.addClass('dq-has-x-shadow-fixedleft');
    else $scroll.removeClass('dq-has-x-shadow-fixedleft');
  }

  // Run now and on scroll/resize
  update();
  $scrollBody.off('scroll.dqXShadow').on('scroll.dqXShadow', update);
  $(window).off('resize.dqXShadow').on('resize.dqXShadow', update);
}

/**
 * Registers a DataTables initialization event listener that applies horizontal overflow
 * shadow indicators to newly created tables.
 *
 * This function listens for the DataTables `init.dt` event, creates a DataTables API
 * instance for the initialized table, and schedules a call to
 * `dqUpdateHorizontalOverflowIndicator` to add/update horizontal scroll shadow indicators.
 *
 * The slight `setTimeout(..., 0)` delay ensures the table is fully rendered in the DOM
 * before measurements are taken and overlay elements are inserted.
 *
 * This is required to support dynamically initialized DataTables instances.
 *
 * @returns {void}
 */
function addDqUpdateHorizontalOverflowIndicatorEventListener() {
  $(document).on('init.dt', function (e, settings) {
    const api = new $.fn.dataTable.Api(settings);
    setTimeout(function () {
      dqUpdateHorizontalOverflowIndicator(api);
    }, 0);
  });
}

//#endregion
