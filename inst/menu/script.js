window.addEventListener("pageshow", function (evt) {
  if (evt.persisted) {
    location.reload();  // reload, if loaded from BF-Cache.
  }
});

document.querySelector(':root').style.setProperty('--screen-width', window.screen.width);

document.querySelector(':root').style.setProperty('--screen-height', window.screen.height);

$(function() {
  // Set up communication with iframes
  // var dataquieRChannel = new BroadcastChannel('dataquieRChannel'); does not work in FF for file:// because origin is null for file://

  const originToSend =
    (window.location.protocol === "file:") ? "*" : window.location.origin;

  if (window.in_iframe) {
    window.sendMsgContextMenu = function(coords) {
      window.parent.postMessage({
          message: 'contextMenu',
          data: {
            coords: coords
          }
        }, origin = originToSend); // but no relevant disclosure

      // does not work in FF for file:// because origin is null for file://
//      localStorage.setItem('msgContextMenuCoords', JSON.stringify(coords));
//      dataquieRChannel.postMessage('contextMenu');
    }

    window.sendMsgToggleHandle = function(figureInfo) {
      window.parent.postMessage({
          message: 'toggleHandle',
          data: {
            figureInfo: figureInfo
          }
        }, origin = originToSend); // but no relevant disclosure

      // does not work in FF for file:// because origin is null for file://
//      localStorage.setItem('msgContextMenuCoords', JSON.stringify(coords));
//      dataquieRChannel.postMessage('contextMenu');
    }

    window.sendMsgInitialSizeEstimate = function(size) {
      window.parent.postMessage({
          message: 'initialSizeEstimate',
          data: {
            size: size
          }
        }, origin = originToSend); // but no relevant disclosure
    }

    window.sendMsgDismissContextMenu = function () {
      window.parent.postMessage({
        message: "dismissContextMenu" ,
        data: {}
      }, origin = originToSend);
    };

  } else {
    window.sendMsgDismissContextMenu = function () { // already in top-level
      dismissContextMenu(); // no need to send, just dismiss it
    };


    window.sendMsgDownloadSVG = function(nm) {
      var target = $('div[data-nm="' + nm + '"] iframe')[0]
      target.contentWindow.postMessage({
          message: 'downloadSVG',
          data: {
            nm: nm
          }
        }, origin = originToSend); // but no relevant disclosure

      // does not work in FF for file:// because origin is null for file://
//      localStorage.setItem('msgDownloadSVGNm', JSON.stringify(nm));
//      dataquieRChannel.postMessage('downloadSVG');
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


    var msg = event.data.message; // not for datauqieRChannel solution with BroadcastChannel
    var data = event.data.data; // not for datauqieRChannel solution with BroadcastChannel
    if (window.in_iframe) {
      // if (messageEvent.data === 'downloadSVG') { does not work in FF for file:// because origin is null for file://
      if (msg == 'downloadSVG') {
        // var nm = JSON.parse(localStorage.getItem('msgDownloadSVGNm'));  does not work in FF for file:// because origin is null for file://
        var nm = data.nm;
        if ($("#nm").attr("data-nm") == nm) {
          dlResult(nm);
        }
      }
    } else {
      // if (messageEvent.data === 'contextMenu') { does not work in FF for file:// because origin is null for file://
      if (msg === 'contextMenu') {
        $(function() {
          // var coords = JSON.parse(localStorage.getItem('msgContextMenuCoords')); does not work in FF for file:// because origin is null for file://
          var coords = data.coords;
          var $iframe = $('div[data-nm="' + coords.nm + '"] iframe');
          var iframeRect = $iframe[0].getBoundingClientRect();
          var resultContainer = $iframe.parents("div.dataquieR_result")[0];
          var absX = coords.x + iframeRect.left;
          var absY = coords.y + iframeRect.top;

          resultContainer._tippy.setProps({
            followCursor: false,
            getReferenceClientRect: function() {
              return {
                top: absY,
                left: absX,
                bottom: absY,
                right: absX,
                width: 0,
                height: 0,
                x: absX,
                y: absY,
                toJSON: function () {} // required by Popper
              };
            }
          });
          resultContainer._tippy.popper.onclick = function() {
            this._tippy.hide();
          }
          resultContainer._tippy.show()
        })
      } else if (msg === 'dismissContextMenu') {
        dismissContextMenu();
      } else if (msg == 'initialSizeEstimate') {
        var size = data.size;
        $($('div[data-nm="' + size.nm + '"] iframe').parents("div.scaler")[0]).attr("data-initialw", size.w);
        $($('div[data-nm="' + size.nm + '"] iframe').parents("div.scaler")[0]).attr("data-initialh", size.h);
        $(function() {
          sizeIframes(/* forceSetSizeToInit = */ true)
        })
      } else if (msg == 'toggleHandle') {
        var figureInfo = data.figureInfo;
        var scaler_div = $('div[data-nm="' + figureInfo.nm + '"] iframe').parent();
        const ih = $(scaler_div).attr("data-initialh").replace(/;+$/g, '');
        const iw = $(scaler_div).attr("data-initialw").replace(/;+$/g, '');
        $(scaler_div).css({"width": iw})
        $(scaler_div).css({"height": ih})
        $(scaler_div).height($(scaler_div).height()) // trigger resize events for potly

//        var was_enabled = $('div[data-nm="' + figureInfo.nm + '"] iframe').parent().css("resize") == "both";
//        if (was_enabled) { // disable
//          $('div[data-nm="' + figureInfo.nm + '"] iframe').parent().css({"resize": ""});
//          sizeIframes(/* forceSetSizeToInit = */ false)
//        } else {
//          $('div[data-nm="' + figureInfo.nm + '"] iframe').parent().css({"resize": "both"});
//        }
      }
    }
  }
})


// https://stackoverflow.com/a/18197341
function download(filename, text) {
  var element = document.createElement('a');
  element.setAttribute('href', text);
  element.setAttribute('download', filename);

  element.style.display = 'none';
  document.body.appendChild(element);

  element.click();

  document.body.removeChild(element);
}



// https://stackoverflow.com/a/10753619
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
        Yes: function() {
          $(this).dialog("close");
          yes()
        },
        No: function() {
          $(this).dialog("close");
          no()
        }
      },
      close: function(event, ui) {
        $(this).remove();
      }
    });
};

function PlotlyIconshomeRED() {
  // https://stackoverflow.com/a/5364657
//  var res = jQuery.extend(true, {}, Plotly.Icons.home);
//  res.style = "fill:#800100";

  var res = {
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

function PlotlyIconsprinter() {
  var res = {
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

downloadPlotlyAsPDF = function(gd, options = {}) {
  // jsPDF aus globalen Namespace holen (1.3.2 UND 2.x unterstützen)
  const jsPDF = window.jsPDF || (window.jspdf && window.jspdf.jsPDF);
  if (!jsPDF) {
    console.error("jsPDF not found on window");
    alert("PDF export is not available (jsPDF not loaded). dataquieR needs the package visNetwork for this feature.");
    return;
  }

  const orientation = options.orientation || "landscape";
  const dpi = options.dpi || 300;

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
  if (!filename.toLowerCase().endsWith(".pdf")) {
    filename += ".pdf";
  }

  // A4 in Inch
  const a4in = orientation === "landscape" ? [11.69, 8.27] : [8.27, 11.69];
  const widthPx  = Math.round(a4in[0] * dpi);
  const heightPx = Math.round(a4in[1] * dpi);
  const widthPt  = a4in[0] * 72;
  const heightPt = a4in[1] * 72;

  // Optional: upscale fonts just for export
  const enlargedLayout = JSON.parse(JSON.stringify(gd.layout || {}));
  enlargedLayout.font = enlargedLayout.font || {};
  enlargedLayout.font.size = (enlargedLayout.font.size || 12) * 1.5;

  Plotly.toImage({
    data: gd.data,
    layout: enlargedLayout,
    config: gd._fullConfig,
    format: "png",
    width: widthPx,
    height: heightPx,
    scale: 1
  }).then(function(dataUrl) {
    if (!dataUrl || dataUrl === "data:,") {
      console.error("Empty image generated.");
      alert("PDF export failed.");
      return;
    }

    // jsPDF 1.3.2: typischer Aufruf ist (orientation, unit, format)
    // orientation: 'p' oder 'l'
    const pdfOrientation = orientation === "landscape" ? "l" : "p";
    const pdf = new jsPDF(pdfOrientation, "pt", [widthPt, heightPt]);

    const margin = 20;
    pdf.addImage(
      dataUrl,
      "PNG",
      margin,
      margin,
      widthPt - 2 * margin,
      heightPt - 2 * margin
    );
    pdf.save(filename);
  }).catch(function(err) {
    console.error("Plotly.toImage failed:", err);
    alert("Could not export to PDF.");
  });
};




function PlotlyIconspdf() {
  return {
    name: 'pdf',
    svg: [
      '<svg width="24" height="24" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg" fill="none">',
        '<rect x="3" y="3" width="18" height="18" rx="2" fill="#E0E0E0" stroke="#333" stroke-width="1.5"/>',
        '<path d="M7 9h2v6H7zM11 9h1.5a1 1 0 010 2H11v4h-1.5V9H11zM15 9h3v1.5h-1.5v1h1.3v1.5h-1.3v2h-1.5V9z" fill="#333"/>',
      '</svg>'
    ].join('')
  };
}

const escCloseStack = [];

function pushEscCloseHandler(fn) {
  escCloseStack.push(fn);
}

function popEscCloseHandler(fn) {
  const i = escCloseStack.lastIndexOf(fn);
  if (i >= 0) escCloseStack.splice(i, 1);
}

document.addEventListener('keydown', function(event) {
  if (event.key === 'Escape' && escCloseStack.length > 0) {
    const fn = escCloseStack.pop();
    if (typeof fn === 'function') fn();
  }
});

 function saveFullImageWithoutLosingView(gd, plotlydl_args) {
    const currentView = {
      'xaxis.range': [...gd.layout.xaxis.range],
      'yaxis.range': [...gd.layout.yaxis.range]
    };

    Plotly.relayout(gd, {
      'xaxis.autorange': true,
      'yaxis.autorange': true
    }).then(() => {
      return Plotly.downloadImage(gd, plotlydl_args);
    }).then(() => {
      return Plotly.relayout(gd, currentView);
    }).catch((err) => {
      console.error(err);
    });
 }

function plotlyDL(gd) {
  if (!gd.id) {
    gd.id = 'plotly_' + Math.random().toString(36).substr(2, 9);
  }
  var nm = $("#nm");
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

  formats.forEach(function(fmt) {
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
    label.onclick = function() {
      Plotly.downloadImage(gd, dlArgs);
      document.body.removeChild(menu);
    };

    const more = document.createElement('button');
    more.innerText = '\u2026';
    more.title = 'Advanced Options';
    more.style.marginLeft = '8px';
    more.onclick = function(e) {
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
    const clickAwayHandler = function(event) {
      if (!menu.contains(event.target)) {
        if (document.body.contains(menu)) {
          document.body.removeChild(menu);
        }
        document.removeEventListener('click', clickAwayHandler);
        popEscCloseHandler(escMenuHandler);
      }
    };
    document.addEventListener('click', clickAwayHandler);

    const escMenuHandler = function() {
      if (document.body.contains(menu)) {
        document.body.removeChild(menu);
      }
      document.removeEventListener('click', clickAwayHandler);
      popEscCloseHandler(escMenuHandler);
    };
    pushEscCloseHandler(escMenuHandler);
  }, 0);

  const bbox = gd._fullLayout._modeBar.element.getBoundingClientRect();
  menu.style.left = (bbox.left + 40) + 'px';
  menu.style.top = (bbox.top + 40) + 'px';
}

function ucfirst(str) {
  return str.charAt(0).toUpperCase() + str.slice(1);
}

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

  setTimeout(function() {
    const clickAwayHandler = function(event) {
      if (!dialog.contains(event.target)) {
        if (document.body.contains(dialog)) {
          document.body.removeChild(dialog);
        }
        document.removeEventListener('click', clickAwayHandler);
        popEscCloseHandler(escDialogHandler);
      }
    };
    document.addEventListener('click', clickAwayHandler);

    const escDialogHandler = function() {
      if (document.body.contains(dialog)) {
        document.body.removeChild(dialog);
      }
      document.removeEventListener('click', clickAwayHandler);
      popEscCloseHandler(escDialogHandler);
    };
    pushEscCloseHandler(escDialogHandler);
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

function plotlyPrint(gd) {
  // Inject the dialog if it hasn't been added yet
  if (typeof injectPlotlyPrintDialog === "function") {
    injectPlotlyPrintDialog();
  }

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

const isFirefox = navigator.userAgent.toLowerCase().includes('firefox');

if (true || isFirefox) {
  $($(window).on("load", function() {
    setTimeout(function() {
        $(".dataTables_wrapper").each(function() {
          var wrapper = $(this);
          var id = wrapper.attr("id");
          if (id.match(/_wrapper$/) !== null) {
            var dt = $("#" + id.replaceAll(/_wrapper$/g, "")).DataTable();
            var cidx = computeColIndex(dt);
            wrapper.find(".dataTables_scrollHead td input").each(function(i) {
              $(this).on( "keyup", function() {
                dt.column(cidx.dom2data[i]).search( $(this).val() ).draw();
              })
            })
          }
        })
/*          $(".dataTables_wrapper .dataTables_scrollHead td input").each(function() {
          var wrapper = $(this.closest(".dataTables_wrapper"));
          var id = wrapper.attr("id");
          if (id.match(/_wrapper$/) !== null) {
            var dt = $("#" + id.replaceAll(/_wrapper$/g, "")).DataTable();
            this.on( "keydown", function() {
              debugger
              dt.column(0).search( "" ).draw();
            }
          }
        })*/
      // $("#DataTables_Table_0_wrapper .dataTables_scrollHead td input")
      // $("#DataTables_Table_0").DataTable().column(0).search( "" ).draw();

      // disable right mousebutton on plotlies:
      var plots = document.querySelectorAll(".js-plotly-plot");

      plots.forEach(function (plotEl) {
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
      });

    }, 1000);
  }))
}

document.addEventListener("click", function (e) {
  const link = e.target.closest("a");
  if (!link || !link.href) return;

  const currentUrl = new URL(window.location.href);
  const targetUrl = new URL(link.href, currentUrl);

  const isFileProtocol = currentUrl.protocol === "file:";

  let shouldCheck = false;

  if (isFileProtocol) {
    // Beim file:// prüfen wir, ob target im gleichen Verzeichnis oder darunter liegt
    const currentDir = currentUrl.pathname.replace(/[^/]+$/, "");
    shouldCheck = targetUrl.pathname.startsWith(currentDir);
  } else {
    // Bei http(s) prüfen wir auf gleichen Host + Pfad-Präfix
    const isSameHost = targetUrl.origin === currentUrl.origin;
    const currentDir = currentUrl.pathname.replace(/[^/]+$/, "");
    const isSameDir = targetUrl.pathname.startsWith(currentDir);
    shouldCheck = isSameHost && isSameDir;
  }

  if (shouldCheck) {
    const baseName = targetUrl.pathname.split("/").pop() + targetUrl.hash;

    if (!all_ids.all_ids.includes(baseName)) {
      e.preventDefault();
      alert("The requested output does not exist: " + baseName + ". Sorry, this is a bug, please report.");
    }
  }
});
