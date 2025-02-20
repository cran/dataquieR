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

  } else {
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
        // var coords = JSON.parse(localStorage.getItem('msgContextMenuCoords')); does not work in FF for file:// because origin is null for file://
        var coords = data.coords;
        // TODO: trigger contextmenu at iframe.offset + coords instead of followCursor
        $(function() {
          $('div[data-nm="' + coords.nm + '"] iframe').parents("div.dataquieR_result")[0]._tippy.setProps({followCursor: true})
          $('div[data-nm="' + coords.nm + '"] iframe').parents("div.dataquieR_result")[0]._tippy.popper.onclick = function() {
            this._tippy.hide();
          }
          $('div[data-nm="' + coords.nm + '"] iframe').parents("div.dataquieR_result")[0]._tippy.show()
        })
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

const isFirefox = navigator.userAgent.toLowerCase().includes('firefox');

if (isFirefox) {
  $($(window).on("load", function() {
    setTimeout(function() {
        $(".dataTables_wrapper").each(function() {
          var wrapper = $(this);
          var id = wrapper.attr("id");
          if (id.match(/_wrapper$/) !== null) {
            var dt = $("#" + id.replaceAll(/_wrapper$/g, "")).DataTable();
            wrapper.find(".dataTables_scrollHead td input").each(function(i) {
              $(this).on( "keyup", function() {
                dt.column(i).search( $(this).val() ).draw();
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
    }, 1000);
  }))
}
