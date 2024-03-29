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
        $($('div[data-nm="' + size.nm + '"] iframe').parents("div.dataquieR_result")[0]).attr("data-initialw", size.w);
        $($('div[data-nm="' + size.nm + '"] iframe').parents("div.dataquieR_result")[0]).attr("data-initialh", size.h);
        $(function() {
          sizeIframes(/* forceSetSizeToInit = */ true)
        })
      } else if (msg = 'toggleHandle') {
        var figureInfo = data.figureInfo;
        var was_enabled = $('div[data-nm="' + figureInfo.nm + '"] iframe').parent().css("resize") == "both";
        if (was_enabled) { // disable
          $('div[data-nm="' + figureInfo.nm + '"] iframe').parent().css({"resize": ""});
          sizeIframes(/* forceSetSizeToInit = */ false)
        } else {
          $('div[data-nm="' + figureInfo.nm + '"] iframe').parent().css({"resize": "both"});
        }
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
