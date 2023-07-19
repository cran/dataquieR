/* https://stackoverflow.com/a/61597966 */
function supportsSelector (selector) {
  const style = document.createElement('style')
  document.head.appendChild(style)
  try {
    style.sheet.insertRule(selector + '{}', 0)
  } catch (e) {
    return false
  } finally {
    document.head.removeChild(style)
  }
  return true
}

if (!SVGElement.prototype.contains) {
  SVGElement.prototype.contains = HTMLDivElement.prototype.contains;
}

function single_page_switch(event) {
  $(function() {
    $( ".navbar a").removeClass("target")
    $( ".navbar a[href='" + window.location.hash + "']").addClass("target")
    if (!supportsSelector(":has(:target)")) { // only FireFox (2023 -- can be removed, once FireFox has it, they guess, in 2023)
      if ($("div.content:has(:target)").length > 0) {
        $(".default-target").hide()
      } else {
        $(".default-target").show()
      }
      $("div.singlePageFinal").hide();
      $("div.singlePageFinal:target").show();
      $("div.singlePageFinal:has(:target)").show();
      if (window.matchMedia('screen and (max-width: 835px)').matches) { // also adjust in CSS at line tagged with !!MEDIA!!
        $(".navbar.responsive").css({
          "float": "",
          "display": "",
          "text-align": ""
        })
        $(".navbar.responsive *").css({
          "float": "",
          "display": "",
          "text-align": ""
        })
        $(".navbar.responsive div:has(*.target)").css({
          "float": "none",
          "display": "block",
          "text-align": "left"
        })
        $(".navbar.responsive div:has(*.target) *").css({
          "float": "none",
          "display": "block",
          "text-align": "left"
        })
      }
      $(function() {
        try {
          document.getElementById(location.hash.replace("#", "")).scrollIntoView();
        } catch(e) {

        }
      })
    }
    window.setTimeout(resizeAll, 20)
  })
  event.stopPropagation()
}

$(function() {
  /* Activate the hidden single pages, if hidden from the beginning, some wisgets cannot correctly be set up (namely datatables) */
  $(".singlePage").addClass("singlePageFinal")
  $(".singlePage").removeClass("singlePage")
  try {
    if (window.location.hash == "")
      window.location.hash = $(".singlePageFinal")[0].id
  } catch (e) {
  }
  $("[title]").each((i, node) => {
    if (node.title.trim() == "") {
      node.removeAttribute("title");
    }
  })
  var tippyInstances = [];
  tippy('[title]', { // https://atomiks.github.io/tippyjs/v6/all-props/
    content(reference) { // https://atomiks.github.io/tippyjs/v5/faq/#can-i-use-the-attribute
      tippyInstances = tippyInstances.concat(reference); // https://github.com/atomiks/tippyjs/issues/473#issuecomment-485055710
      const title = reference.getAttribute('title');
      reference.removeAttribute('title');
      return title;
    },
    allowHTML: true,
    followCursor: 'initial',
    hideOnClick: true,// FIXME: position is negative for huge pages.
    delay: 1000,
    trigger: "mouseenter",
    onShow(instance) {
      setTimeout(() => {
        instance.hide();
      }, 120000);
    }
  });
  if ($("#_enabletippy").length == 1) {
    var handleEnableTippy = function() {
      tippyInstances.forEach(reference => {
        var this_tippy = reference._tippy;
        reference = $(reference)
        if (reference.find('.plotly').length > 0) {
          if ($("#_enabletippy").is(':checked')) {
            this_tippy.enable()
          } else {
            this_tippy.hide()
            this_tippy.disable()
          }
        }

      } )

    }
    $("#_enabletippy").click(handleEnableTippy)
    handleEnableTippy()
  }
  $(window).scroll(float_menus);
  hideMatrices()
  $(float_menus)
})

/*.prliminary hide matrices which are too technical */
function hideMatrices() {
  hideMatrix("ermat")
  hideMatrix("anamat")
}
function hideMatrix(id) {
  $("#" + id).prev("h2").nextUntil("h2").css("display", "none")
  $("#" + id).prev("h2").css("display", "none")
  $("a[href='report.html#"+ id + "']").css("display", "none")
}
function showMatrices() {
  showMatrix("ermat")
  showMatrix("anamat")
}
function showMatrix(id) {
  $("#" + id).prev("h2").nextUntil("h2").css("display", "")
  $("#" + id).prev("h2").css("display", "")
  $("a[href='report.html#"+ id + "']").css("display", "")
}

/* Toggle between adding and removing the "responsive" class to topnav when the user clicks on the icon */
function toggleTopNav(event) {
  $(".navbar").first().toggleClass("responsive")
  event.stopPropagation()
}

function hideTopNav(event) {
  if (!(event.target.tagName.toLowerCase() === 'a')) {
    $(".navbar").first().removeClass("responsive")
    event.stopPropagation()
  }
}

/* https://stackoverflow.com/a/34064434 */
function htmlDecode(input) {
  var doc = new DOMParser().parseFromString(input, "text/html");
  return doc.documentElement.textContent;
}

/* Show descriptoin of a main menu entry */
function showDescription(id, description) {
  if ($("div#desc_" + id).length == 0) {
    var description = htmlDecode(description);

    var dv = jQuery('<div>', {
      id: "desc_" + id,
      class: 'singlePageFinal'
//      title: 'now this div has a title!'
    });
    dv.html(description);
    dv.appendTo('.content');
  }
  if (window.matchMedia('screen and (max-width: 835px)').matches) { // also adjust in CSS at line tagged with !!MEDIA!!
    if (window.location.hash == "#" + id) {
      window.location.hash = "" ;
    } else {
      window.location.hash = "#" + id;
    }
  } else {
    window.location.hash = "#" + "desc_" + id;
  }
}

/*
https://stackoverflow.com/a/5316785
add a floating menu to the single page's divs
*/
function float_menus() {
  if ($(window).scrollTop() >= 20 - $(".navbar").height()) {
      $(".floatbar").css({position:'fixed',right:'0px',top: $(".navbar").height() + 'px'});
  } else {
      $(".floatbar").css({position:'absolute',right:'0px',top: ($(".navbar").height() + 20) + 'px'});
  }
}

function context_menu(event) {
  event.preventDefault();
  var tgt = event.currentTarget;
  var context_menu_el = tgt._tippy;

  context_menu_el.setProps({
    getReferenceClientRect: () => ({
      width: 0,
      height: 0,
      top: event.clientY,
      bottom: event.clientY,
      left: event.clientX,
      right: event.clientX,
    }),
  });

  context_menu_el.show();
  /*
  call for report somewhere
  <button class="btn" data-clipboard-text="Just because you can doesn't mean you should — clipboard.js">
      Copy to clipboard
  </button>
  x = new ClipboardJS('.btn', {
      text: function(trigger) {
          return trigger.getAttribute('aria-label');
      }
  });
  x.destroy()
  */
}

const min_pixels_per_tick = 40; // TODO: Depends on the kind of the plot
const min_width_in_em = 30;
const min_height_in_em = 15;
const def_height_in_vh = 80;

function resizeAll() {
  var div = $('<div style="width: 1em;"></div>').appendTo('body');
  var em = div.width();
  div.remove();
  $(document).find(".js-plotly-plot").each(function(i, py) {
    var cont = $(py).parent();
    cont.css("overflow", "auto")
    var x = JSON.parse(Plotly.Plots.graphJson(py)).layout.xaxis.tickvals.length;
    var y = JSON.parse(Plotly.Plots.graphJson(py)).layout.yaxis.tickvals.length;
    if (JSON.parse(Plotly.Plots.graphJson(py)).layout.hasOwnProperty("xaxis2")) {
      var x2 = JSON.parse(Plotly.Plots.graphJson(py)).layout.xaxis2.tickvals.length;
      x <- x + x2;
    }
    if (JSON.parse(Plotly.Plots.graphJson(py)).layout.hasOwnProperty("yaxis2")) {
      var y2 = JSON.parse(Plotly.Plots.graphJson(py)).layout.yaxis2.tickvals.length;
      y <- y + y2;
    }
    var h = def_height_in_vh * window.innerHeight / 100 - 45 ; // 45 for the scrollbar
    var w = cont.width() - 45; // for the scrollbar
    if (min_pixels_per_tick * x > w) {
      w = min_pixels_per_tick * x;
    }
    if (min_width_in_em*em > w) {
      w = min_width_in_em*em;
    }
    var h = Math.max(h, cont.height() - 45); // for the scrollbar
    if (min_pixels_per_tick * y > h) {
      h = min_pixels_per_tick * y;
    }
    if (min_height_in_em*em > h) {
      h = min_height_in_em * em;
    }
    $(py).width(w);
    $(py).height(h);
    var max_xlab = Math.max.apply(Math, JSON.parse(Plotly.Plots.graphJson(py)).layout.xaxis.ticktext.map(x => x.length))
    var max_ylab = Math.max.apply(Math, JSON.parse(Plotly.Plots.graphJson(py)).layout.yaxis.ticktext.map(x => x.length))
    if (JSON.parse(Plotly.Plots.graphJson(py)).layout.hasOwnProperty("xaxis2")) {
      var max_xlab2 = Math.max.apply(Math, JSON.parse(Plotly.Plots.graphJson(py)).layout.xaxis2.ticktext.map(x => x.length))
      max_xlab = Math.max(max_xlab, max_xlab2)
    }
    if (JSON.parse(Plotly.Plots.graphJson(py)).layout.hasOwnProperty("yaxis2")) {
      var max_ylab2 = Math.max.apply(Math, JSON.parse(Plotly.Plots.graphJson(py)).layout.yaxis2.ticktext.map(x => x.length))
      max_ylab = Math.max(max_ylab, max_ylab2)
    }
    var update = py.layout;
    update.xaxis.tickangle = "auto";
    update.width = Math.min(w, window.innerWidth - 45);
    update.height = Math.min(h, window.innerHeight - 45);
    update.autosize =  false;
    // update.margin.l = 0.5 * em * max_ylab; // rotation is now auto, so have space for full label
    update.margin.l = em * max_ylab;
    // update.margin.b = 0.3 * em * max_xlab; // rotation is now auto, so have space for full label
    update.margin.b = em * max_xlab;
    update.margin.autoexpand = false;
    if (update.width * 0.2 < update.margin.l) {
      update.margin.l = update.width * 0.2;
    }
    update.margin.l = Math.max(update.margin.l, 1.2 * em)
    if (update.height * 0.2 < update.margin.b) {
      update.margin.b = update.height * 0.2;
    }
    update.margin.b = Math.max(update.margin.b, 1.2 * em)
    try {
      Plotly.relayout(py, update); // this may crash
    } catch(e) {
      console.log(e);
    }
/*    window.setTimeout(function() { // work around error from this may crash
    // see also https://community.plotly.com/t/cant-show-heatmap-inside-div-error-something-went-wrong-with-axis-scaling/30616/2
      // py.querySelector('a[data-attr="zoom"][data-val="out"]').click()
      py.querySelector('a[data-attr="zoom"][data-val="reset"]').click()
//      window.setTimeout(function() {
//        py.querySelector('a[data-attr="zoom"][data-val="in"]').click()
//      }, 500)
    }, 500)*/
  })
}

$(function() {
  var resized = true;
  $(window).on("resize", function() {
    resized = true;
  });
  window.setInterval(function() {
    try {
      if (resized) {
        resizeAll();
      }
    } finally {
      resized = false;
    }
  }, 100);
})

function resize(resultId) {
  var cont = $(document.getElementById(resultId)).next()
  var py = cont.find(".js-plotly-plot")[0]
  var update = {
    width: $(py).parent().width(),
    height: $(py).parent().height()
  };
  Plotly.relayout(py, update);
}

function dlResult(event) {
  try {
    var py = $(currentContextMenu.reference).find(".js-plotly-plot")[0]
    var ppy = $(currentContextMenu.reference).find(".js-plotly-plot")
    var id = null
    try {
      id = ppy.parent("div.dataquieR_result").prevAll("a[id]")[0].id
    } catch (e) {
      console.log(e)
    }
    if (id == null) {
      try {
        id = ppy.parent("div.dataquieR_result").parent("div[id]")[0].id
      } catch (e) {
        console.log(e)
      }
    }
    Plotly.toImage(py, {format: 'svg'}).then(function(url){download(id + ".svg", url)})
  } catch(e) {
    console.log(e)
  }
  try {
    $(currentContextMenu.reference).find("div.dt-buttons button.buttons-excel").click()
  } catch(e) {
    console.log(e)
  }
  try {
    var id = $(currentContextMenu.reference).find("img").parent().prevAll("a[id]")[0].id
    download(id + ".png", $(currentContextMenu.reference).find("img")[0].src)
  } catch (e) {
    console.log(e)
  }
}

currentContextMenu = null;

$(function() {
  $('div.dataquieR_result').each(function() {
    var stderr = this.getAttribute("data-stderr").trim();
    if (stderr != "") {
        var btn1 = $('<input />', {
                type: 'button',
                value: '\u26A0',
                style: 'background-color: white; color: lightgrey; border: 0;',
                'data-stderr': stderr,
                on: {
                   mouseenter: function() {
                     showTip(this.getAttribute("data-stderr").trim());
                   },
                   mouseleave: function() {
                     messenger.hide()
                   }
                }
        });
        var btn2 = $('<input />', {
                type: 'button',
                value: '\u26A0',
                style: 'background-color: white; color: lightgrey; border: 0;',
                'data-stderr': stderr,
                on: {
                   mouseenter: function() {
                     showTip(this.getAttribute("data-stderr").trim());
                   },
                   mouseleave: function() {
                     messenger.hide()
                   }
                }
        });
        $(this).prepend(btn1);
        $(this).prepend("<br />");
        // $(this).append(btn2);
        $(this).append("<br />");
    }
  });
  tippy('div.dataquieR_result', {
    content(reference) {
      const call = $(reference).attr("data-call");
      const l = 30;
      const call_link = $("<button/>", {
        text: "Copy R-Call: " + (call.length > l + 3? call.substr(0, l) + "..." : call),
        "data-clipboard-text": call,
        "class": "context-menu"
      });
      const save_link = $("<button/>", {
        text: "Save to disk",
        onclick: "dlResult(event)",
        "class": "context-menu"
      });
      save_link.reference = reference;
      const call_li = $("<li />")
      call_li.append(call_link);
      const save_li = $("<li />")
      save_li.append(save_link);
      const ul = $("<ul />");
      ul.append(call_li);
      ul.append(save_li);
      return ul[0];
//      return "<ul>" +
//        "<li class=\"context-menu\">" + call_link[0].outerHTML + "</li>" +
//        "<li class=\"context-menu\">" + save_link[0].outerHTML + "</li>" +
//        "</ul>";
    },
    onShown(instance) {
      currentContextMenu = instance;
      var stderr =
        currentContextMenu.reference.getAttribute("data-stderr").trim()
      if (stderr != "") {
        // showTip(stderr)
      }
      instance._clipboard = new ClipboardJS($(instance.popper).find("button")[0])
      instance._clipboard.on('success', function(e) {
        showTip("Call Copied to Clipboard.");
        instance.hide(500);
      })
      instance._clipboard.on('error', function(e) {
        showTip("Error Copying Call to Clipboard.");
        console.log(e),
        instance.hide(500);
      })
    },
    onHide(instance) {//https://clipboardjs.com/ https://atomiks.github.io/tippyjs/v5/lifecycle-hooks/ https://clipboardjs.com/ https://atomiks.github.io/tippyjs/v6/html-content/ https://atomiks.github.io/tippyjs/v6/misc/
      try {
        currentContextMenu = instance;
      } catch (e) {
        console.log(e);
      }
      try {
        instance._clipboard.destroy();
      } catch (e) {
        console.log(e);
      }
      return true; // dont cancel it
    },
    allowHTML: true,
    placement: 'right-start',
    trigger: 'manual',
    interactive: true,
    arrow: false,
    offset: [0, 0],
  }); // TODO: fix race condition
  $("div.dataquieR_result").contextmenu(context_menu);
})

var messenger = null;

$(function() {
  messenger = tippy('body', {
    arrow: true,
    hideOnClick: true,
    content: '<div id="messenger-msg">xxx</div>',
    allowHTML: true,
    followCursor: true,
    interactive: true,
    trigger: 'manual',
  })[0];
  var clip_inst = new ClipboardJS('.clipbtn');
  clip_inst.on('success', function(e) {
    showTip("Text Copied to Clipboard.");
  })
  clip_inst.on('error', function(e) {
    showTip("Error Copying T  ext to Clipboard.");
    console.log(e);
  })

});

function showTip(msg, delay = 10000) {
  if (messenger == null) {
    console.log("showTip called too early.");
    return;
  }
  messenger.show();
  $("#messenger-msg").html(msg)
  window.setTimeout(messenger.hide, delay);
}

// https://stackoverflow.com/a/34579496
function readTextFile(file, callback) {
    var rawFile = new XMLHttpRequest();
    rawFile.overrideMimeType("application/json");
    rawFile.open("GET", file, true);
    rawFile.onreadystatechange = function() {
        if (rawFile.readyState === 4 && rawFile.status == "200") {
            callback(rawFile.responseText);
        }
    }
    rawFile.send(null);
}

// https://stackoverflow.com/a/18197341
function download(filename, text) {
  var element = document.createElement('a');
//  element.setAttribute('href', 'data:text/plain;charset=utf-8,' + encodeURIComponent(text));
  element.setAttribute('href', text);
  element.setAttribute('download', filename);

  element.style.display = 'none';
  document.body.appendChild(element);

  element.click();

  document.body.removeChild(element);
}


// TODO: About with citation
