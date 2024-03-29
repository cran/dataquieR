window.in_iframe = false;

 // remove all info about initialized iframe divs, if such exist
 // reset initialSizeReported
 // used in script_ifrmae.js, look for "reset initialSizeReported" in that file
for (let i = 0; i < sessionStorage.length; i++) {
  var key = sessionStorage.key(i);
  if (key.startsWith("initialSizeReported.")) {
    sessionStorage.removeItem(key);
  }
}

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

function handle_bread_crumbs() {
  $(".breadcrumb").remove()
  if ($("a.target").parent().parent().find("p").html() !== undefined) {
    // on a "real page""
    var breadcrumb0 = $('<div/>', {
        class: "breadcrumb",
        html: $('<a/>', {
            html: document.title,
            href: "report.html"
        })
    })
    breadcrumb0.appendTo('div.navbar');
    var breadcrumb05 = $("<span/>", { html: ">", style: "padding-left: 0.5em; padding-right: 0.5em" })
    var breadcrumb1 = $('<div/>', {
        class: "breadcrumb",
        html: $('<a/>', {
            html: $("a.target").parent().parent().find("p").html(),
            onclick: $("a.target").parent().parent().attr("onclick"),
            style: "cursor: pointer;"
        })
    })
    var breadcrumb2 = $("<span/>", { html: ">", style: "padding-left: 0.5em; padding-right: 0.5em" })
    var breadcrumb3 = $("<a/>", {
          href: $("a.target").attr("href"),
          html: $("a.target").html()
    })
    breadcrumb05.appendTo(breadcrumb0);
    breadcrumb1.appendTo(breadcrumb0);
    breadcrumb2.appendTo(breadcrumb0);
    breadcrumb3.appendTo(breadcrumb0);
  } else {
    // maybe a description page from the concept database
    if (window.location.hash.startsWith("#desc_")) {
      var pattern = "^#desc_";
      var re = new RegExp(pattern);
      var id = window.location.hash.replace(re, "");
      var breadcrumb0 = $('<div/>', {
          class: "breadcrumb",
          html: $('<a/>', {
              html: document.title,
              href: "report.html"
          })
      })
      breadcrumb0.appendTo('div.navbar');
      var breadcrumb05 = $("<span/>", { html: ">", style: "padding-left: 0.5em; padding-right: 0.5em" })
      var breadcrumb1 = $('<div/>', {
          class: "breadcrumb",
          html: $('<a/>', {
              html: $("#" + id).find("p").html(),
              onclick: $("#" + id).attr("onclick"),
              style: "cursor: pointer;"
          })
      })
      breadcrumb05.appendTo(breadcrumb0);
      breadcrumb1.appendTo(breadcrumb0);
    }
  }
}

function single_page_switch(event) {
  $(function() {
    if (window.location.hash.startsWith("#desc_")) {
      if ($(".default-target").is(":visible")) {
          var pattern = "^#desc_";
          var re = new RegExp(pattern);
          var id = window.location.hash.replace(re, "");
          $("div#desc_" + id).remove();
          $("#" + id).trigger("click");
          window.location = window.location.hash;
      }
    }
    $( ".navbar a").removeClass("target");
    // $( ".navbar a[href='" + window.location.hash + "']").addClass("target")
    $( ".navbar a[href='" + $("dataquier-data:visible").attr("curr-url") + "']").addClass("target");

    $(function() {
    if (!supportsSelector(":has(:target)")) { // only FireFox (2023 -- can be removed, once FireFox has it, they guess, in 2023)
        $(".navbar div button.dropbtn").parent().removeClass("highlight-in-ff");
        $(".navbar div:has(*.target) button.dropbtn").parent().addClass("highlight-in-ff");
        if ($("div.content:has(:target)").length > 0) {
          $(".default-target").hide()
        } else {
          $(".default-target").show()
        }
        $("div.singlePageFinal").hide();
        $("div.singlePageFinal:target").show();
        $("div.singlePageFinal:has(:target)").show();
        $( ".navbar a[href='" + $("dataquier-data:visible").attr("curr-url") + "']").addClass("target");
        if (window.matchMedia('screen and (max-width: 1082px)').matches) { // also adjust in CSS at line tagged with !!MEDIA!!
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
      }

      $(handle_bread_crumbs)

      window.setTimeout(function() {
        sizeIframes(/* forceSetSizeToInit = */ false)
      }, 20)
      window.setTimeout(function() {
        try {
          $('html, body').animate({scrollTop: $(document.getElementById(decodeURI(location.hash.replace("#", "")))).offset().top-$(".navbar").height()})
        } catch(e) {

        }
      }, 40)
    })
  })
  event.stopPropagation()
}

$(function() {

  $("div.singlePageFinal").css("padding-top", $(".navbar").height() + "px");
  $("div.content").css("margin-top", $(".navbar").height() + "px");

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
          var always_on = "true" === reference.attr("data-tippy-always-on")
          if (always_on || $("#_enabletippy").is(':checked')) {
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
//  hideMatrices()
  fixTDforMozilla()
  $(float_menus)
})

/*.prliminary hide matrices which are too technical -- superseeded by new summaries
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
*/

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
  if (window.matchMedia('screen and (max-width: 1082px)').matches) { // also adjust in CSS at line tagged with !!MEDIA!!
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
const max_pixels_per_tick = 100; // TODO: Depends on the kind of the plot
const min_width_in_em = 30;
const min_height_in_em = 15;
const def_height_in_vh = 50;
const def_width_in_vh = 70;
const default_plotlyScreenFit = false;

function togglePlotlyWindowZoom(gd) { // FIXME: !! AND also FIXME: Make cursor positon nicer. FIXME: class_ReportSummaryTable test fails; FIXME: Merge and fix padding of Elena's stuff
  if (!gd.hasOwnProperty("plotlyScreenFit")) {
    gd.plotlyScreenFit = default_plotlyScreenFit;
    if (window.hasOwnProperty("sessionStorage")) {
      var plotlyScreenFit = JSON.parse(window.sessionStorage.getItem(
                                                "plotlyScreenFit." + gd.id));
      if (plotlyScreenFit != null) {
        gd.plotlyScreenFit = plotlyScreenFit ;
      }
    }
  }
  if (!gd.plotlyScreenFit) {
    gd.parentElement.scrollTo({left: 0});
  }
  gd.plotlyScreenFit = !gd.plotlyScreenFit ;
  if (window.hasOwnProperty("sessionStorage")) {
      window.sessionStorage.setItem("plotlyScreenFit." + gd.id,
        gd.plotlyScreenFit);
  }
  sizeIframes(/* forceSetSizeToInit = */ false);
}

function getPlotlyWindowZoom(gd) {
  if (!gd.hasOwnProperty("plotlyScreenFit")) {
    gd.plotlyScreenFit = default_plotlyScreenFit;
    if (window.hasOwnProperty("sessionStorage")) {
      var plotlyScreenFit = JSON.parse(window.sessionStorage.getItem(
                                                "plotlyScreenFit." + gd.id));
      if (plotlyScreenFit != null) {
        gd.plotlyScreenFit = plotlyScreenFit ;
      }
    }
  }
  return gd.plotlyScreenFit;
}

$(function() {
  var resized = true;
  $(window).on("resize", function() {
    resized = true;
  });
  window.setInterval(function() {
    try {
      if (resized) {
        sizeIframes(/* forceSetSizeToInit = */ false);
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
    var iframes = $(currentContextMenu.reference).find("iframe")
    if (iframes.length == 1) {
      var iframe = iframes[0];
      var parent_nm = $(iframe).parents("div [data-nm]").attr("data-nm")
      sendMsgDownloadSVG(parent_nm)
      return ;
    }
  } catch(e) {
    console.log(e)
  }
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
    showTip("Error Copying Text to Clipboard.");
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

function fixTDforMozilla() {
  if (navigator.userAgent.toLowerCase().includes('firefox')) {
    var cells = $("table.myDT td")
    cells.each(function(cell) { $(cells[cell]).html("<div class=\"td-div\">" + $(cells[cell]).html() + "</div>") })
  }
}

$(function() {
  new ResizeObserver(function() {
      $("div.content").css("margin-top", Math.round($("div.navbar").height()) +
        "px");
  }).observe($('div.navbar')[0])
})

$(function() {
  if (!window.hasOwnProperty("dq_report2") || !window.dq_report2) {
    $("a").attr("href", "javascript:alert(\"links work in dq_report2 reports, only.\")")
  }
})


function sizeIframes(forceSetSizeToInit = false) {
  // $("[data-nm='con_hard_limits.SBP_0'] iframe").parent().css("resize")
  // $("[data-nm='con_hard_limits.SBP_0'] iframe").parent().css({"resize": "both"})
  // $("[data-nm='con_hard_limits.SBP_0'] iframe").parent().css({"resize": ""})
  const all_iframe_plots_scalers =
    $(document).find(".dataquieR_result[data-initialw][data-initialh] iframe").parent();
  const all_iframe_plots_result = all_iframe_plots_scalers.parent();
  all_iframe_plots_result.each(function(i, result_div) {
    const scaler_div = all_iframe_plots_scalers[i];
    if (forceSetSizeToInit) {
      // set size even if size differs from intial size -- for first call in $()
      // or a click to resize button in the plotly
      $(scaler_div).css({"resize": ""}) // turn off scaler handle
    }
    if ($(scaler_div).css("resize") != "both") { // auto sizing, now handle shown
      // set size only, if size has not been changed manually -- for resize calls
      const ih = parseInt($(result_div).attr("data-initialh"))
      const iw = parseInt($(result_div).attr("data-initialw"))
//      const h = def_height_in_vh * $(window).height() / 100 - 45 ; // 45 for the scrollbar
      const h = Infinity;
//      const w = def_width_in_vh * $(window).width() / 100 - 45; // for the scrollbar
      const w = Infinity;
      $(scaler_div).width(Math.min(iw, w));
      $(scaler_div).height(Math.min(ih, h));
    }
  })
}
