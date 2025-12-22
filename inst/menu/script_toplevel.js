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
  $('div.infobutton').each(function(index, element) {
    const template = element;
    const btn = $('<button />');
    btn.html('?').insertAfter(element);
    tippy(btn[0], { // https://atomiks.github.io/tippyjs/v6/html-content/
      content(reference) {
        return template.outerHTML;
      },
      allowHTML: true,
      trigger: 'click',
      interactive: true,
    });
    $(element).hide();
  })
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
    return ;
  } catch(e) {
    console.log(e)
  }
  try {
    var el = $(currentContextMenu.reference).find("div.dt-buttons button.buttons-excel");
    if (el.length > 0) {
      el.click()
      return ;
    }
  } catch(e) {
    console.log(e)
  }
  try {
    var id = $(currentContextMenu.reference).find("img").parent().prevAll("a[id]")[0].id
    download(id + ".png", $(currentContextMenu.reference).find("img")[0].src)
    return ;
  } catch (e) {
    console.log(e)
  }
  try {
    var imgTag = $(currentContextMenu.reference).find("img");
    if (imgTag.length == 0) return;
    var downloadFromImg = function() {
      var id = $(currentContextMenu.reference).attr("data-nm");
      download(id + ".png", $(currentContextMenu.reference).find("img").attr("src"))
    }
    var downloadFromPlotly = function() {
      tglePy(imgTag[0]);
      setTimeout(function() {
        dlResult()
      }, 1000)
    }
    if (imgTag.data("iframe") !=
        undefined) {
          yesnoDialog("This is a thumbnail, so you'll get a PNG, do you want to activate interactive mode and download an SVG, instead?",
            downloadFromPlotly,
            downloadFromImg)
        } else {
          downloadFromImg()
        }
    return ;
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
                     showTip(this.getAttribute("data-stderr").trim(),
                       delay = null);
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

var mh = null;

function showTip(msg, delay = 10000) {
  if (messenger == null) {
    console.log("showTip called too early.");
    return;
  }
  messenger.show();
  $("#messenger-msg").html(msg)
  if (delay !== null) {
    mh = window.setTimeout(messenger.hide, delay);
  }
}

function hideTip() {
  if (mh != null) {
    window.clearTimeout(mh);
    mh = null;
  }
  messenger.hide()
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
    if (!window.hasOwnProperty("dq_report_by_overview") || !window.dq_report_by_overview) {
      $("a").attr("href", "javascript:alert(\"links work in dq_report2 reports, only.\")")
    }
  }
})


function sizeIframes(forceSetSizeToInit = false) {
  // $("[data-nm='con_hard_limits.SBP_0'] iframe").parent().css("resize")
  // $("[data-nm='con_hard_limits.SBP_0'] iframe").parent().css({"resize": "both"})
  // $("[data-nm='con_hard_limits.SBP_0'] iframe").parent().css({"resize": ""})
  const all_iframe_plots_scalers =
    $(document).find(".scaler[data-initialw][data-initialh] iframe").parent();
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
      const ih = $(scaler_div).attr("data-initialh").replace(/;+$/g, '');
      const iw = $(scaler_div).attr("data-initialw").replace(/;+$/g, '');

      $(scaler_div).css({"width": iw})
      $(scaler_div).css({"height": ih})
      $(scaler_div).height($(scaler_div).height()) // trigger resize events for potly
    }

  })
}

var highlight_timer = null;

var highlight_menu = function(pattern) {
  try {
    var p = new RegExp(pattern, "i");
    $(".navbar a").filter(function() {
      return this.innerHTML.match(p);
    }).parent().show()
    $(".navbar a").filter(function() {
      return this.innerHTML.match(p);
    })[0].scrollIntoView()
    $(".navbar a").filter(function() {
      return this.innerHTML.match(p);
    }).effect("highlight", {}, 4000);
    var $this = $(this);
    setTimeout(function() { // https://stackoverflow.com/a/30107066
  //        $this.attr('disabled', false);
  //        $this.val('Submit');
      $(".navbar a").filter(function() {
        return this.innerHTML.match(p);
      }).parent().css("display", "");
    }, 4000);
  } catch(e) {
    console.log(e)
  }
}

function dismissContextMenu() {
  document.querySelectorAll(".dataquieR_result").forEach(function (el) {
    if (el._tippy && typeof el._tippy.hide === "function") {
      el._tippy.hide();
    }
});
}

$(function() {
  document.addEventListener("keydown", function (e) {
      if (e.key === "Escape") {
        dismissContextMenu()
      }
    });
});

var search_string = "";
document.onkeyup = function(e) {
  if (document.activeElement instanceof HTMLInputElement) {
    return ; // no auto-serch, if user tries to fill an input field
  }
  highlight_timer = window.setTimeout(function() {
    if (search_string.length > 2) {
      highlight_menu(search_string);
      hideTip()
      if (highlight_timer != null) {
        window.clearTimeout(highlight_timer);
        highlight_timer = null;
      }
    }
  }, 3000);
  if (e.ctrlKey && e.which == 70) {
//    console.log("Ctrl-F")
      search_string = "";
    hideTip();
    showTip("Search: " + search_string);
  } else if (e.which == 8) {
    if (search_string.length > 0) {
      search_string = search_string.substr(0, search_string.length - 1)
      hideTip();
      showTip("Search: " + search_string);
    }
  } else if (e.which == 27) {
    search_string = "";
    hideTip()
  } else if (e.which == 13) {
    if (highlight_timer != null) {
      window.clearTimeout(highlight_timer);
      highlight_timer = null;
    }
    highlight_menu(search_string);
    search_string = "";
    hideTip()
  } else {
    if (e.key.length == 1) {
      search_string = search_string + e.key ;
      hideTip();
      showTip("Search: " + search_string);
    }
  }
};

function tglePy(et) {
  var iframeHTML = $(et).data('iframe');
  var scaler = $(et).parent("div.scaler");
  var initialh = scaler.attr("data-initialh");
  var initialw = scaler.attr("data-initialw");
  $(et).replaceWith(iframeHTML);
  if (scaler.length == 1) {
    $(function() {
      scaler.css('width', initialw.replace(/;$/, ""));
      scaler.css('height', initialh.replace(/;$/, ""));
    });
  }
}

/* Debounced DataTables layout adjust in one or more result containers */
var dtRebuildTimer = null;
var dtRebuildPending = new Set();

function scheduleDtRebuild(resultDiv) {
  try {
    if (!resultDiv) return;
    dtRebuildPending.add(resultDiv);

    if (dtRebuildTimer) clearTimeout(dtRebuildTimer);
    dtRebuildTimer = setTimeout(function () {
      try {
        dtRebuildPending.forEach(function (container) {
          adjustDtInResult(container);
        });
      } catch (e) {
        console.log("DT adjust failed:", e);
      } finally {
        dtRebuildPending.clear();
      }
    }, 80);
  } catch (e) {
    console.log(e);
  }
}


/* Just adjust column widths / responsive / fixed header in all tables in a result */
function adjustDtInResult(container) {
  try {
    if (!container) return;
    var $container = $(container);

    $container.find("table.myDT").each(function(_, tbl) {
      try {
        var $tbl = $(tbl);

        // skip if table or its closest visible container is hidden or too narrow
        if (!$tbl.is(":visible")) return;

        var $wrap = $tbl.closest(".dq-table-cell, .table_result, .datatables.html-widget, .dataTables_wrapper");
        var availWidth = $wrap.innerWidth();
        if (!availWidth || !isFinite(availWidth) || availWidth < 30) {
          // container effectively collapsed – don't adjust yet
          return;
        }

        var dt = $tbl.DataTable();

        dt.columns.adjust();
        if (dt.responsive) dt.responsive.recalc();
        if (dt.fixedHeader) dt.fixedHeader.adjust();
        if (dt.fixedColumns && dt.fixedColumns().relayout) {
          dt.fixedColumns().relayout();
        }

        // Helps with scrollY / body height recalculation
        dt.draw(false);

      } catch (e) {
        // table not initialised yet; ignore
      }
    });
  } catch (e) {
    console.log("adjustDtInResult failed:", e);
  }
}


function tglePyHandler() {
  if (event.which == 1) {
    tglePy(event.target)
    return false;
  }
}

const initializedScalerDivs = new WeakSet();

function resize_scaler_div(mutationList) { // if resize-handle was clicked, makt it an iframe
  for (const mutation of mutationList) {
    const scalerdiv = mutation.target;

    if (!initializedScalerDivs.has(scalerdiv)) {
      initializedScalerDivs.add(scalerdiv);
      var sdiv = $(scalerdiv);
      var img  = sdiv.find("img");
      if (img.length == 1) {

        var initialh = sdiv.attr("data-initialh");
        var initialw = sdiv.attr("data-initialw");

        let w = img[0].naturalWidth;
        let h = img[0].naturalHeight;


        let iw = initialw.replace(/;$/, "");
        let ih = initialh.replace(/;$/, "");

        sdiv.css({ width: iw, height: ih });      // Bounding-Box setzen

        let maxW = sdiv.width();                  // Pixel-Breite nach CSS
        let maxH = sdiv.height();                 // Pixel-Höhe nach CSS
        let scale = Math.min(maxW / w, maxH / h, 1);  // höchstens 1 ⇒ nie hochskalieren

        sdiv.css({
          width:  (w * scale) + "px",
          height: (h * scale) + "px"
        });
      }
      continue; // skip first (initial) call
    }

    if (dataquieR.isReady()) { // not initial sizing
      if (window.dataquieR_single_result === true) return ;
      const et = $(scalerdiv).find("img[data-iframe]");
      if (et.length === 1) {
        tglePy(et[0]);
      }
    }
  }
}

$(function() {
  // https://stackoverflow.com/a/66487907
  let observer = new ResizeObserver(resize_scaler_div);
  $("div.scaler").each(function(i, obj) {
    observer.observe(obj);
  });
})

// React to scaler size changes (manual resize)
const scalerResizeObserver = new ResizeObserver((entries) => {
  for (const entry of entries) {
    const scaler = entry.target;
    const resultDiv = scaler.closest("div.dataquieR_result");
    if (!resultDiv) continue;

    // update plot/table layout for this scaler
    updatePlotTableLayoutForScaler(scaler);

    // debounce rebuilding/adapting datatables
    scheduleDtRebuild(resultDiv);
  }
});

function updatePlotTableLayoutForScaler(scaler_div) {
  try {
    var $scaler = $(scaler_div);

    // container that has the plot+table grid
    var $container = $scaler.closest(".dq-plot-table-result");
    if ($container.length === 0) return;

    // table cell and table / widget inside it
    var $tableCell = $container.find(".dq-table-cell").first();
    if ($tableCell.length === 0) return;

    var $tableWrapper = $tableCell.find(".datatables.html-widget, table.myDT, table").first();
    if ($tableWrapper.length === 0) return;

    // available width for the table (the grid column width)
    var availableWidth = $tableCell.innerWidth();
    if (!availableWidth || !isFinite(availableWidth)) return;

    // how much width the content actually wants
    var requiredWidth = $tableWrapper[0].scrollWidth || 0;

    // primary condition: does the table overflow its cell?
    var tableTooNarrow = requiredWidth > availableWidth * 1.05; // 5% tolerance

    // secondary condition: plot eats almost all width
    var containerWidth = $container.width() || 0;
    var plotWidth = $scaler.outerWidth(true) || 0;
    var plotTooWide = (containerWidth > 0 && plotWidth / containerWidth > 0.66);

    var shouldStack = tableTooNarrow || plotTooWide;

    if (shouldStack) {
      $container.addClass("dq-stack-table");
    } else {
      $container.removeClass("dq-stack-table");
    }
  } catch (e) {
    console.log(e);
  }
}


// Observe all scaler-divs
$(function() {
  $("div.scaler").each(function(i, obj) {
    scalerResizeObserver.observe(obj);
  });
  // Also react to browser window resizes
  $(window).on("resize", function() {
    // For each plot+table result, recompute layout based on available space
    $(".dataquieR_result.dq-plot-table-result div.scaler").each(function(_, scaler) {
      updatePlotTableLayoutForScaler(scaler);
      var resultDiv = scaler.closest("div.dataquieR_result");
      if (resultDiv) {
        scheduleDtRebuild(resultDiv);
      }
    });
  });
});

dataquieR = {
  isReady: function isDataquieReady() {
    return !$('#loading-ani').is(":visible");
  }
}
