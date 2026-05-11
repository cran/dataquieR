window.in_iframe = false;

//runs imidiatly
(() => {
  initDropdownHandlers();

  cleanUpSessionStorage();

  if (!SVGElement.prototype.contains) {
    SVGElement.prototype.contains = HTMLDivElement.prototype.contains;
  }

})()

//runs after dom is loaded
$(() => {

  initSinglePageViewFromHash();

  guardLinks();

  dtLayoutRebuilder = new DtLayoutRebuilder();

  initializePageElements();

  initIframeResized();

  initDataquieRResultTooltips();

  messageTooltip = new MessageTooltip()

  menuSearch = new MenuSearch();

  init_util_scaler_nav();

  initScalerResizeObserver();

  initDivNavbarResizeObserver();

  initResize_scaler_divResizeObserver();

  init_util_scaler_nav_clear_button();

  initPopupWindowManager();

  init_adjust_all();
})

//#region general init functions

/**
 * remove all info about initialized iframe divs, if such exist
 *
 * reset initialSizeReported
 *
 * used in script_ifrmae.js, look for "reset initialSizeReported" in that file
 */
function cleanUpSessionStorage() {
  for (let i = 0; i < sessionStorage.length; i++) {
    var key = sessionStorage.key(i);
    if (key.startsWith("initialSizeReported.")) {
      sessionStorage.removeItem(key);
    }
  }
}

/**
 * Initializes page elements and behaviors:
 * - Adjusts layout for navbar height.
 * - Activates hidden single page sections.
 * - Sets up Tippy.js tooltips for elements with a `title` attribute.
 * - Adds info buttons next to headings with tooltips for additional content.
 * - Enables/disables tooltips based on user preference.
 * - Manages floating menus on scroll.
 */
function initializePageElements() {

  $("div.singlePageFinal").css("padding-top", $(".navbar").height() + "px");
  $("div.content").css("margin-top", $(".navbar").height() + "px");

  /* Activate the hidden single pages, if hidden from the beginning, some wisgets cannot correctly be set up (namely datatables) */
  $(".singlePage").addClass("singlePageFinal")
  $(".singlePage").removeClass("singlePage")
  try {
    if (window.location.hash == "") {
      if (window.__dqPersistPopupHistory) window.__dqPersistPopupHistory();
      window.location.hash = $(".singlePageFinal")[0].id
    }
  } catch (e) {
  }
  $("[title]").each((i, node) => {
    if (node.title.trim() == "") {
      node.removeAttribute("title");
    }
  })
  fixTDforMozilla();
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
  $('div.infobutton').each(function (index, element) {
    const template = element;

    // find nearest preceding heading
    const $heading = $(element).prevAll('h1,h2,h3,h4,h5,h6').first();
    if (!$heading.length) return;

    const btn = $('<button type="button" />')
      .html('&#9432;') // ℹ
      .addClass('dq-infobutton')
      .attr('aria-label', 'Additional information');

    // ensure heading can host an absolute element
    if ($heading.css('position') === 'static') {
      $heading.css('position', 'relative');
    }

    $heading.append(btn);

    tippy(btn[0], {
      content() {
        return template.outerHTML;
      },
      allowHTML: true,
      trigger: 'click',
      interactive: true,
    });

    $(element).hide();
  });
  if ($("#_enabletippy").length == 1) {
    var handleEnableTippy = function () {
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

      })

    }
    $("#_enabletippy").click(handleEnableTippy)
    handleEnableTippy()
  }
  $(window).scroll(float_menus);
  //  hideMatrices()
  $(float_menus)
}

/**
 * Initializes iframe resizing on window resize events.
 * It triggers `sizeIframes()` once resizing stops, checking every 100ms.
 */
function initIframeResized() {
  let resized = true;
  $(window).on("resize", function () {
    resized = true;
  });
  window.setInterval(function () {
    try {
      if (resized) {
        sizeIframes(/* forceSetSizeToInit = */ false);
      }
    } finally {
      resized = false;
    }
  }, 100);
}

/**
 * --- util: UI button to clear figure cache (window.name) and reload ---
 */
function init_util_scaler_nav_clear_button() {
  "use strict";

  /**
 * Checks whether a cached scaler/navigation state exists for the current report.
 * Validates the cache against the current report root and (if available) report ID.
 *
 * @returns {boolean} True if a valid cache exists for the current report, otherwise false.
 */
  function util_scaler_nav_has_cache_for_current_report() {
    try {
      var store = util_scaler_nav_read_store && util_scaler_nav_read_store();
      if (!store) return false;
      var rootKey = util_scaler_nav_report_root && util_scaler_nav_report_root();
      if (!rootKey || store.r !== rootKey) return false;

      var rid = util_scaler_nav_current_report_id && util_scaler_nav_current_report_id();
      // If this page has a reportId, require an exact match.
      if (rid) {
        if (!!store.id && String(store.id) === String(rid)) {
          return Object.values(store.pages).some(p => p.length > 0);
        } else {
          return false;
        }
      }

      // Otherwise, root match is all we can reliably do.
      return true;
    } catch (e) { }
    return false;
  }

  /**
 * Clears cached scaler/navigation state for the current report.
 * Resets window.name storage and removes related sessionStorage entries
 * used for plot sizing and layout persistence.
 *
 * @returns {void}
 */
  function util_scaler_nav_clear_current_report_cache() {
    try {
      var store = util_scaler_nav_read_store && util_scaler_nav_read_store();
      if (store) {
        var rootKey = util_scaler_nav_report_root && util_scaler_nav_report_root();
        if (rootKey && store.r === rootKey) {
          var rid = util_scaler_nav_current_report_id && util_scaler_nav_current_report_id();
          if (!rid || (!!store.id && String(store.id) === String(rid))) {
            try { window.name = ""; } catch (e) { }
          }
        }
      }
    } catch (e) { }

    // Also clear per-report sessionStorage helpers (best-effort).
    try {
      if (window.sessionStorage) {
        var dels = [];
        for (var i = 0; i < sessionStorage.length; i++) {
          var k = sessionStorage.key(i);
          if (!k) continue;
          if (k.indexOf("plotlyScreenFit.") === 0) dels.push(k);
          if (k.indexOf("initialSizeReported.") === 0) dels.push(k);
        }
        for (var j = 0; j < dels.length; j++) sessionStorage.removeItem(dels[j]);
      }
    } catch (e) { }
  }

  /**
 * Installs a floating "reset figures" button in the UI.
 * The button clears cached interactive state and reloads the page when clicked.
 * It is only shown when a cache exists for the current report.
 *
 * @returns {void}
 */
  function util_scaler_nav_install_clear_button() {
    try {
      if (document.getElementById("dq-reset-figures-btn")) return;

      var btn = document.createElement("button");
      btn.id = "dq-reset-figures-btn";
      btn.type = "button";
      btn.setAttribute("aria-label", "Reset all figures to PNG thumbnails");
      btn.title = "Switch all figures back to PNG thumbnails (clears interactive cache and reloads)";

      // Small inline SVG "thumbnails reset" icon (custom drawn; no external license).
      btn.innerHTML =
        '<svg viewBox="0 0 24 24" width="18" height="18" aria-hidden="true" focusable="false">' +
        '  <rect x="4" y="6" width="16" height="12" rx="2" fill="none" stroke="currentColor" stroke-width="1.6"></rect>' +
        '  <path d="M7.2 15.6l3.3-3.2 2.6 2.4 2.1-1.8 2.9 2.6" fill="none" stroke="currentColor" stroke-width="1.6" stroke-linecap="round" stroke-linejoin="round"></path>' +
        '  <circle cx="9" cy="10" r="1.1" fill="currentColor"></circle>' +
        '  <path d="M6.2 5.2a4.8 4.8 0 0 1 6.3-.7" fill="none" stroke="currentColor" stroke-width="1.6" stroke-linecap="round"></path>' +
        '  <path d="M12.5 2.9l.2 2.5-2.4-.3" fill="none" stroke="currentColor" stroke-width="1.6" stroke-linecap="round" stroke-linejoin="round"></path>' +
        '</svg>';

      btn.style.position = "fixed";
      btn.style.right = "5px";
      btn.style.top = "5px";
      btn.style.zIndex = "2147483647";
      btn.style.width = "30px";
      btn.style.height = "30px";
      btn.style.display = "none"; // only shown when cache exists for this report

      btn.style.padding = "0";
      btn.style.lineHeight = "0";
      btn.style.borderRadius = "10px";
      btn.style.border = "1px solid rgba(0,0,0,0.20)";
      btn.style.background = "rgba(255,255,255,0.75)";
      btn.style.color = "rgba(0,0,0,0.55)";
      btn.style.boxShadow = "0 2px 10px rgba(0,0,0,0.10)";
      btn.style.opacity = "0.45";
      btn.style.cursor = "pointer";

      btn.addEventListener("mouseenter", function () {
        btn.style.opacity = "1";
        btn.style.color = "rgba(0,0,0,0.85)";
        btn.style.background = "rgba(255,255,255,0.92)";
      });
      btn.addEventListener("mouseleave", function () {
        btn.style.opacity = "0.45";
        btn.style.color = "rgba(0,0,0,0.55)";
        btn.style.background = "rgba(255,255,255,0.75)";
      });

      btn.addEventListener("click", function (e) {
        try { e.preventDefault(); } catch (err) { }
        // Prevent our unload handlers from saving the current page state again.
        try { window.dqScalerNavDisableSave = true; } catch (err0) { }
        util_scaler_nav_clear_current_report_cache();
        try { location.reload(); } catch (err2) { }
      });

      (document.body || document.documentElement).appendChild(btn);

      // expose updater so save() can make the button appear when needed
      window.util_scaler_nav_update_clear_button = function () {
        try {
          var show = util_scaler_nav_has_cache_for_current_report();
          btn.style.display = show ? "block" : "none";
          btn.style.alignItems = "center";
          btn.style.justifyContent = "center";
          const svg = btn.querySelector('svg');
          svg.style.removeProperty('display');
          svg.querySelectorAll('[style*="display"]').forEach(n => {
            n.style.removeProperty('display');
          });
        } catch (e) { }
      };

      window.setTimeout(window.util_scaler_nav_update_clear_button, 0);

    } catch (e) { }
  }

  util_scaler_nav_install_clear_button();
}

/**
 * Initializes a ResizeObserver on the navbar element to keep page content properly offset.
 * When the navbar height changes, updates the top margin of content to avoid overlap.
 */
function initDivNavbarResizeObserver() {
  new ResizeObserver(function () {
    $("div.content").css("margin-top", Math.round($("div.navbar").height()) +
      "px");
  }).observe($('div.navbar')[0])
}

/**
 * Disables navigation links in unsupported report modes by replacing all anchor hrefs
 * with a no-op alert. Only applies when specific report globals are not present.
 *
 * @returns {void}
 */
function guardLinks() {
  if (!window.hasOwnProperty("dq_report2") || !window.dq_report2) {
    if (!window.hasOwnProperty("dq_report_by_overview") || !window.dq_report_by_overview) {
      $("a").attr("href", "javascript:alert(\"links work in dq_report2 reports, only.\")")
    }
  }
}

/**
 * Initializes a hash-based view switcher for a single-page HTML document.
 *
 * This function enables “multi-page” behavior within one file by showing
 * only a specific result block and hiding all other DOM elements.
 */
function initSinglePageViewFromHash() {
  const PREFIX_SHOW = "nm=";
  const MARK = "data-dq-singlepage-patched";

  /**
 * Extracts a name from the URL hash if it matches the expected prefix.
 * @returns {string | null}
 */
  function getNmFromHash() {
    const h = (window.location.hash || "").replace(/^#/, "");
    if (!h || h.indexOf(PREFIX_SHOW) !== 0) return null;
    return decodeURIComponent(h.slice(PREFIX_SHOW.length));
  }

  /**
 * Forces an element's display style using !important and marks it as patched.
 * @param {HTMLElement} el
 * @param {"none" | "revert" | "block"} value
 * @returns {void}
 */
  function setDisplayImportant(el, value) {
    // value: "none" | "revert" (prefer) | "block" (fallback if you ever want)
    el.style.setProperty("display", value, "important");
    el.setAttribute(MARK, "1");
  }

  /**
 * Removes previously applied display override if present.
 * @param {HTMLElement} el
 * @returns {void}
 */
  function clearDisplayOverride(el) {
    if (el.hasAttribute(MARK)) {
      el.style.removeProperty("display");
      el.removeAttribute(MARK);
    }
  }

  /**
 * Checks whether an element should be excluded from DOM manipulation.
 * @param {HTMLElement} el
 * @returns {boolean}
 */
  function isSkippableTag(el) {
    // never touch these (keep scripts/styles functional; avoid weird rendering side effects)
    const tag = (el.tagName || "").toUpperCase();
    return tag === "SCRIPT" || tag === "STYLE" || tag === "LINK" ||
      tag === "META" || tag === "NOSCRIPT" || tag === "TEMPLATE";
  }

  /**
 * Applies single-page visibility rules based on the current hash selection.
 * Shows the matching result block and hides all unrelated DOM elements.
 * @returns {void}
 */
  function apply() {
    const nm = getNmFromHash();

    // 0) Revert only what we changed in a previous run
    $("[data-dq-singlepage-patched]").each(function () {
      clearDisplayOverride(this);
    });

    if (!nm) return;

    // 1) Find wanted blocks
    const $wanted = $("div.dataquieR_result").filter(function () {
      return $(this).attr("data-nm") === nm;
    });

    if (!$wanted.length) return;

    // 2) Build "keep" set: wanted + ancestors + descendants
    const keep = new Set(
      $wanted
        .add($wanted.parents())
        .add($wanted.find("*"))
        .get()
    );

    // 3) Hide everything outside keep (but don't touch skippable tags)
    $("body *").each(function () {
      if (isSkippableTag(this)) return;
      if (keep.has(this)) return;
      setDisplayImportant(this, "none");
    });

    // 4) Ensure wanted + their ancestors are visible even if parent CSS hid them
    //    Use "revert" to not break table/flex/etc.
    $wanted.each(function () {
      $(this).parents().addBack().each(function () {
        if (isSkippableTag(this)) return;
        setDisplayImportant(this, "revert");
      });
    });
  }

  apply();
  $(window).on("hashchange", apply);
}

/**
 * Initializes a popup window manager for opening and controlling result views
 * inside draggable, resizable dialog windows.
 */
function initPopupWindowManager() { // TODO: scaler-div inside resizable dialogs is a bit duplicated, make them always 100% if shown in a dialog.
  "use strict";

  const NS = "__dqPopups__";
  const HIST_OPEN_KEY = "dq_open_results";
  const HIST_GEOM_KEY = "dq_popup_geom"; // url -> {l,t,w,h}
  const HIST_META_KEY = "dq_popup_meta"; // url -> { link_url, title }

  const MOUSE_KEY = "__dqPopupMouse__";
  const CLOSEALL_ID = "dq-closeall-popups";

  function state() {
    if (!window[NS]) {
      window[NS] = {
        nextId: 1,
        z: 200000,
        byKey: Object.create(null), // urlKey -> $win
        suppressHistory: false,
        historyInstalled: false,
        keysInZ: [],               // bottom -> top list of keys
        activeKey: null,
        escInstalled: false,
        mouseInstalled: false
      };
    }
    return window[NS];
  }

  function ensureCss() {
    if (document.getElementById("dq-popup-css")) return;

    const css = `
.dq-popup {
  position: fixed;
  width: 640px;
  height: 420px;
  max-width: 92vw;
  max-height: 86vh;
  background: #fff;
  border-radius: 10px;
  box-shadow: 0 16px 40px rgba(0,0,0,.22);
  border: 1px solid rgba(0,0,0,.15);
  overflow: hidden;
}

/* fallback resizable (native) when jQuery UI is not present */
.dq-popup.dq-css-resize {
  resize: both;
  overflow: hidden;
  min-width: 360px;
  min-height: 240px;
}

.dq-head {
  height: 34px;
  display: flex;
  align-items: center;
  justify-content: space-between;
  padding: 0 8px;
  background: rgba(0,0,0,.05);
  cursor: move;
  user-select: none;
}
.dq-title {
  font: 13px system-ui, sans-serif;
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
  opacity: .8;
}
.dq-close {
  border: 0;
  background: transparent;
  font-size: 18px;
  cursor: pointer;
}

.dq-body { height: calc(100% - 34px - 26px); }
.dq-body iframe { width: 100%; height: 100%; border: 0; }

.dq-foot {
  height: 26px;
  display: flex;
  align-items: center;
  justify-content: space-between;
  padding: 0 10px;
  border-top: 1px solid rgba(0,0,0,.10);
  background: rgba(0,0,0,.03);
  font: 12px system-ui, sans-serif;
}
.dq-foot a { opacity: .85; text-decoration: none; }
.dq-foot a:hover { text-decoration: underline; opacity: 1; }

@keyframes dqFlash {
  0% { box-shadow: 0 16px 40px rgba(0,0,0,.22); }
  40% { box-shadow: 0 0 0 4px rgba(255,200,0,.6), 0 16px 40px rgba(0,0,0,.22); }
  100% { box-shadow: 0 16px 40px rgba(0,0,0,.22); }
}
.dq-flash { animation: dqFlash .6s ease-out; }

/* fixed "close all" button */
#${CLOSEALL_ID}{
  position: fixed;
  top: 10px;
  right: 40px;
  z-index: 2147483647;
  border: 1px solid rgba(0,0,0,.18);
  background: rgba(255,255,255,.92);
  border-radius: 10px;
  padding: 6px 10px;
  cursor: pointer;
  font: 12px system-ui, sans-serif;
  box-shadow: 0 10px 26px rgba(0,0,0,.18);
  user-select: none;
}
#${CLOSEALL_ID}:hover{
  background: rgba(255,255,255,1);
}
#${CLOSEALL_ID} .dq-x{
  display: inline-block;
  width: 18px;
  height: 18px;
  line-height: 18px;
  text-align: center;
  border-radius: 6px;
  margin-right: 6px;
  border: 1px solid rgba(0,0,0,.16);
  background: rgba(0,0,0,.04);
  font-weight: 700;
}
`;
    $("<style id='dq-popup-css'>").text(css).appendTo("head");
  }

  function urlKey(url) {
    // Keep hash -> different #nm=... => different dialog + different geometry
    return String(url || "");
  }

  function bringToFront($w) {
    const s = state();
    s.z++;
    $w.css("z-index", s.z);
  }

  function flash($w) {
    $w.removeClass("dq-flash");
    void $w[0].offsetWidth;
    $w.addClass("dq-flash");
  }

  function setFooterLink($w, link_url) {
    const $a = $w.find(".dq-link");

    if (link_url) {
      $a
        .attr("href", link_url)
        .text("Open full page")
        .show()
        .off("click.dqhist")
        .on("click.dqhist", function () {
          // Only matters for same-tab navigation; harmless otherwise
          if (window.__dqPersistPopupHistory) window.__dqPersistPopupHistory();
        });
    } else {
      $a.removeAttr("href").hide().off("click.dqhist");
    }
  }

  function setTitle($w, url, title) {
    const fallbackTitle = String(url).split(/[?#]/)[0].split("/").pop();
    $w.find(".dq-title").text(title ? String(title) : fallbackTitle);
  }

  // ---- mouse tracking (for initial placement near click) ----

  function ensureMouseTrackingOnce() {
    const s = state();
    if (s.mouseInstalled) return;
    s.mouseInstalled = true;

    // Track last pointer position (mouse + touch + pen)
    function remember(e) {
      let x = null, y = null;
      if (e && e.touches && e.touches.length) {
        x = e.touches[0].clientX; y = e.touches[0].clientY;
      } else if (e && typeof e.clientX === "number") {
        x = e.clientX; y = e.clientY;
      }
      if (x === null || y === null) return;
      window[MOUSE_KEY] = { x: x, y: y, t: Date.now() };
    }

    document.addEventListener("pointerdown", remember, true);
    document.addEventListener("pointermove", remember, true);
    document.addEventListener("mousedown", remember, true);
    document.addEventListener("mousemove", remember, true);
    document.addEventListener("touchstart", remember, true);
    document.addEventListener("touchmove", remember, true);
  }

  function lastMousePos() {
    const m = window[MOUSE_KEY];
    if (!m || typeof m.x !== "number" || typeof m.y !== "number") return null;
    return m;
  }

  function clamp(v, lo, hi) { return Math.max(lo, Math.min(hi, v)); }

  function rectsOverlap(a, b) {
    return !(a.r <= b.l || a.l >= b.r || a.b <= b.t || a.t >= b.b);
  }

  function getViewportRect() {
    const vw = window.innerWidth || 1200;
    const vh = window.innerHeight || 800;
    return { l: 0, t: 0, r: vw, b: vh };
  }

  function getRectAt(l, t, w, h) {
    return { l: l, t: t, r: l + w, b: t + h };
  }

  function computePosNearPointerAvoidingOverlap(w, h) {
    const s = state();
    const vp = getViewportRect();
    const m = lastMousePos();

    const mx = m ? m.x : (vp.r - w - 24);
    const my = m ? m.y : 24;

    // Collect existing rects
    const existing = [];
    Object.keys(s.byKey).forEach(function (k) {
      const $w = s.byKey[k];
      if (!$w || !$w.length) return;
      const l = parseFloat($w.css("left")) || 0;
      const t = parseFloat($w.css("top")) || 0;
      existing.push(getRectAt(l, t, $w.outerWidth(), $w.outerHeight()));
    });

    const candidates = [
      { l: mx + 16, t: my + 16 },
      { l: mx + 16, t: my - h - 16 },
      { l: mx - w - 16, t: my + 16 },
      { l: mx - w - 16, t: my - h - 16 },
      { l: vp.r - w - 24, t: 24 },
      { l: 24, t: 24 },
      { l: 24, t: vp.b - h - 24 },
      { l: vp.r - w - 24, t: vp.b - h - 24 }
    ];

    for (let i = 0; i < candidates.length; i++) {
      const c = candidates[i];
      const l = clamp(c.l, 10, vp.r - w - 10);
      const t = clamp(c.t, 10, vp.b - h - 10);
      const r = getRectAt(l, t, w, h);
      const overlaps = existing.some(function (er) { return rectsOverlap(r, er); });
      if (!overlaps) return { left: l, top: t };
    }

    // Fallback: cascade
    let l = clamp(mx + 16, 10, vp.r - w - 10);
    let t = clamp(my + 16, 10, vp.b - h - 10);
    for (let step = 0; step < 40; step++) {
      const r = getRectAt(l, t, w, h);
      const overlaps = existing.some(function (er) { return rectsOverlap(r, er); });
      if (!overlaps) return { left: l, top: t };
      l = clamp(l + 28, 10, vp.r - w - 10);
      t = clamp(t + 22, 10, vp.b - h - 10);
    }

    return { left: l, top: t };
  }

  // ---- "close all" ----

  function ensureCloseAllButton() {
    ensureCss();
    if (document.getElementById(CLOSEALL_ID)) return;

    const $btn = $(`
      <div id="${CLOSEALL_ID}" role="button" tabindex="0" aria-label="Close all pop-ups">
        <span class="dq-x">×</span>
        <span>Close all pop-ups</span>
      </div>
    `);

    function doCloseAll() { closeAllWindows(); }

    $btn.on("click", doCloseAll);
    $btn.on("keydown", function (e) {
      const k = e.key || e.keyCode;
      if (k === "Enter" || k === " " || k === 13 || k === 32) {
        e.preventDefault();
        doCloseAll();
      }
    });

    $("body").append($btn);
    updateCloseAllVisibility();
  }

  function updateCloseAllVisibility() {
    const s = state();
    const hasAny = Object.keys(s.byKey).length > 0;
    const el = document.getElementById(CLOSEALL_ID);
    if (!el) return;
    el.style.display = hasAny ? "block" : "none";
  }

  function closeAllWindows() {
    const s = state();
    const keys = Object.keys(s.byKey);

    if (!keys.length) {
      updateCloseAllVisibility();
      return;
    }

    persistSnapshotOnLeave();

    s.suppressHistory = true;

    keys.forEach(function (k) {
      const $w = s.byKey[k];
      if ($w && $w.length) {
        try { saveOneGeom(k, $w, true); } catch (e) { }
        $w.remove();
      }
      delete s.byKey[k];
    });

    // Create a NEW history entry that represents "all closed"
    // Must NOT be suppressed; use pushState (replace=false)
    s.suppressHistory = false;
    try { setOpenList([], false); } catch (e) { }

    s.keysInZ = [];
    s.activeKey = null;
    s.suppressHistory = false;

    persistSnapshotOnLeave();
    updateCloseAllVisibility();
  }

  // ---- active window + ESC close ----

  function markActiveByKey(key) {
    const s = state();
    if (!key) return;
    s.activeKey = key;

    // Update z-order list
    const i = s.keysInZ.indexOf(key);
    if (i !== -1) s.keysInZ.splice(i, 1);
    s.keysInZ.push(key);
  }

  function topmostKeyFallback() {
    const s = state();
    if (s.activeKey && s.byKey[s.activeKey]) return s.activeKey;

    // If we have a z list, use last existing
    for (let i = s.keysInZ.length - 1; i >= 0; i--) {
      const k = s.keysInZ[i];
      if (s.byKey[k]) return k;
    }

    // Last resort: compute by z-index
    let bestK = null;
    let bestZ = -Infinity;
    Object.keys(s.byKey).forEach(function (k) {
      const $w = s.byKey[k];
      if (!$w || !$w.length) return;
      const z = parseFloat($w.css("z-index")) || 0;
      if (z > bestZ) { bestZ = z; bestK = k; }
    });
    return bestK;
  }

  function ensureEscOnce() {
    const s = state();
    if (s.escInstalled) return;
    s.escInstalled = true;

    document.addEventListener("keydown", function (e) {
      const key = e.key || e.keyCode;
      if (key !== "Escape" && key !== "Esc" && key !== 27) return;

      const k = topmostKeyFallback();
      if (!k) return;

      // Close "focused/topmost" dialog
      const $w = s.byKey[k];
      if ($w && $w.length) {
        $w.find(".dq-close").trigger("click");
        e.preventDefault();
        e.stopPropagation();
      }
    }, true);
  }

  // ---- History helpers (open list + geometry map) ----

  function getStateObj() {
    return (history.state && typeof history.state === "object") ? history.state : {};
  }

  function getOpenList() {
    const st = getStateObj();
    return Array.isArray(st[HIST_OPEN_KEY]) ? st[HIST_OPEN_KEY].slice() : [];
  }

  function getGeomMap() {
    const st = getStateObj();
    const m = st[HIST_GEOM_KEY];
    return (m && typeof m === "object") ? Object.assign({}, m) : {};
  }

  function getMetaMap() {
    const st = getStateObj();
    const m = st[HIST_META_KEY];
    return (m && typeof m === "object") ? Object.assign({}, m) : {};
  }

  function setMetaMap(nextMap, replace) {
    const st = Object.assign({}, getStateObj());
    st[HIST_META_KEY] = nextMap;
    setHistory(st, replace);
  }

  function saveOneMeta(url, link_url, title, replace) {
    const key = urlKey(url);
    const m = getMetaMap();

    // keep what we already know unless caller provides new info
    const prev = m[key] && typeof m[key] === "object" ? m[key] : {};
    m[key] = {
      link_url: (typeof link_url === "string" && link_url.length) ? link_url : (prev.link_url || ""),
      title: (typeof title === "string" && title.length) ? title : (prev.title || "")
    };

    setMetaMap(m, replace);
  }

  function setHistory(stNext, replace) {
    const s = state();
    if (s.suppressHistory) return;
    try {
      if (replace) history.replaceState(stNext, "");
      else history.pushState(stNext, "");
    } catch (e) { }
  }

  function setOpenList(nextList, replace) {
    const st = Object.assign({}, getStateObj());
    st[HIST_OPEN_KEY] = nextList;
    setHistory(st, replace);
  }

  function setGeomMap(nextMap, replace) {
    const st = Object.assign({}, getStateObj());
    st[HIST_GEOM_KEY] = nextMap;
    setHistory(st, replace);
  }

  function saveOneGeom(url, $w, replace) {
    const key = urlKey(url);
    const m = getGeomMap();

    m[key] = {
      l: parseFloat($w.css("left")) || 0,
      t: parseFloat($w.css("top")) || 0,
      w: $w.outerWidth(),
      h: $w.outerHeight()
    };

    setGeomMap(m, replace);
  }

  function buildGeomMapFromOpenWindows() {
    const s = state();
    const m = getGeomMap();

    Object.keys(s.byKey).forEach(function (key) {
      const $w = s.byKey[key];
      if (!$w || !$w.length) return;
      m[key] = {
        l: parseFloat($w.css("left")) || 0,
        t: parseFloat($w.css("top")) || 0,
        w: $w.outerWidth(),
        h: $w.outerHeight()
      };
    });

    return m;
  }

  function applySavedGeomIfAny(url, $w) {
    const key = urlKey(url);
    const m = getGeomMap();
    const g = m[key];
    if (!g) return false;

    const vw = window.innerWidth || 1200;
    const vh = window.innerHeight || 800;
    const w = Math.max(360, Math.min(g.w || 640, vw - 20));
    const h = Math.max(240, Math.min(g.h || 420, vh - 20));
    const l = Math.max(10, Math.min(g.l || 40, vw - w - 10));
    const t = Math.max(10, Math.min(g.t || 40, vh - h - 10));

    $w.css({ left: l, top: t, width: w, height: h });
    return true;
  }

  function historyAddUrl(url) {
    const key = urlKey(url);
    const list = getOpenList();
    if (list.indexOf(key) !== -1) return;
    list.push(key);
    setOpenList(list, false);
  }

  function historyRemoveUrl(url, replace) {
    const key = urlKey(url);
    const list = getOpenList().filter(function (u) { return u !== key; });
    setOpenList(list, !!replace);
  }

  // IMPORTANT FIX: snapshot open dialogs + geometry into *current* entry before leaving
  function persistSnapshotOnLeave() {
    const s = state();

    // authoritative open list from actually open windows
    const openNow = Object.keys(s.byKey);

    // update geometry map for those windows
    const geomNow = buildGeomMapFromOpenWindows();

    const st = Object.assign({}, getStateObj());
    st[HIST_OPEN_KEY] = openNow;
    st[HIST_GEOM_KEY] = geomNow;
    // meta map is already in state; keep it as-is

    try {
      history.replaceState(st, "");
    } catch (e) { }
  }

  // make it callable from other code (e.g. single_page_switch)
  window.__dqPersistPopupHistory = persistSnapshotOnLeave;

  function installHistorySyncOnce() {
    const s = state();
    if (s.historyInstalled) return;
    s.historyInstalled = true;

    function syncFromHistory() {
      const desired = new Set(getOpenList());
      const openNow = Object.keys(s.byKey);

      s.suppressHistory = true;

      // close those not desired
      openNow.forEach(function (u) {
        if (!desired.has(u)) {
          const $w = s.byKey[u];
          if ($w && $w.length) $w.remove();
          delete s.byKey[u];
        }
      });

      // open those desired but missing
      desired.forEach(function (u) {
        if (!s.byKey[u]) {
          const meta = getMetaMap()[u] || {};
          openWindow(u, meta.link_url || null, meta.title || null, true);
        }
      });

      s.suppressHistory = false;
      updateCloseAllVisibility();
    }

    window.addEventListener("popstate", syncFromHistory);
    window.addEventListener("pageshow", syncFromHistory);

    // Save snapshot when navigating away / switching to another HTML file / reload / bfcache
    window.addEventListener("pagehide", function () {
      persistSnapshotOnLeave();
    });

    // Hash navigation is your main "leave"/"switch" mechanism -> snapshot here too
    window.addEventListener("hashchange", function () {
      persistSnapshotOnLeave();
    }, true);

    // Extra safety for real navigations / reloads
    window.addEventListener("beforeunload", function () {
      persistSnapshotOnLeave();
    });

    document.addEventListener("visibilitychange", function () {
      if (document.visibilityState === "hidden") {
        persistSnapshotOnLeave();
      }
    });

    // Ensure keys exist (non-destructive) on current entry
    const st = Object.assign({}, getStateObj());
    if (!Array.isArray(st[HIST_OPEN_KEY])) st[HIST_OPEN_KEY] = [];
    if (!st[HIST_GEOM_KEY] || typeof st[HIST_GEOM_KEY] !== "object") st[HIST_GEOM_KEY] = {};
    if (!st[HIST_META_KEY] || typeof st[HIST_META_KEY] !== "object") st[HIST_META_KEY] = {};
    setHistory(st, true);

    syncFromHistory();
  }

  // ---- Drag/Resize with geometry saving ----

  function enableDrag($w, url) {
    if ($.ui && $.ui.draggable) {
      $w.draggable({
        handle: ".dq-head",
        containment: "window",
        start: function () { bringToFront($w); },
        stop: function () { saveOneGeom(url, $w, true); }
      });
      return;
    }

    let drag = false, sx, sy, sl, st;

    $w.on("mousedown", ".dq-head", function (e) {
      if ($(e.target).closest(".dq-close").length) return;
      drag = true;
      bringToFront($w);
      sx = e.clientX;
      sy = e.clientY;
      sl = parseFloat($w.css("left")) || 0;
      st = parseFloat($w.css("top")) || 0;
      e.preventDefault();
    });

    $(document).on("mousemove.dqdrag", function (e) {
      if (!drag) return;
      $w.css({ left: sl + e.clientX - sx, top: st + e.clientY - sy });
    }).on("mouseup.dqdrag", function () {
      if (!drag) return;
      drag = false;
      saveOneGeom(url, $w, true);
    });
  }

  function enableResize($w, url) {
    if ($.ui && $.ui.resizable) {
      $w.resizable({
        handles: "n,e,s,w,ne,nw,se,sw",
        minHeight: 240,
        minWidth: 360,
        start: function () { bringToFront($w); },
        stop: function () { saveOneGeom(url, $w, true); }
      });
    } else {
      $w.addClass("dq-css-resize");
      // Save geometry after a native resize ends (best-effort)
      let timer = null;
      $w.on("mouseup", function () {
        clearTimeout(timer);
        timer = setTimeout(function () { saveOneGeom(url, $w, true); }, 0);
      });
    }
  }

  // ---- Window open/close ----

  function openWindow(url, link_url, title, fromHistory) {
    ensureCss();
    ensureCloseAllButton();
    ensureEscOnce();
    ensureMouseTrackingOnce();
    installHistorySyncOnce();

    const s = state();
    const key = urlKey(url);

    // dedupe
    if (s.byKey[key]) {
      const $existing = s.byKey[key];
      bringToFront($existing);
      flash($existing);
      setFooterLink($existing, link_url);
      if (title) setTitle($existing, url, title);
      if (!fromHistory) saveOneMeta(url, link_url, title, true);
      if (!fromHistory) historyAddUrl(url);

      markActiveByKey(key);
      updateCloseAllVisibility();
      return $existing;
    }

    const id = s.nextId++;
    const $w = $(`
      <div class="dq-popup" role="dialog" aria-label="dataquieR result" tabindex="0">
        <div class="dq-head">
          <div class="dq-title"></div>
          <button type="button" class="dq-close" aria-label="Close">×</button>
        </div>
        <div class="dq-body">
          <iframe></iframe>
        </div>
        <div class="dq-foot">
          <a class="dq-link" rel="noopener noreferrer"></a>
          <span></span>
        </div>
      </div>
    `);

    setTitle($w, url, title);

    // IMPORTANT: iframe src is ORIGINAL url (hash preserved)
    $w.find("iframe").attr("src", url);

    setFooterLink($w, link_url);
    if (!fromHistory) saveOneMeta(url, link_url, title, true);

    $("body").append($w);

    // Default position:
    // Prefer saved geometry; otherwise place near pointer avoiding overlap.
    const hadSaved = applySavedGeomIfAny(url, $w);

    if (!hadSaved) {
      // ensure measurable size first
      const w0 = $w.outerWidth();
      const h0 = $w.outerHeight();
      const pos = computePosNearPointerAvoidingOverlap(w0, h0);
      $w.css(pos);
    }

    bringToFront($w);
    enableDrag($w, url);
    enableResize($w, url);
    flash($w);

    // Track "active/topmost" for ESC behavior
    function activate() {
      bringToFront($w);
      markActiveByKey(key);
    }

    $w.on("mousedown", activate);
    $w.on("focusin", activate);

    // When interacting inside the iframe, we only get a blur on parent;
    // so also treat pointerdown on window frame as activation (already above).

    $w.find(".dq-close").on("click", function () {
      // save geometry on close, then close
      saveOneGeom(url, $w, true);

      $w.remove();
      delete s.byKey[key];

      // remove from open list (but keep geom/meta in history for later reopen)
      if (!s.suppressHistory) historyRemoveUrl(url, false);

      // also snapshot right now (so leaving immediately after close is safe)
      persistSnapshotOnLeave();

      updateCloseAllVisibility();
    });

    s.byKey[key] = $w;

    // Save initial geom and update history open list
    saveOneGeom(url, $w, true);
    if (!fromHistory) historyAddUrl(url);

    // Activate newly opened
    markActiveByKey(key);
    updateCloseAllVisibility();

    // Focus the dialog frame (not the iframe) so ESC works immediately
    try { $w.trigger("focus"); } catch (e) { }

    return $w;
  }

  // Public API: showDataquieRResult(url, link_url, title)
  window.showDataquieRResult = function (url, link_url, title) {
    var ok = util_checked_navigate(link_url, { navigate: false });
    if (ok) {
      openWindow(url, link_url, title, false);
    }
  };

  // --- auto-init restore on page load ---
  function shouldAutoInit() {
    const st = getStateObj();
    const open = st[HIST_OPEN_KEY];
    const geom = st[HIST_GEOM_KEY];
    return (Array.isArray(open) && open.length > 0) ||
      (geom && typeof geom === "object" && Object.keys(geom).length > 0);
  }

  function autoInit() {
    try {
      if (shouldAutoInit()) installHistorySyncOnce();
      ensureCloseAllButton();
      ensureEscOnce();
      ensureMouseTrackingOnce();
    } catch (e) { }
  }

  // If DOM is ready later, run then…
  $(autoInit);
  // …and also run once immediately in case we're already ready
  if (document.readyState === "interactive" || document.readyState === "complete") {
    autoInit();
  }

}

//#endregion

//#region general stuff
// or stuff that didn't fit elsewhere and I thought not important enough for its own region

/**
 * Determines if a given CSS selector is supported by the current browser.
 *
 * https://stackoverflow.com/a/61597966
 * @param {string} selector - The CSS selector to test.
 * @returns {boolean} - Returns true if the selector is supported, false otherwise.
 */
function supportsSelector(selector) {
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

/**
 * Generates dynamic breadcrumb navigation for the page based on its content and URL structure.
 * The breadcrumb trail is updated depending on whether the page is a "real page" or a description page from the concept database.
 *
 * - If the page contains specific elements, it creates a breadcrumb trail with links to `report.html` and the current page.
 * - If the page is a description page (identified by the `#desc_` hash in the URL), it creates a breadcrumb trail with a link to `report.html` and the description content.
 *
 * @function handle_bread_crumbs
 * @returns {void} No return value. It directly modifies the DOM to insert breadcrumb navigation.
 */
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

/**
 * Initializes and configures a responsive sunburst chart with interactive drilldowns,
 * custom data handling, and dynamic UI controls. This function is invoked after the
 * sunburst chart element is rendered and is responsible for setting up interactivity,
 * managing state with the History API, and adding custom UI elements such as sliders.
 *
 * @param {HTMLElement} el - The DOM element where the sunburst plot is rendered.
 * @param {Object} x - The configuration object passed from the server (e.g., R) containing parameters like `ex_init`.
 * @param {Object} data - The data object containing the initial state of the chart, including optional `ex_init` and other attributes.
 *
 * @returns {void}
 *
 * @example
 * sunburst_on_render(document.getElementById('sunburst-chart'), { ex_init: 1.5 }, { ex_init: 2.0 });
 */
function sunburst_on_render(el, x, data) {

  $(function () {
    var update = {
      width: $(el).parent().width(),
      height: $(el).parent().height()
    };
    Plotly.relayout(el, update);
  })

  // --- sunburst drilldown state via History API (no location.hash usage) ---
  var DQ_SUNBURST_STATE_KEY = 'dq_sunburst_level';
  var DQ_SUNBURST_EX_KEY = 'dq_sunburst_ex'; // keep for compatibility: now interpreted as "p"

  // per-widget default, set from R via onRender(..., data = list(ex_init = <number>))
  var dq_default_ex = (function () {
    var src = (data && typeof data === 'object') ? data : null;
    // fallback for any older wiring attempts
    var raw = (src && typeof src.ex_init !== 'undefined') ? src.ex_init :
      (x && typeof x.ex_init !== 'undefined') ? x.ex_init : 1.0;

    var v = Number(raw);
    return (isFinite(v) && v > 0) ? v : 1.0;
  })();

  var dq_current_level =
    ((history.state || {})[DQ_SUNBURST_STATE_KEY]) || '';

  var dq_current_ex = (function () {
    var st = history.state;
    var v = (st && typeof st === 'object') ? st[DQ_SUNBURST_EX_KEY] : null;
    v = (v === null || typeof v === 'undefined') ? dq_default_ex : Number(v);
    return (isFinite(v) && v > 0) ? v : 1.0;
  })();

  var gd = (el && el.querySelector) ? (el.querySelector('.js-plotly-plot') || el) : el;

  function dq_apply_level(level) {
    if (typeof Plotly === 'undefined') return;
    var lv = (level === null || typeof level === 'undefined') ? '' : level;
    try { Plotly.restyle(gd, { level: lv }, [0]); } catch (e) { }
  }

  function dq_set_history_level(level, replace) {
    var st = (history.state && typeof history.state === 'object') ? history.state : {};
    if (st && st[DQ_SUNBURST_STATE_KEY] === level) return;
    var next = Object.assign({}, st);
    next[DQ_SUNBURST_STATE_KEY] = level;
    try {
      if (replace) history.replaceState(next, '');
      else history.pushState(next, '');
    } catch (e) { }
  }

  function dq_set_history_ex(ex, replace) {
    var st = (history.state && typeof history.state === 'object') ? history.state : {};
    var v = Number(ex);
    if (!isFinite(v) || v <= 0) v = 1.0;
    if (st && st[DQ_SUNBURST_EX_KEY] === v) return;
    var next = Object.assign({}, st);
    next[DQ_SUNBURST_EX_KEY] = v;
    try {
      if (replace) history.replaceState(next, '');
      else history.pushState(next, '');
    } catch (e) { }
  }

  function dq_restore() {
    var st = history.state;

    var level = (st && typeof st === 'object') ? (st[DQ_SUNBURST_STATE_KEY] || '') : '';
    dq_current_level = level || '';
    dq_apply_level(dq_current_level);

    var ex = (st && typeof st === 'object') ? st[DQ_SUNBURST_EX_KEY] : null;
    ex = (ex === null || typeof ex === 'undefined') ? dq_current_ex : Number(ex);
    if (!isFinite(ex) || ex <= 0) ex = 1.0;
    dq_current_ex = ex;

    // IMPORTANT: overwrite initial R-computed marker.colors with subtree power-mean colors
    dq_apply_ex(dq_current_ex);

    // keep UI in sync (if present)
    if (gd && gd.__dqExUI && gd.__dqExUI.input && gd.__dqExUI.value) {
      gd.__dqExUI.input.value = String(dq_current_ex);
      gd.__dqExUI.value.textContent = (Math.round(dq_current_ex * 100) / 100).toFixed(2);
    }
  }

  // --- customdata / normalization helpers ---
  function dq_get_customdata() {
    try {
      return (gd && gd.data && gd.data[0] && gd.data[0].customdata) ? gd.data[0].customdata : [];
    } catch (e) { }
    return [];
  }

  function dq_norm_severity(sev) {
    var s = Number(sev);
    if (!isFinite(s)) return null;
    // expects 1..5; clamp for safety
    if (s < 1) s = 1;
    if (s > 5) s = 5;
    return (s - 1) / 4; // 0..1
  }

  // --- build tree cache once (ids/parents/children + postorder + leafT) ---
  function dq_build_tree_cache() {
    if (!gd || !gd.data || !gd.data[0]) return null;

    var ids = gd.data[0].ids || [];
    var parents = gd.data[0].parents || [];
    var cd = dq_get_customdata();

    var n = ids.length;
    if (!n) return null;

    var idxById = Object.create(null);
    for (var i = 0; i < n; i++) idxById[String(ids[i])] = i;

    var children = new Array(n);
    for (var i = 0; i < n; i++) children[i] = [];

    var roots = [];
    for (var i = 0; i < n; i++) {
      var p = parents[i];
      if (p === null || typeof p === 'undefined' || String(p) === '') {
        roots.push(i);
      } else {
        var pi = idxById[String(p)];
        if (typeof pi === 'number') children[pi].push(i);
        else roots.push(i);
      }
    }

    // postorder traversal (children before parent)
    var post = [];
    var stack = roots.map(function (r) { return [r, 0]; });
    while (stack.length) {
      var top = stack[stack.length - 1];
      var v = top[0], j = top[1];
      if (j < children[v].length) {
        top[1] = j + 1;
        stack.push([children[v][j], 0]);
      } else {
        stack.pop();
        post.push(v);
      }
    }

    // leaf normalized severities t in [0..1] from customdata.severity_num
    var leafT = new Array(n);
    for (var i = 0; i < n; i++) {
      var item = cd[i];
      var sev = item ? item.severity_num : null;
      leafT[i] = dq_norm_severity(sev);
    }

    return { n: n, ids: ids, parents: parents, children: children, roots: roots, post: post, leafT: leafT };
  }

  // --- subtree power-mean coloring (true proxy: mean -> max as p increases) ---
  function dq_apply_ex(ex) {
    if (typeof Plotly === 'undefined') return;

    var p = Number(ex);
    if (!isFinite(p) || p <= 0) p = 1.0;

    // build cache once
    if (!gd.__dqTreeCache) gd.__dqTreeCache = dq_build_tree_cache();
    var C = gd.__dqTreeCache;
    if (!C) return;

    var n = C.n;
    var children = C.children;
    var post = C.post;
    var leafT = C.leafT;

    // DP accumulators
    var count = new Array(n);
    var sumP = new Array(n);
    var maxT = new Array(n);

    // bottom-up accumulation over subtree leaves
    for (var k = 0; k < post.length; k++) {
      var i = post[k];

      if (!children[i].length) {
        // leaf
        var t = leafT[i];
        if (t === null || typeof t === 'undefined' || !isFinite(t)) t = 0;
        if (t < 0) t = 0;
        if (t > 1) t = 1;

        count[i] = 1;
        maxT[i] = t;
        sumP[i] = Math.pow(t, p);
      } else {
        var c = 0;
        var s = 0;
        var m = 0;
        for (var jj = 0; jj < children[i].length; jj++) {
          var ch = children[i][jj];
          c += count[ch] || 0;
          s += sumP[ch] || 0;
          var mt = maxT[ch] || 0;
          if (mt > m) m = mt;
        }
        if (c <= 0) { c = 1; s = 0; m = 0; }
        count[i] = c;
        sumP[i] = s;
        maxT[i] = m;
      }
    }

    // compute power mean per node; for very large p, shortcut to max to avoid numeric issues
    var colors = new Array(n);
    var useMax = (p >= 50); // adjustable

    for (var i = 0; i < n; i++) {
      var v;
      if (useMax) {
        v = maxT[i] || 0;
      } else {
        var c = count[i] || 1;
        var s = sumP[i] || 0;
        v = (c > 0) ? Math.pow(s / c, 1 / p) : 0;
      }

      if (!isFinite(v)) v = 0;
      if (v < 0) v = 0;
      if (v > 1) v = 1;
      colors[i] = v;
    }

    try { Plotly.restyle(gd, { 'marker.colors': [colors] }, [0]); } catch (e) { }
  }

  // --- install history listeners once, set initial state, restore ---
  dq_set_history_level(dq_current_level || '', true);
  dq_set_history_ex(dq_current_ex, true);

  if (!window.__dqSunburstHistoryInstalled) {
    window.__dqSunburstHistoryInstalled = true;

    window.addEventListener('popstate', function () {
      dq_restore();
    });

    window.addEventListener('pageshow', function () {
      dq_restore();
    });
  }

  dq_restore();

  // --- existing hover/click behaviour (unchanged) ---
  function isNavigableHref(customdata) {
    if (!customdata) return false;
    if (!customdata.popup_href) return false;
    if (!customdata.href) return false;

    return (window.hasOwnProperty('all_ids') &&
      window.all_ids &&
      window.all_ids.hasOwnProperty('all_ids') &&
      Array.isArray(window.all_ids.all_ids) &&
      window.all_ids.all_ids.indexOf(customdata.href) !== -1);
  }

  function setCursor(cur) {
    gd.style.cursor = cur;
  }

  gd.on('plotly_hover', function (evt) {
    if (!evt || !evt.points || !evt.points.length) return;

    var customdata = evt.points[0].customdata;
    if (isNavigableHref(customdata)) {
      setCursor('pointer');
    } else {
      setCursor('default');
    }
  });

  gd.on('plotly_unhover', function (evt) {
    setCursor('default');
  });

  gd.on('plotly_click', function (evt) {
    if (!evt || !evt.points || !evt.points.length) return;

    var id = evt.points[0].id || '';
    var customdata = evt.points[0].customdata;

    if (isNavigableHref(customdata)) {
      showDataquieRResult(customdata.popup_href,
        customdata.href,
        customdata.title);
      return;
    }

    dq_current_level = id;
    dq_set_history_level(dq_current_level, false);
  });

  // --- slider UI + friendly tooltip (installed once per plot) ---
  if (gd && !gd.__dqExUIInstalled) {
    gd.__dqExUIInstalled = true;

    // "p" parameter: 1 = mean, larger = more worst-case emphasis
    var exMin = 1.0;
    var exMax = 25.0;
    var exStep = 0.5;

    function dq_is_small_screen() {
      try { return (window.innerWidth || 0) < 700; } catch (e) { }
      return false;
    }

    function dq_find_modebar() {
      try {
        // In plotly.js, modebar sits inside the plot container
        return gd.querySelector('.modebar');
      } catch (e) { }
      return null;
    }

    function dq_find_plot_container() {
      try {
        return gd.querySelector('.plot-container') || gd;
      } catch (e) { }
      return gd;
    }

    function dq_ensure_panel_styles() {
      if (document.getElementById('__dqSunburstExCss')) return;
      var css = document.createElement('style');
      css.id = '__dqSunburstExCss';
      css.type = 'text/css';
      css.textContent =
        ".dq-ex-panel{position:absolute; right:8px; top:44px; z-index:1000;" +
        "background:rgba(255,255,255,0.94); border:1px solid rgba(0,0,0,0.22);" +
        "border-radius:8px; padding:8px 10px; font:12px/1.2 sans-serif;" +
        "box-shadow:0 2px 10px rgba(0,0,0,0.15); max-width:280px;}" +
        ".dq-ex-panel .dq-row{display:flex; align-items:center; gap:8px;}" +
        ".dq-ex-panel input[type=range]{width:170px;}" +
        ".dq-ex-tip{margin-left:6px; cursor:help; opacity:0.75; user-select:none;}" +
        ".dq-ex-tipbox{position:absolute; right:0; top:34px; width:260px;" +
        "background:rgba(20,20,20,0.92); color:#fff; padding:8px 10px;" +
        "border-radius:6px; box-shadow:0 2px 10px rgba(0,0,0,0.25);" +
        "display:none; font:12px/1.25 sans-serif;}" +
        ".modebar-btn.dq-ex-btn{opacity:.5;}" +
        ".modebar-btn.dq-ex-btn:hover{opacity:1;}" +
        ".modebar {align-items: center;}" +
        ".modebar-group {align-items: center;}" +
        ".modebar > .modebar-group {align-self: center;}" +
        ".js-plotly-plot a.modebar-btn{display:inline-flex !important; align-items:center !important; justify-content:center !important; line-height:0 !important;}" +
        ".js-plotly-plot a.modebar-btn svg.icon{display:block !important; vertical-align:middle !important;}" +
        "";
      css.textContent +=
        /* Normalize Plotly modebar sizing so groups align */
        ".js-plotly-plot .modebar{display:flex !important; align-items:center !important;}" +
        ".js-plotly-plot .modebar-group{display:flex !important; align-items:center !important; height:32px !important;}" +
        ".js-plotly-plot a.modebar-btn{height:32px !important; line-height:32px !important; display:inline-flex !important; align-items:center !important;}" +
        ".js-plotly-plot a.modebar-btn svg.icon{display:block !important;}" +
        ".js-plotly-plot a.modebar-btn{min-width:32px !important; justify-content:center !important;}" +
        ".js-plotly-plot a.modebar-btn svg.icon{flex:0 0 auto !important;}" +
        "";

      document.head.appendChild(css);
    }

    dq_ensure_panel_styles();

    var plotContainer = dq_find_plot_container();
    var modebar = dq_find_modebar();

    // --- create the panel (hidden by default on small screens) ---
    var panel = document.createElement('div');
    panel.className = 'dq-ex-panel';

    // header row
    var header = document.createElement('div');
    header.style.display = 'flex';
    header.style.alignItems = 'center';
    header.style.justifyContent = 'space-between';
    header.style.gap = '8px';
    header.style.marginBottom = '6px';

    var lbl = document.createElement('div');
    lbl.textContent = 'Severity focus';

    var tip = document.createElement('span');
    tip.className = 'dq-ex-tip';
    tip.textContent = 'ⓘ';

    var tipBox = document.createElement('div');
    tipBox.className = 'dq-ex-tipbox';
    tipBox.textContent =
      "Left = colors show the overall picture in each branch. " +
      "Right = colors focus more and more on the worst items inside a branch. " +
      "It doesn’t change the data — just what gets highlighted.";

    function showTip() { tipBox.style.display = 'block'; }
    function hideTip() { tipBox.style.display = 'none'; }
    tip.addEventListener('mouseenter', showTip);
    tip.addEventListener('mouseleave', hideTip);
    tip.addEventListener('click', function () {
      tipBox.style.display = (tipBox.style.display === 'none') ? 'block' : 'none';
    });

    header.appendChild(lbl);
    header.appendChild(tip);

    // slider row
    var row = document.createElement('div');
    row.className = 'dq-row';

    var inp = document.createElement('input');
    inp.type = 'range';
    inp.min = String(exMin);
    inp.max = String(exMax);
    inp.step = String(exStep);
    inp.value = String(dq_current_ex);

    var val = document.createElement('span');
    val.textContent = (Math.round(dq_current_ex * 100) / 100).toFixed(2);

    inp.addEventListener('input', function () {
      var ex = Number(inp.value);
      if (!isFinite(ex) || ex <= 0) ex = 1.0;
      dq_current_ex = ex;
      val.textContent = (Math.round(dq_current_ex * 100) / 100).toFixed(2);

      dq_apply_ex(dq_current_ex);
      dq_set_history_ex(dq_current_ex, true); // don't spam history
    });

    row.appendChild(inp);
    row.appendChild(val);

    panel.appendChild(header);
    panel.appendChild(row);
    panel.appendChild(tipBox);

    // default visibility
    var panelOpen = false; // !dq_is_small_screen();
    panel.style.display = panelOpen ? 'block' : 'none';

    // panel positioning: below modebar if present
    function positionPanel() {
      try {
        var top = 44; // fallback
        var right = 8; // fallback

        if (modebar) {
          // compute right offset so panel's right edge aligns with modebar's right edge
          var pr = plotContainer.getBoundingClientRect();
          var mr = modebar.getBoundingClientRect();
          right = Math.max(0, Math.round(pr.right - mr.right));

          // position below modebar
          var mbTop = modebar.offsetTop || 0;
          var mbH = modebar.offsetHeight || 34;
          top = mbTop + mbH + 8;
        }

        panel.style.right = String(right) + 'px';
        panel.style.top = String(Math.max(8, Math.round(top))) + 'px';
      } catch (e) { }
    }
    gd.on('plotly_relayout', positionPanel);
    gd.on('plotly_redraw', positionPanel);

    // Append panel into plot container so it scrolls/clips with the plot
    // Ensure plotContainer is a positioned element for absolute children
    try {
      var cs = window.getComputedStyle ? window.getComputedStyle(plotContainer) : null;
      if (cs && cs.position === 'static') plotContainer.style.position = 'relative';
    } catch (e) { }
    plotContainer.appendChild(panel);
    positionPanel();

    // --- create a modebar-like toggle button ---
    function makeToggleButton() {
      // Use an existing Plotly modebar button as template
      var tpl = modebar ? modebar.querySelector('a.modebar-btn') : null;

      var btn;
      if (tpl) {
        btn = tpl.cloneNode(true); // clone markup, no event handlers
      } else {
        // fallback (should rarely happen)
        btn = document.createElement('a');
        btn.className = 'modebar-btn';
        btn.setAttribute('role', 'button');
        btn.setAttribute('tabindex', '0');
      }

      // Hard reset: the cloned tpl may carry inline style or "active" class
      btn.removeAttribute('style');
      btn.style.cssText = '';
      btn.className = 'modebar-btn';
      btn.classList.remove('active');

      // Remove any tooltip remnants / old handlers (cloneNode(true) keeps attributes)
      btn.removeAttribute('href');
      btn.onclick = null;

      // Set tooltip like Plotly does
      btn.setAttribute('data-title', 'Severity focus');
      btn.setAttribute('title', 'Severity focus');

      btn.classList.add('dq-ex-btn');

      // Replace icon with a 1000x1000 viewBox (Plotly icons use that scale)
      btn.innerHTML =
        '<svg class="icon" viewBox="0 0 1000 1000" width="1em" height="1em" aria-hidden="true">' +
        // 3 horizontal lines
        '<path d="M200 260H800M200 500H800M200 740H800" ' +
        'fill="none" stroke="currentColor" stroke-width="80" stroke-linecap="round"/>' +
        // 3 knobs
        '<circle cx="360" cy="260" r="90" fill="currentColor"/>' +
        '<circle cx="640" cy="500" r="90" fill="currentColor"/>' +
        '<circle cx="460" cy="740" r="90" fill="currentColor"/>' +
        '</svg>';

      function togglePanel(e) {
        if (e && e.preventDefault) e.preventDefault();
        panelOpen = !panelOpen;
        panel.style.display = panelOpen ? 'block' : 'none';
        if (!panelOpen) tipBox.style.display = 'none';
        positionPanel();
      }

      btn.addEventListener('click', togglePanel);
      btn.addEventListener('keydown', function (e) {
        var k = e.key || e.keyCode;
        if (k === 'Enter' || k === ' ' || k === 13 || k === 32) togglePanel(e);
      });

      return btn;
    }

    // Try to attach next to Plotly modebar buttons; fallback to panel only
    var toggleBtn = null;
    if (modebar) {
      // Put button into an existing group if possible
      var groups = modebar.querySelectorAll('.modebar-group');
      var group = groups.length ? groups[groups.length - 1] : modebar;
      toggleBtn = makeToggleButton();
      group.appendChild(toggleBtn);
    } else {
      // no modebar found: keep panel small-screen-collapsed but offer quick open on click
      panel.style.display = dq_is_small_screen() ? 'none' : 'block';
    }

    // --- close panel on click outside (only while open) ---
    function dq_click_outside_close(e) {
      try {
        if (!panelOpen) return;

        var t = e && e.target ? e.target : null;
        if (!t) return;

        // clicks inside panel shouldn't close it
        if (panel.contains(t)) return;

        // clicks on the toggle button (or its children) shouldn't close it
        if (toggleBtn && (toggleBtn === t || (toggleBtn.contains && toggleBtn.contains(t)))) return;

        panelOpen = false;
        panel.style.display = 'none';
        tipBox.style.display = 'none';
      } catch (err) { }
    }

    // use capture so we see the click even if plotly stops propagation
    document.addEventListener('mousedown', dq_click_outside_close, true);
    document.addEventListener('touchstart', dq_click_outside_close, true);

    // keep it responsive
    window.addEventListener('resize', function () {
      try {
        // auto-collapse on small screens
        var small = dq_is_small_screen();
        if (small && panelOpen) { panelOpen = false; panel.style.display = 'none'; tipBox.style.display = 'none'; }
        if (!small && !panelOpen) { /* keep user's choice; don't auto-open */ }
        positionPanel();
      } catch (e) { }
    });

    gd.__dqExUI = { panel: panel, input: inp, value: val, tip: tip, tipBox: tipBox };
  }

}

/**
 * Switches between different sections or pages based on the current URL hash. This function
 * handles showing and hiding specific elements, applying dynamic CSS styles, updating the navbar
 * state, and managing scrolling and layout adjustments on a single-page application (SPA).
 *
 * The function supports:
 * - Removing a description and triggering a click event based on the URL hash (e.g., `#desc_`).
 * - Updating the navbar by adding/removing a "target" class to the corresponding link.
 * - Adjusting layout and styles for small screen sizes and Firefox (for `:target` support).
 * - Animating scrolling to the current target element while considering the navbar height.
 * - Dynamically adjusting visibility of content sections based on the URL hash.
 *
 * @param {Event} event - The event object that triggered the page switch.
 * @returns {void}
 */
function single_page_switch(event) {
  $(function () {
    if (window.location.hash.startsWith("#desc_")) {
      if ($(".default-target").is(":visible")) {
        var pattern = "^#desc_";
        var re = new RegExp(pattern);
        var id = window.location.hash.replace(re, "");
        $("div#desc_" + id).remove();
        $("#" + id).trigger("click");
        if (window.__dqPersistPopupHistory) window.__dqPersistPopupHistory();
        window.location = window.location.hash;
      }
    }
    $(".navbar a").removeClass("target");
    // $( ".navbar a[href='" + window.location.hash + "']").addClass("target")
    $(".navbar a[href='" + $("dataquier-data:visible").attr("curr-url") + "']").addClass("target");

    $(function () {
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
        $(".navbar a[href='" + $("dataquier-data:visible").attr("curr-url") + "']").addClass("target");
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

      window.setTimeout(function () {
        sizeIframes(/* forceSetSizeToInit = */ false)
      }, 20)
      window.setTimeout(function () {
        try {
          $('html, body').animate({ scrollTop: $(document.getElementById(decodeURI(location.hash.replace("#", "")))).offset().top - $(".navbar").height() })
        } catch (e) {

        }
        try {
          setTimeout(relayout_after_reveiling_dom_parts, 150);
        } catch (e) {

        }
      }, 40)
      setTimeout(util_dt_adjust_all_now, 80);
    })
  })
  event.stopPropagation()
}

/**
 * Escapes special characters in a string for safe use in a RegExp.
 *
 * @param {string} s - Raw input string.
 * @returns {string} Escaped string safe for RegExp construction.
 */
function escapeRegExp(s) {
  return s.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
}

/**
 * @type {{ isReady: () => boolean }}
 */
const dataquieR = {
  isReady: function isDataquieReady() {
    return !$('#loading-ani').is(":visible");
  }
};

/**
 * add a floating menu to the single page's divs
 *
 * https://stackoverflow.com/a/5316785
 */
function float_menus() {
  if ($(window).scrollTop() >= 20 - $(".navbar").height()) {
    $(".floatbar").css({ position: 'fixed', left: '0px', top: $(".navbar").height() + 'px' });
  } else {
    $(".floatbar").css({ position: 'absolute', left: '0px', top: ($(".navbar").height() + 20) + 'px' });
  }
}

/**
 * Retrieves the Plotly window zoom level for a given plot.
 * If not already set, it initializes the zoom level from sessionStorage if available.
 *
 * @param {Object} gd - The Plotly graph div element.
 * @returns {number} The current zoom level of the Plotly graph.
 */
function getPlotlyWindowZoom(gd) {

  if (!gd.hasOwnProperty("plotlyScreenFit")) {
    gd.plotlyScreenFit = default_plotlyScreenFit;
    if (window.hasOwnProperty("sessionStorage")) {
      var plotlyScreenFit = JSON.parse(window.sessionStorage.getItem(
        "plotlyScreenFit." + gd.id));
      if (plotlyScreenFit != null) {
        gd.plotlyScreenFit = plotlyScreenFit;
      }
    }
  }
  return gd.plotlyScreenFit;
}

/**
 * Resets Plotly sunburst/drilldown state for a given element.
 * Clears any applied "level" restyle and removes persisted drilldown state
 * from the browser history (if present).
 *
 * @param {HTMLElement} el - The Plotly chart DOM element to reset.
 * @returns {void}
 */
function Plotly_dq_go_root(el) {

  try { Plotly.restyle(el, { level: "" }, [0]); } catch (e) { }
  // If you also store level in history state, clear it too:
  try { history.replaceState(Object.assign({}, history.state, { dq_sunburst_level: "" }), ""); } catch (e) { }
}

/**
 * Decodes an HTML-encoded string into a plain text string.
 * Uses the DOMParser to parse the input as HTML and retrieves the decoded text.
 *
 * https://stackoverflow.com/a/34064434
 *
 * @param {string} input - The HTML-encoded string to decode.
 * @returns {string} - The decoded plain text.
 */
function htmlDecode(input) {
  var doc = new DOMParser().parseFromString(input, "text/html");
  return doc.documentElement.textContent;
}

/**
 * Resizes a Plotly graph by updating its width and height based on its parent container.
 *
 * @param {string} resultId - The ID of the element containing the Plotly graph.
 */
function resize(resultId) {
  const cont = $(document.getElementById(resultId)).next()
  const py = cont.find(".js-plotly-plot")[0]
  const update = {
    width: $(py).parent().width(),
    height: $(py).parent().height()
  };
  Plotly.relayout(py, update);
}

/**
 * Handles the download of plot results, either as SVG or PNG based on context.
 * It checks for an iframe, Plotly graph, or image and initiates the download accordingly.
 *
 * @param {Event} event - The event object triggered by the context menu action.
 */
function dlResult() {
  let ctxArea = null;
  try {
    ctxArea = $(currentContextMenu.reference).attr("data-context-area");
  } catch (e) {
    console.log(e);
  }
  if (ctxArea == "plot" || ctxArea == "unknown") { // plot or 1-col-layout
    try {
      var iframes = $(currentContextMenu.reference).find("iframe")
      if (iframes.length == 1) {
        var iframe = iframes[0];
        var parent_nm = $(iframe).parents("div [data-nm]").attr("data-nm")
        sendMsgDownloadSVG(parent_nm)
        return;
      }
    } catch (e) {
      console.log(e)
    }
    try {
      var py = $(currentContextMenu.reference).find(".js-plotly-plot")[0]
      if (py != null) {
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
        Plotly.toImage(py, { format: 'svg' }).then(function (url) { download(id + ".svg", url) })
        return;
      }
    } catch (e) {
      console.log(e)
    }
    try {
      var imgTag = $(currentContextMenu.reference).find("img");
      if (imgTag.length > 0) {
        var downloadFromImg = function () {
          var id = $(currentContextMenu.reference).attr("data-nm");
          download(id + ".png", $(currentContextMenu.reference).find("img").attr("src"))
        }
        var downloadFromPlotly = function () {
          tglePy(imgTag[0]);
          setTimeout(function () {
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
        return;
      }
    } catch (e) {
      console.log(e)
    }
  }
  // not plot
  try {
    var el = $(currentContextMenu.reference).find("div.dt-buttons button.buttons-excel");
    if (el.length > 0) {
      el.click()
      return;
    }
  } catch (e) {
    console.log(e)
  }
  try {
    var id = $(currentContextMenu.reference).find("img").parent().prevAll("a[id]")[0].id
    download(id + ".png", $(currentContextMenu.reference).find("img")[0].src)
    return;
  } catch (e) {
    console.log(e)
  }
}

/**
 * Loads a text file (typically JSON) via an asynchronous XMLHttpRequest.
 * Calls the provided callback with the file contents once successfully loaded.
 *
 * // https://stackoverflow.com/a/34579496
 *
 * @param {string} file - Path or URL to the file to load.
 * @param {function(string): void} callback - Function invoked with the file content as a string.
 * @returns {void}
 */
function readTextFile(file, callback) {
  var rawFile = new XMLHttpRequest();
  rawFile.overrideMimeType("application/json");
  rawFile.open("GET", file, true);
  rawFile.onreadystatechange = function () {
    if (rawFile.readyState === 4 && rawFile.status == "200") {
      callback(rawFile.responseText);
    }
  }
  rawFile.send(null);
}

/**
 * Retained as a no-op for reports that still call the old Firefox table-cell shim.
 *
 * @returns {void}
 */
function fixTDforMozilla() {
}


/**
 * Adjusts iframe-based Plotly scaler elements to their initial or user-defined sizes.
 * Resets or preserves sizing depending on whether the user has manually resized the element.
 * Optionally forces reset to initial dimensions on first load or explicit resize actions.
 *
 * @param {boolean} [forceSetSizeToInit=false] - If true, resets scaler resize handles and reapplies initial dimensions even if sizes differ.
 * @returns {void}
 */
function sizeIframes(forceSetSizeToInit = false) {
  // $("[data-nm='con_hard_limits.SBP_0'] iframe").parent().css("resize")
  // $("[data-nm='con_hard_limits.SBP_0'] iframe").parent().css({"resize": "both"})
  // $("[data-nm='con_hard_limits.SBP_0'] iframe").parent().css({"resize": ""})
  const all_iframe_plots_scalers =
    $(document).find(".scaler[data-initialw][data-initialh] iframe").parent();
  const all_iframe_plots_result = all_iframe_plots_scalers.parent();
  all_iframe_plots_result.each(function (i, result_div) {
    const scaler_div = all_iframe_plots_scalers[i];
    if (forceSetSizeToInit) {
      // set size even if size differs from intial size -- for first call in $()
      // or a click to resize button in the plotly
      $(scaler_div).css({ "resize": "" }) // turn off scaler handle
    }
    if (!$(scaler_div).data("user-resized") && $(scaler_div).css("resize") != "both") { // auto sizing, now handle shown
      // set size only, if size has not been changed manually -- for resize calls
      const ih = $(scaler_div).attr("data-initialh").replace(/;+$/g, '');
      const iw = $(scaler_div).attr("data-initialw").replace(/;+$/g, '');

      $(scaler_div).css({ "width": iw })
      $(scaler_div).css({ "height": ih })
      $(scaler_div).height($(scaler_div).height()) // trigger resize events for potly
    }

  })
}

/**
 * Toggles a Plotly/iframe visualization by replacing a placeholder element with stored iframe HTML.
 * Restores the scaler’s initial dimensions unless the user has resized or navigation restore state is active.
 * Also triggers a delayed save of interactive state so UI controls (e.g., reset button) update immediately.
 *
 * @param {HTMLElement} et - The element triggering the toggle (containing stored iframe data).
 * @returns {void}
 */
function tglePy(et) {
  var iframeHTML = $(et).data('iframe');
  var scaler = $(et).parent("div.scaler");
  var initialh = scaler.attr("data-initialh");
  var initialw = scaler.attr("data-initialw");
  $(et).replaceWith(iframeHTML);
  if (scaler.length == 1) {
    var ds = scaler[0] && scaler[0].dataset ? scaler[0].dataset : null;
    // Do not overwrite restored / user-resized sizes
    if (!(ds && (ds.userResized === "1" || ds.navRestore === "1"))) {
      $(function () {
        scaler.css('width', initialw.replace(/;$/, ""));
        scaler.css('height', initialh.replace(/;$/, ""));
      });
    }
  }

  // Remember interactive state immediately so the reset icon can appear without reload.
  try { setTimeout(function () { try { if (typeof util_scaler_nav_save_now === "function") util_scaler_nav_save_now(); } catch (e) { } }, 0); } catch (e) { }
}

//#endregion

//#region content Menu

/**
 * Prevent dropdown onclick=showDescription(...) when clicking submenu links.
 *
 * Use capture phase so it runs BEFORE the parent's inline onclick.
 *
 * top menu "onclick=showDescription(...)" from firing when clicking submenu links
 */
function initDropdownHandlers() {
  document.addEventListener("click", function (e) {
    const a = e.target.closest && e.target.closest(".dropdown .dropdown-content a");
    if (!a) return;

    // Don't block navigation, only prevent parent menu handler.
    e.stopPropagation();
  }, true);

  document.addEventListener("keydown", function (e) {
    if (e.key !== "Enter") return;
    const a = e.target.closest && e.target.closest(".dropdown .dropdown-content a");
    if (!a) return;
    e.stopPropagation();
  }, true);

  $(document).on("click", ".dropdown .dropdown-content a", function (e) {
    e.stopPropagation();
  });
}

/**
 * Displays the description of a main menu entry. It handles updating the URL hash
 * to reflect the currently visible description, while managing history and the display logic.
 *
 * In narrow mode (screen width ≤ 1082px), the function uses the `:target` pseudo-class
 * to toggle visibility of the submenu without affecting the history.
 * In wide mode, it shows the description, adds it to the page, and updates the URL hash with history.
 *
 * @param {string} id - The ID of the main menu entry whose description is to be shown.
 * @param {string} description - The HTML-encoded description to display.
 */
function showDescription(id, description) {

  function setHashReplaceNoHistory(hash) {
    // hash: "" or "#Completeness"
    try {
      var base = window.location.pathname + window.location.search;
      window.location.replace(base + (hash || ""));
    } catch (e) {
      window.location.hash = (hash || "");
    }
  }

  if (window.matchMedia('screen and (max-width: 1082px)').matches) { // also adjust in CSS at line tagged with !!MEDIA!!
    // Toggle via :target (reliable) without adding history
    if (window.location.hash === ("#" + id)) {
      setHashReplaceNoHistory("");       // close
    } else {
      setHashReplaceNoHistory("#" + id); // open this menu
    }
    return;
  }

  function setHashNoHistory(hash) {
    // hash: "" or "#Completeness"
    try {
      var url = window.location.pathname + window.location.search + (hash || "");
      history.replaceState(history.state, "", url);
    } catch (e) {
      // fallback (may add history)
      window.location.hash = (hash || "");
    }
  }

  // Narrow mode: click should ONLY open/close the submenu
  if (window.matchMedia('screen and (max-width: 1082px)').matches) { // also adjust in CSS at line tagged with !!MEDIA!!
    var $nav = $(".navbar.responsive");
    var $dd = $("#" + id + ".dropdown");
    if (!$dd.length) return;

    // close others, toggle this one
    $nav.find(".dropdown.dq-dd-open").not($dd).removeClass("dq-dd-open");
    var willOpen = !$dd.hasClass("dq-dd-open");
    $dd.toggleClass("dq-dd-open", willOpen);

    // keep :target in sync without polluting history
    setHashNoHistory(willOpen ? ("#" + id) : "");

    return;
  }

  // Wide mode: create/show description and navigate to it (history is OK here)
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

  if (window.__dqPersistPopupHistory) window.__dqPersistPopupHistory();
  window.location.hash = "#desc_" + id;
}

/**
 * Displays a custom context menu when the user right-clicks on an element within a specified area (e.g., plot or table).
 * It determines whether the context menu should be shown for a plot or table cell and updates the context accordingly.
 *
 * The context menu is positioned based on the user's mouse click location.
 *
 * @param {Event} event - The mouse event triggered by the right-click action.
 */
function context_menu(event) {
  event.preventDefault();
  var tgt = event.currentTarget;

  // Remember where the right-click happened (plot vs table) on this result div.
  try {
    var ctxArea = "unknown";
    // Prefer hit-testing by coordinates (more robust than relying on event.target)
    try {
      if (document.elementsFromPoint) {
        var els = document.elementsFromPoint(event.clientX, event.clientY);
        for (var i = 0; i < els.length; i++) {
          var el = els[i];
          if (!el || !el.closest) continue;
          if (!tgt.contains(el) && el !== tgt) continue;
          if (el.closest(".dq-plot-cell")) { ctxArea = "plot"; break; }
          if (el.closest(".dq-table-cell")) { ctxArea = "table"; break; }
        }
      }
    } catch (e) {
      console.log(e);
    }
    // Fallback to event.target-based lookup
    if (ctxArea == "unknown" && event.target && event.target.closest) {
      if (event.target.closest(".dq-plot-cell")) {
        ctxArea = "plot";
      } else if (event.target.closest(".dq-table-cell")) {
        ctxArea = "table";
      }
    }
    tgt.setAttribute("data-context-area", ctxArea);
  } catch (e) {
    console.log(e);
    tgt.setAttribute("data-context-area", "unknown");
  }
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


/**
 * Represents the current context menu reference.
 *
 * please list corect type
 * @type {{reference: HTMLElement }|null}
 */
let currentContextMenu = null;

/**
 * Initializes stderr warning buttons and context menu tooltips
 * for all `.dataquieR_result` elements.
 * Adds a clickable warning icon (⚠) with a Tippy popup for stderr messages,
 * and attaches a custom context menu with clipboard + download actions.
 */
function initDataquieRResultTooltips() {
  $('div.dataquieR_result').each(function () {
    var stderr = this.getAttribute("data-stderr").trim();
    if (stderr != "") {
      // Plain tippy, open on click,
      // close on outside-click OR via "×" in the top-right of the popup.
      var btn1 = $('<input />', {
        type: 'button',
        value: '\u26A0',
        class: 'dq-warning-btn',
        'data-stderr': stderr
      });

      // Create a dedicated tippy for THIS button.
      (function attachStderrTippy(btnEl) {
        var msg = (btnEl.getAttribute('data-stderr') || '').trim();
        if (!msg) return;

        function mkContent(instance) {
          var $wrap = $('<div/>', { class: 'dq-stderr-pop' });

          var $head = $('<div/>', { class: 'dq-stderr-head' });
          var $title = $('<div/>', { class: 'dq-stderr-title', text: 'Messages' });

          var $close = $('<button/>', {
            type: 'button',
            class: 'dq-stderr-close',
            text: '×',
            'aria-label': 'Close'
          }).on('click', function (e) {
            e.preventDefault();
            e.stopPropagation();
            instance.hide();
          });

          $head.append($title, $close);

          var $body = $('<pre/>', {
            class: 'dq-stderr-body',
            text: msg
          });

          $wrap.append($head, $body);
          return $wrap[0];
        }

        tippy(btnEl, {
          trigger: 'click',
          hideOnClick: true,
          interactive: true,
          allowHTML: true,
          placement: 'right-start',
          arrow: false,
          offset: [0, 6],

          theme: 'dq-stderr',
          maxWidth: 'min(70vw, 720px)',

          onShow(instance) {
            instance.setContent(mkContent(instance));
          }
        });

        // Prevent click from bubbling to parent handlers (e.g. contextmenu or row click)
        $(btnEl).on('click', function (e) { e.stopPropagation(); });
      })(btn1[0]);
      $(this).prepend(btn1);
      $(this).prepend("<br />");
    }
  });
  tippy('div.dataquieR_result', {
    content(reference) {
      const call = $(reference).attr("data-call");
      const l = 30;
      const call_link = $("<button/>", {
        text: "Copy R-Call: " + (call.length > l + 3 ? call.substr(0, l) + "..." : call),
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
    },
    onShown(instance) {
      currentContextMenu = instance;
      var stderr =
        currentContextMenu.reference.getAttribute("data-stderr").trim()
      if (stderr != "") {
        // showTip(stderr)
      }
      instance._clipboard = new ClipboardJS($(instance.popper).find("button")[0])
      instance._clipboard.on('success', function (e) {
        showTip("Call Copied to Clipboard.");
        instance.hide(500);
      })
      instance._clipboard.on('error', function (e) {
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
  document.addEventListener("keydown", function (e) {
    if (e.key === "Escape") {
      dismissContextMenu()
    }
  });

}

/**
 * Hides any active Tippy.js context menus attached to dataquieR result elements.
 * Iterates over all ".dataquieR_result" nodes and calls `.hide()` on their Tippy instances if available.
 */
function dismissContextMenu() {
  document.querySelectorAll(".dataquieR_result").forEach(function (el) {
    if (el._tippy && typeof el._tippy.hide === "function") {
      el._tippy.hide();
    }
  });
}


//#endregion

//#region Nav

/**
 * Toggles the "responsive" class on the navbar when the user clicks on the icon,
 *
 * @param {Event} event - The click event that triggered the function.
 */
function toggleTopNav(event) {
  $(".navbar").first().toggleClass("responsive")
  event.stopPropagation()
}

/**
 * Hides the navbar by removing the "responsive" class when a click happens
 * outside of anchor (`<a>`) elements inside the navbar.
 *
 * @param {Event} event - The click event that triggered the function.
 */
function hideTopNav(event) {
  if (!(event.target.tagName.toLowerCase() === 'a')) {
    $(".navbar").first().removeClass("responsive")
    event.stopPropagation()
  }
}

//#endregion

//#region Search

/**
 * Tooltip utility class
 *
 * Displays contextual tooltips, such as the current search prompt
 */
class MessageTooltip {

  messenger = null;
  messengerHasMouse = false;
  mh = null;

  constructor() {
    $(document).on('mousemove', () => {
      this.messengerHasMouse = true;
    });

    // Create tippy instance correctly
    this.messenger = tippy('body', {
      arrow: true,
      hideOnClick: true,
      allowHTML: true,
      followCursor: "initial",
      interactive: true,
      trigger: 'manual',
      placement: 'top',
      content: '',
      popperOptions: {
        modifiers: [
          {
            name: 'preventOverflow',
            options: {
              padding: 12
            }
          }
        ]
      }
    })[0];

    const clip_inst = new ClipboardJS('.clipbtn');

    clip_inst.on('success', () => {
      this.showTip("Text Copied to Clipboard.");
    });
    clip_inst.on('error', (e) => {
      this.showTip("Error Copying Text to Clipboard.");
      console.log(e);
    });
  }

  /**
   * Displays a tooltip/message in the messenger UI.
   *
   * @param {string} msg - The message to display.
   * @param {number|null} [delay=10000] - Time in ms before auto-hiding the tip. If null, it stays visible.
   */
  showTip(msg, delay = 10000) {
    if (!this.messenger) {
      console.log("showTip called too early.");
      return;
    }

    this.messenger.setContent(msg);

    this.messenger.setProps({
      followCursor: this.messengerHasMouse ? "initial" : false
    });

    this.messenger.show();

    if (delay !== null) {
      this.mh = window.setTimeout(() => {
        this.messenger.hide();
      }, delay);
    }
  }

  /**
   * Clears any pending auto-hide timeout.
   */
  hideTip() {
    if (this.mh != null) {
      window.clearTimeout(this.mh);
      this.mh = null;
    }
  }
}
/**
 * @type {MessageTooltip}
 */
let messageTooltip;

/**
 * Creates a menu search controller that listens for keyboard input
 * and triggers navbar item highlighting.
 */
class MenuSearch {
  /**
 * Highlights matching menu items in the navbar based on a search pattern.
 * Optionally triggers a click on a single exact match.
 *
 * @param {string} pattern - Search text used to match menu items.
 * @param {boolean} [fire=false] - If true, auto-clicks a single exact match.
 */
  highlight_menu(pattern, fire) {
    pattern = escapeRegExp(pattern);
    try {
      var p = new RegExp(pattern, "i");
      var p_exact = new RegExp("^" + pattern + "$");
      var p_exact_ci = new RegExp("^" + pattern + "$", "i");

      if (this.highlight_cleanup_timer != null) {
        window.clearTimeout(this.highlight_cleanup_timer);
        this.highlight_cleanup_timer = null;
      }
      var $m = [];
      var patterns = [p_exact, p_exact_ci, p]; // order does matter, start strict, stepwise relax
      for (var i = 0; $m.length == 0 &&
        i < patterns.length; i++) {
        var pat = patterns[i];
        $m = $(".navbar .dropdown-content a").filter(function () {
          try {
            var alt_names = $(this).data("alternative-names");
            if (Array.isArray(alt_names) && alt_names.length > 0) {
              if (alt_names.some(function (name) {
                return typeof name === "string" && pat.test(name);
              })) {
                return true;
              }
            }
          } catch (e) {
            console.error(e);
          }
          return pat.test($(this).text());
        });
      }

      let vcnt = $(".navbar div.dropdown[id='SingleVariables'] .dropdown-content a").length;
      let mcnt = $(".navbar .dropdown-content a").length;

      if ($m.length === 0 || $m.length >= mcnt) return;

      if ($m.length === 1 && fire === true) {
        setTimeout(function () {
          $m[0].click();
        }, 500);
      }

      // --- narrow mode ---
      if (window.matchMedia && window.matchMedia('screen and (max-width: 1082px)').matches) {

        // ensure the responsive navbar is open (otherwise nothing is visible)
        var $nav = $(".navbar");

        $nav.addClass("dq-suspend-target");

        if ($nav.length && !$nav.hasClass("responsive")) {
          $nav.addClass("responsive");
        }

        // mark matches so: .navbar.responsive div:has(*.target) ... becomes active
        $m.addClass("target");

        // also (optional) open the dropdown(s) if you still use dq-dd-open
        $m.each(function () {
          $(this).closest(".dropdown")
            .addClass("dq-dd-open")
            .addClass("dq-dd-open-by-search");
        });

        // scroll first match into view (now it is actually visible)
        $m[0].scrollIntoView({ block: "nearest" });

        // highlight the matches (jQuery UI)
        $m.effect("highlight", {}, 4000);
        this.highlighted = true;

        this.highlight_cleanup_timer = window.setTimeout(function () {
          $m.removeClass("target");
          $nav.removeClass("dq-suspend-target");

          $(".dropdown.dq-dd-open-by-search")
            .removeClass("dq-dd-open dq-dd-open-by-search");

          this.highlighted = false;
          this.highlight_cleanup_timer = null;
        }, 4000);

        return;
      }

      // --- wide mode ---
      $m.parent().show();
      $m[0].scrollIntoView({ block: "nearest" });

      $m.effect("highlight", {}, 4000);
      this.highlighted = true;

      this.highlight_cleanup_timer = window.setTimeout(function () {
        $m.parent().css("display", "");
        this.highlighted = false;
        this.highlight_cleanup_timer = null;
      }, 4000);

    } catch (e) {
      console.log(e);
    }
  }

  highlight_timer = null;
  highlight_cleanup_timer = null;
  highlighted = false;

  /**
   * Initializes global keyboard listener for in-page search (Ctrl/Cmd+F style),
   * building a search string and triggering menu highlighting with delay.
   */
  constructor() {
    let search_string = "";
    let last_cf = 0;
    document.addEventListener("keydown", (e) => {
      if (document.activeElement instanceof HTMLInputElement) {
        return; // no auto-search, if user tries to fill an input field
      }
      const c_f = (e.ctrlKey || e.metaKey) && !e.shiftKey && e.which == 70;
      if (this.highlight_timer != null) {
        window.clearTimeout(this.highlight_timer);
        this.highlight_timer = null;
      }
      if (c_f && Date.now() - last_cf < 1200) { // last c-f pressed less than 1.2 s ago
        last_cf = 0;
        return;
      }
      this.highlight_timer = window.setTimeout(() => {
        if (search_string.length > 2) {
          this.highlight_menu(search_string);
          if (this.highlight_timer != null) {
            window.clearTimeout(this.highlight_timer);
            this.highlight_timer = null;
          }
        }
      }, 3000);
      e.preventDefault();
      if (c_f) {
        last_cf = Date.now();
        search_string = "";
        messageTooltip.hideTip();
        messageTooltip.showTip("Search: " + search_string);
      } else if (e.which == 8) {
        if (search_string.length > 0) {
          search_string = search_string.substr(0, search_string.length - 1);
          messageTooltip.hideTip();
          messageTooltip.showTip("Search: " + search_string);
        }
      } else if (e.which == 27) {
        search_string = "";
        messageTooltip.hideTip();
      } else if (e.which == 13) {
        if (this.highlighted) {
          if (this.highlight_timer != null) {
            window.clearTimeout(this.highlight_timer);
            this.highlight_timer = null;
          }
          this.highlight_menu(search_string, true);
          search_string = "";
          this.highlighted = false;
          messageTooltip.hideTip();
        } else {
          this.highlight_menu(search_string);
        }
      } else {
        if (e.key.length == 1) {
          search_string = search_string + e.key;
          messageTooltip.hideTip();
          messageTooltip.showTip("Search: " + search_string);
        }
      }
    });
  }
}
/**
 * @type {MenuSearch}
 */
let menuSearch;

//#endregion


//#region togglePlotlyWindowZoom

const min_pixels_per_tick = 40; // TODO: Depends on the kind of the plot
const max_pixels_per_tick = 100; // TODO: Depends on the kind of the plot
const min_width_in_em = 30;
const min_height_in_em = 15;
const def_height_in_vh = 50;
const def_width_in_vh = 70;
const default_plotlyScreenFit = false;

/**
 * Toggles the Plotly chart between normal size and "fit-to-screen" mode.
 *
 * This function:
 * - Initializes the `plotlyScreenFit` state on the graph div (`gd`) if not already set
 * - Restores the user's previous preference from sessionStorage (if available)
 * - Toggles the fit-to-screen mode on each call
 * - Persists the updated preference in sessionStorage
 * - Scrolls the parent container to the left when exiting fit-to-screen mode
 * - Triggers a UI resize via `sizeIframes`
 *
 * @param {HTMLElement} gd - The Plotly graph container element (graph div)
 */
function togglePlotlyWindowZoom(gd) { // FIXME: !! AND also FIXME: Make cursor positon nicer. FIXME: class_ReportSummaryTable test fails; FIXME: Merge and fix padding of Elena's stuff
  if (!gd.hasOwnProperty("plotlyScreenFit")) {
    gd.plotlyScreenFit = default_plotlyScreenFit;
    if (window.hasOwnProperty("sessionStorage")) {
      var plotlyScreenFit = JSON.parse(window.sessionStorage.getItem(
        "plotlyScreenFit." + gd.id));
      if (plotlyScreenFit != null) {
        gd.plotlyScreenFit = plotlyScreenFit;
      }
    }
  }
  if (!gd.plotlyScreenFit) {
    gd.parentElement.scrollTo({ left: 0 });
  }
  gd.plotlyScreenFit = !gd.plotlyScreenFit;
  if (window.hasOwnProperty("sessionStorage")) {
    window.sessionStorage.setItem("plotlyScreenFit." + gd.id,
      gd.plotlyScreenFit);
  }
  sizeIframes(/* forceSetSizeToInit = */ false);
}

//#endregion


//#region util_scaler_nav (util_nav)

/* --- util: restore scaler (thumbnail/iframe + size) on back/forward across documents --- */
let DQ_SCALER_NAV_PREFIX = "dq_scaler_nav_v1:";

/**
 * Determines the type of navigation that loaded the current page.
 *
 * @returns {"navigate"  | "back_forward"}
 */
function util_nav_type() {
  try {
    let nav = (performance && performance.getEntriesByType) ? performance.getEntriesByType("navigation") : [];
    if (nav && nav.length && nav[0] && nav[0].type) return nav[0].type;
  } catch (e) { }
  try {
    // deprecated but still present in some browsers
    if (performance && performance.navigation && typeof performance.navigation.type === 'number') {
      return (performance.navigation.type === 2) ? 'back_forward' : 'navigate';
    }
  } catch (e) { }
  return 'navigate';
}

/**
 * Checks whether the current page load was triggered by
 * browser back/forward navigation.
 *
 * @returns {boolean} True if navigation type is "back_forward", otherwise false.
 */
function util_nav_is_back_forward() {
  return util_nav_type() === 'back_forward';
}

/**
 * Returns a stable page key based on the current URL without the hash.
 * The hash is removed because it may change due to in-page anchors.
 *
 * @returns {string} The current page URL without the fragment identifier.
 */
function util_scaler_nav_page_key() {
  // keep hash out (hash can change due to in-page anchors)
  try { return String(location.href).split('#')[0]; } catch (e) { }
  return '';
}

/**
 * Builds a storage key for scaler navigation state using a global prefix
 * and the current page key.
 *
 * @returns {string} The computed storage key.
 */
function util_scaler_nav_storage_key() {
  return DQ_SCALER_NAV_PREFIX + util_scaler_nav_page_key();
}


/**
 * Returns a stable root URL for the current report/document.
 * This strips the filename and hash, leaving only the directory path.
 *
 * @returns {string} The root URL of the current document/report.
 */
function util_scaler_nav_report_root() {
  try {
    var u = new URL(String(location.href));
    // directory of current document; stable across hash changes
    return String(u.protocol) + "//" + String(u.pathname).replace(/\/[^\/]*$/, "/");
  } catch (e) { }
  try {
    var href = String(location.href).split('#')[0];
    return href.replace(/\/[^\/]*$/, "/");
  } catch (e) { }
  return '';
}

/**
 * Retrieves the current report ID from global rendering data if available.
 *
 * @returns {string} The report ID, or empty string if not available.
 */
function util_scaler_nav_current_report_id() {
  try {
    if (window.renderingData && window.renderingData.reportId) {
      var rid = String(window.renderingData.reportId);
      return rid ? rid : '';
    }
  } catch (e) { }
  return '';
}


/**
 * Reads the persisted scaler navigation state from window.name.
 * The state is expected to be prefixed and URI-encoded JSON.
 *
 * @returns {Object|null} Parsed state object, or null if invalid/not found.
 */
function util_scaler_nav_read_store() {
  try {
    var nm = String(window.name || '');
    if (nm.indexOf(DQ_SCALER_NAV_PREFIX) !== 0) return null;
    var enc = nm.slice(DQ_SCALER_NAV_PREFIX.length);
    var obj = JSON.parse(decodeURIComponent(enc));
    return obj && typeof obj === 'object' ? obj : null;
  } catch (e) { }
  return null;
}

/**
 * Writes scaler navigation state into window.name using a prefixed
 * URI-encoded JSON payload. Also triggers optional UI refresh callback.
 *
 * @param {Object} obj - State object to persist.
 */
function util_scaler_nav_write_store(obj) {
  try {
    var pack = JSON.stringify(obj);
    window.name = DQ_SCALER_NAV_PREFIX + encodeURIComponent(pack);
  } catch (e) { }
  // keep the cache-reset UI in sync
  try { if (window.util_scaler_nav_update_clear_button) window.util_scaler_nav_update_clear_button(); } catch (e2) { }
}

/**
 * Generates a stable identifier for a scaler element.
 * Prefers dataset ID, then iframe src, then image data-iframe, fallback index.
 *
 * @param {HTMLElement} el - Scaler container element.
 * @param {number} idx - Fallback index if no stable ID is found.
 * @returns {string} Stable identifier for the element.
 */
function util_scaler_nav_id_for(el, idx) {
  try {
    // Keep a stable id across thumbnail <img> and plotly <iframe> swaps.
    if (el && el.dataset && el.dataset.navId) return el.dataset.navId;

    // Prefer the plotly HTML target (iframe src).
    var ifr = el.querySelector && el.querySelector('iframe');
    if (ifr && ifr.getAttribute('src')) {
      var id1 = 'u:' + ifr.getAttribute('src');
      if (el && el.dataset) el.dataset.navId = id1;
      return id1;
    }

    // If still a thumbnail, prefer the embedded iframe target from data-iframe.
    var img = el.querySelector && el.querySelector('img');
    if (img) {
      var di = img.getAttribute && img.getAttribute('data-iframe');
      if (di && typeof di === 'string') {
        // data-iframe is usually HTML-escaped (contains &quot;).
        var mm = di.match(/src=(?:&quot;([^&]+)&quot;|"([^"]+)")/);
        var src = mm ? (mm[1] || mm[2]) : null;
        if (src) {
          var id2 = 'u:' + src;
          if (el && el.dataset) el.dataset.navId = id2;
          return id2;
        }
      }
      if (img.getAttribute && img.getAttribute('src')) {
        var id3 = 'u:' + img.getAttribute('src');
        if (el && el.dataset) el.dataset.navId = id3;
        return id3;
      }
    }
  } catch (e) { }
  // fallback: DOM order
  return 'i:' + String(idx);
}



/**
 * Collects all scaler elements that have meaningful state changes
 * (iframe presence or user resizing) and serializes their state.
 *
 * @returns {Array<Object>} List of scaler state objects.
 */
function util_scaler_nav_collect() {
  var out = [];
  var els = document.querySelectorAll ? document.querySelectorAll('div.scaler') : [];
  for (var i = 0; i < els.length; i++) {
    var el = els[i];
    if (!el) continue;
    var hasIframe = false;
    try { hasIframe = !!(el.querySelector && el.querySelector('iframe')); } catch (e) { }
    var userResized = false;
    try { userResized = (el.dataset && el.dataset.userResized === '1'); } catch (e) { }

    // store only if anything meaningful happened
    if (!hasIframe && !userResized) continue;

    out.push({
      k: util_scaler_nav_id_for(el, i),
      w: (el.style && el.style.width) ? el.style.width : '',
      h: (el.style && el.style.height) ? el.style.height : '',
      ifr: hasIframe ? 1 : 0,
      ur: userResized ? 1 : 0
    });
  }
  return out;
}

/**
 * Applies a single saved scaler state to a DOM element.
 * Restores iframe and size if previously persisted.
 *
 * @param {HTMLElement} el - Target scaler element.
 * @param {Object} st - Saved state object.
 */
function util_scaler_nav_apply_one(el, st) {
  if (!el || !st) return;

  // ensure iframe is present if it was interactive before
  if (st.ifr) {
    try {
      var hasIframe = !!(el.querySelector && el.querySelector('iframe'));
      if (!hasIframe) {
        var img = el.querySelector && el.querySelector('img[data-iframe]');
        if (img) {
          // mark restore so tglePy won't reset to initial cm sizes
          try { el.dataset.navRestore = '1'; } catch (e) { }
          // if a concrete size was stored, treat it as user-resized for restore
          if (st.w || st.h || st.ur) {
            try { el.dataset.userResized = '1'; } catch (e) { }
          }
          tglePy(img);
          try { delete el.dataset.navRestore; } catch (e) { }
        }
      }
    } catch (e) { }
  }

  // restore px sizes (only when they exist)
  try {
    if (st.w) el.style.width = st.w;
    if (st.h) el.style.height = st.h;
  } catch (e) { }

  // mark user-resized (keeps your existing logic intact)
  if (st.ur) {
    try { el.dataset.userResized = '1'; } catch (e) { }
  }
}

/**
 * Applies a full saved payload to all matching scaler elements on the page.
 *
 * @param {Array<Object>} payload - Array of saved scaler states.
 */
function util_scaler_nav_apply(payload) {
  if (!payload || !payload.length) return;
  var map = new Map();
  for (var i = 0; i < payload.length; i++) {
    var st = payload[i];
    if (st && st.k) map.set(st.k, st);
  }
  var els = document.querySelectorAll ? document.querySelectorAll('div.scaler') : [];
  for (var j = 0; j < els.length; j++) {
    var el = els[j];
    var k = util_scaler_nav_id_for(el, j);
    if (map.has(k)) util_scaler_nav_apply_one(el, map.get(k));
  }
}

/**
 * Saves current scaler state for the current page into persistent storage.
 * Prevents saving during disabled-save states.
 */
function util_scaler_nav_save_now() {
  try {
    // If the user explicitly cleared the cache, avoid re-populating it during
    // the unload sequence (beforeunload/visibilitychange/pagehide).
    if (window && window.dqScalerNavDisableSave) return;

    var payload = util_scaler_nav_collect();
    var pageKey = util_scaler_nav_page_key();
    var rootKey = util_scaler_nav_report_root();

    var store = util_scaler_nav_read_store();
    if (!store || store.r !== rootKey || !store.pages) {
      store = { v: 1, r: rootKey, id: util_scaler_nav_current_report_id(), pages: {} };
    }
    var rid = util_scaler_nav_current_report_id();
    if (rid) store.id = rid;
    store.pages[pageKey] = payload;
    util_scaler_nav_write_store(store);
  } catch (e) { }
}

/**
 * Loads persisted scaler data for the current page, if available and valid.
 *
 * @returns {{u: string, p: Array<Object>}|null} Loaded page pack or null.
 */
function util_scaler_nav_load_pack() {
  var pageKey = util_scaler_nav_page_key();
  var rootKey = util_scaler_nav_report_root();

  try {
    var store = util_scaler_nav_read_store();
    if (!store || store.r !== rootKey || !store.pages) return null;
    var rid = util_scaler_nav_current_report_id();
    if (rid && !store.id) return null;
    if (rid && store.id && store.id !== rid) return null;
    if (!store.pages.hasOwnProperty(pageKey)) return null;
    return { u: pageKey, p: store.pages[pageKey] };
  } catch (e) { }
  return null;
}

/**
 * Restores scaler layout state if a saved pack exists.
 */
function util_scaler_nav_restore_if_needed() {
  var pack = util_scaler_nav_load_pack();
  if (!pack || !pack.p) return;
  util_scaler_nav_apply(pack.p);
}


/**
 * Removes a persisted scaler entry by iframe/image source URL.
 *
 * @param {string} src - Source URL of the scaler content.
 */
function util_scaler_nav_delete_one_by_src(src) {
  try {
    if (!src) return;

    var pageKey = util_scaler_nav_page_key();
    var rootKey = util_scaler_nav_report_root();

    var store = util_scaler_nav_read_store();
    if (!store || store.r !== rootKey || !store.pages || !store.pages[pageKey]) return;

    var k = 'u:' + String(src);
    var arr = store.pages[pageKey];
    if (!Array.isArray(arr) || !arr.length) return;

    var next = [];
    for (var i = 0; i < arr.length; i++) {
      var it = arr[i];
      if (!it || it.k !== k) next.push(it);
    }
    store.pages[pageKey] = next;
    util_scaler_nav_write_store(store);
  } catch (e) { }
}

/**
 * Restores a thumbnail view for a named scaler element, replacing iframe
 * content with an image and resetting persisted state.
 *
 * @param {string} nm - Name/data-nm identifier of the scaler element.
 * @returns {boolean} True if restoration succeeded, otherwise false.
 */
function util_scaler_restore_thumbnail_for_nm(nm) {
  try {
    if (!nm) return false;

    var $ifr = $('div[data-nm="' + nm + '"] iframe');
    if (!$ifr.length) return false;

    var ifr = $ifr[0];
    var src = ifr.getAttribute('src') || '';
    if (!src) return false;

    var $scaler = $ifr.closest('div.scaler');
    if (!$scaler.length) return false;

    var scaler = $scaler[0];
    var initialh = $scaler.attr("data-initialh");
    var initialw = $scaler.attr("data-initialw");

    // Remove any persisted nav-size state for this figure on this page.
    util_scaler_nav_delete_one_by_src(src);

    // Reset scaler metadata so later tglePy() won't treat it as user-resized/restored.
    try { delete scaler.dataset.userResized; } catch (e) { }
    try { delete scaler.dataset.navRestore; } catch (e) { }
    try { delete scaler.dataset.navId; } catch (e) { }
    $scaler.removeAttr("data-user-resized data-nav-restore data-nav-id");

    // Reset size to initial (as on first load).
    if (initialw) $scaler.css("width", String(initialw).replace(/;$/, ""));
    if (initialh) $scaler.css("height", String(initialh).replace(/;$/, ""));

    // Build a fresh thumbnail <img> that can be toggled back to iframe on click.
    var png = src.replace(/\.html(?:[#?].*)?$/, ".png");
    if (png === src) png = src + ".png";

    var iframeHTML = '<iframe src="' + src + '" style="border:0;width:100%;height:calc(100% - 5px);"></iframe>';

    var img = document.createElement("img");
    img.setAttribute("src", png);
    img.setAttribute("style", "margin:0px;width:100%;height:100%;object-fit:scale-down;object-position: left top;cursor:zoom-in;display:block;");
    img.setAttribute("onmouseup", "tglePyHandler()");
    img.setAttribute("data-iframe", iframeHTML);

    // Replace the iframe with the thumbnail.
    ifr.parentNode.replaceChild(img, ifr);

    // Best-effort: trigger any layout recalcs
    try { sizeIframes(/* forceSetSizeToInit = */ false); } catch (e) { }

    return true;
  } catch (e) { }
  return false;
}

/**
 * Registers navigation lifecycle handlers for saving and restoring scaler state.
 *
 * - Saves state on page exit (visibilitychange, beforeunload, pagehide)
 * - Restores state when page is shown again (pageshow, incl. back/forward cache)
 *
 */
function init_util_scaler_nav() {

  // Save state when leaving the page (tab switch, close, navigation)
  document.addEventListener("visibilitychange", function () {
    if (document.visibilityState === "hidden") {
      try { util_scaler_nav_save_now(); } catch (e) { }
    }
  });

  window.addEventListener("beforeunload", function () {
    try { util_scaler_nav_save_now(); } catch (e) { }
  });

  // Extra safety for some mobile browsers / BFCache edge cases
  window.addEventListener("pagehide", function () {
    try { util_scaler_nav_save_now(); } catch (e) { }
  });

  // Restore state when returning via back/forward cache or reload
  window.addEventListener("pageshow", function () {
    try { util_scaler_nav_restore_if_needed(); } catch (e) { }
  });
}

//#endregion

//#region DtLayoutRebuilder

/**
 *  Debounced DataTables layout adjust in one or more result containers
 */
class DtLayoutRebuilder {
  constructor(delay = 80) {
    this.delay = delay;
    this.timer = null;
    this.pending = new Set();
  }

  schedule(resultDiv) {
    try {
      if (!resultDiv) return;

      this.pending.add(resultDiv);

      if (this.timer) clearTimeout(this.timer);

      this.timer = setTimeout(() => {
        try {
          this.pending.forEach((container) => {
            adjustDtInResult(container);
          });
        } catch (e) {
          console.log("DT adjust failed:", e);
        } finally {
          this.pending.clear();
        }
      }, this.delay);

    } catch (e) {
      console.log(e);
    }
  }
}
let dtLayoutRebuilder;

/** --- util: last-wins DT adjust (prevents resize race between header/body) --- */
function util_dt_schedule_safe_adjust(dt) {
  try {
    if (!dt || !dt.table) return;

    var node = dt.table().node();
    if (!node) return;

    // last-wins token per table
    node.__dqDtAdjTok = (node.__dqDtAdjTok || 0) + 1;
    var tok = node.__dqDtAdjTok;

    // cancel pending run
    if (node.__dqDtAdjTimer) {
      cancelAnimationFrame(node.__dqDtAdjTimer);
      node.__dqDtAdjTimer = null;
    }

    // two frames: let CSS breakpoint + scrollbars settle
    node.__dqDtAdjTimer = requestAnimationFrame(function () {
      requestAnimationFrame(function () {
        // if a newer request happened: abort
        if (node.__dqDtAdjTok !== tok) return;

        try {
          dt.columns.adjust();

          if (dt.responsive) dt.responsive.recalc();
          if (dt.fixedColumns && dt.fixedColumns().relayout) dt.fixedColumns().relayout();

          // draw can change scrollbars → do it once, then re-adjust widths
          dt.draw(false);

          dt.columns.adjust();
          if (dt.fixedColumns && dt.fixedColumns().relayout) dt.fixedColumns().relayout();
          if (dt.fixedHeader) dt.fixedHeader.adjust();

          // final “belt & suspenders”: keep scrollHead/table width in sync with body table
          var $wrap = $(dt.table().container());
          var $bodyTbl = $wrap.find('.dataTables_scrollBody table.dataTable').first();
          var $headInner = $wrap.find('.dataTables_scrollHeadInner').first();
          var $headTbl = $wrap.find('.dataTables_scrollHead table.dataTable').first();

          if ($bodyTbl.length && $headInner.length && $headTbl.length) {
            var w = $bodyTbl.outerWidth();
            if (w && isFinite(w) && w > 0) {
              $headInner.css('width', w + 'px');
              $headTbl.css('width', w + 'px');
            }
          }
        } catch (e) {
          // ignore per-table failures
        }
      });
    });
  } catch (e) { }
}

/** Just adjust column widths / responsive / fixed header in all tables in a result */
function adjustDtInResult(container) {
  try {
    if (!container) return;
    var $container = $(container);

    $container.find("table.myDT").each(function (_, tbl) {
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
        util_dt_schedule_safe_adjust(dt);

      } catch (e) {
        // table not initialised yet; ignore
      }
    });
  } catch (e) {
    console.log("adjustDtInResult failed:", e);
  }
}


//#endregion


//#region Scaler

/**
 * Trims a CSS value string and removes a trailing semicolon.
 * @param {string} x
 * @returns {string}
 */
function cssLen(x) {
  return String(x).trim().replace(/;$/, "");
}

/**
 * Normalizes a CSS length value and appends "px" if unit is missing.
 * @param {string|number} x
 * @returns {string}
 */
function util_scaler_norm_len(x) {
  const s = cssLen(x);
  if (!s) return "";
  // If unit is missing, assume px.
  if (/^[+-]?(?:\d+\.?\d*|\.\d+)$/.test(s)) return s + "px";
  return s;
}

/**
 * Resolves and caches an element's initial pixel width/height from data attributes.
 * Returns null if values cannot be computed or element is not measurable.
 * @param {HTMLElement} el
 * @returns {{w: number, h: number} | null}
 */
function util_scaler_get_initial_px(el) {
  try {
    if (el.__dqInitialPx && el.__dqInitialPx.w > 0 && el.__dqInitialPx.h > 0) return el.__dqInitialPx;

    var w0 = util_scaler_norm_len(el.dataset ? el.dataset.initialw : el.getAttribute("data-initialw"));
    var h0 = util_scaler_norm_len(el.dataset ? el.dataset.initialh : el.getAttribute("data-initialh"));
    if (!w0 || !h0) return null;

    // If hidden (display:none), defer.
    var r = el.getBoundingClientRect();
    if (!(r.width > 0 && r.height > 0)) return null;

    var probe = document.createElement("div");
    probe.style.position = "absolute";
    probe.style.visibility = "hidden";
    probe.style.left = "-100000px";
    probe.style.top = "0";
    probe.style.width = w0;
    probe.style.height = h0;

    // Percent lengths depend on the parent; attach near the element.
    var host = el.parentElement || document.body;
    host.appendChild(probe);
    var pr = probe.getBoundingClientRect();
    probe.remove();

    if (!(pr.width > 0 && pr.height > 0)) return null;

    el.__dqInitialPx = { w: pr.width, h: pr.height };
    return el.__dqInitialPx;
  } catch (e) {
    return null;
  }
}

/**
 * Checks whether an element's current size differs from its initial cached size.
 * @param {HTMLElement} el
 * @param {number} [tolerancePx=3]
 * @returns {boolean}
 */
function scalerIsResized(el, tolerancePx = 3) {
  const r = el.getBoundingClientRect();
  if (r.width <= 0 || r.height <= 0) return false; // hidden => check later

  const init = util_scaler_get_initial_px(el);
  if (!init) return false;

  return (
    Math.abs(r.width - init.w) > tolerancePx ||
    Math.abs(r.height - init.h) > tolerancePx
  );
}

/**
 * Handles left-click events and triggers tglePy on the event target.
 * @returns {false|undefined}
 */
function tglePyHandler() {
  if (event.which == 1) {
    tglePy(event.target)
    return false;
  }
}

/**
 * Installs a lightweight detector to flag user-initiated resize attempts
 * when pointer/mouse events occur in the bottom-right "resize handle" area.
 * Ensures installation runs only once per element.
 *
 * @param {HTMLElement} el
 * @param {number} [handlePx=24] - Size of the active resize corner area in pixels.
 * @returns {void}
 */
function util_scaler_install_user_resize_detector(el, handlePx = 24) {
  if (el.__dqUserResizeInstalled) return;
  el.__dqUserResizeInstalled = true;

  function armIfHandle(ev) {
    try {
      if (ev.button !== undefined && ev.button !== 0) return;
      const r = el.getBoundingClientRect();
      if (r.width <= 0 || r.height <= 0) return;
      const x = ev.clientX - r.left;
      const y = ev.clientY - r.top;
      if (x >= (r.width - handlePx) && y >= (r.height - handlePx)) {
        el.__dqResizeArmed = true;
      }
    } catch (e) { }
  }

  // Arm "manual resize" only if the pointer down happens in the bottom-right corner
  // (where the native CSS resize handle lives).
  if (window.PointerEvent) {
    el.addEventListener("pointerdown", armIfHandle, { passive: true });
  } else {
    el.addEventListener("mousedown", armIfHandle, { passive: true });
  }
}

const initializedScalerDivs = new WeakSet();

/**
 * Handles ResizeObserver entries for scaler elements.
 * Detects user-initiated resize actions and conditionally triggers iframe conversion logic.
 *
 * @param {ResizeObserverEntry[]} entries
 */
function resize_scaler_div(entries) { // if resize-handle was clicked, makt it an iframe
  for (const entry of entries) {
    const scalerdiv = entry.target;

    // If hidden (display:none or not laid out yet), skip and let RO call again later.
    const curW = entry.contentRect && entry.contentRect.width ? entry.contentRect.width : 0;
    const curH = entry.contentRect && entry.contentRect.height ? entry.contentRect.height : 0;
    if (curW <= 0 || curH <= 0) continue;

    if (!initializedScalerDivs.has(scalerdiv)) {
      initializedScalerDivs.add(scalerdiv);
      util_scaler_install_user_resize_detector(scalerdiv);
      continue; // skip first (initial) call
    }

    if (scalerdiv.__dqResizeArmed) {
      scalerdiv.__dqResizeArmed = false;
      try { scalerdiv.dataset.userResized = "1"; } catch (e) { }
    }

    if (dataquieR.isReady()) { // not initial sizing
      if (window.dataquieR_single_result === true) return;
      const et = $(scalerdiv).find("img[data-iframe]");
      if (et.length === 1) {
        // scalerIsResized must also tolerate "hidden" cases; but we already filtered >0 above.
        if (scalerdiv.dataset && scalerdiv.dataset.userResized === "1") {
          tglePy(et[0]);
        }
      }
    }
  }
}
/**
 * Initializes a ResizeObserver for all div.scaler elements
 * and attaches the resize handler.
 *
 * https://stackoverflow.com/a/66487907
 */
function initResize_scaler_divResizeObserver() {
  let observer = new ResizeObserver(resize_scaler_div);
  $("div.scaler").each(function (i, obj) {
    observer.observe(obj);
  });
}

//#endregion

//#region scalerResizeObserver

/**
 * @type {ResizeObserver}
 */
let scalerResizeObserver;

/**
 * Initializes a ResizeObserver to monitor scaler elements and update layouts dynamically.
 * When a scaler resizes, this updates the associated plot-table layout and schedules
 * a DataTable layout rebuild. Also attaches a window resize handler to trigger a full relayout.
 *
 * @returns {void}
 */
function initScalerResizeObserver() {
  scalerResizeObserver = new ResizeObserver((entries) => {
    for (const entry of entries) {
      const scaler = entry.target;
      const resultDiv = scaler.closest("div.dataquieR_result");
      if (!resultDiv) continue;

      // update plot/table layout for this scaler
      updatePlotTableLayoutForScaler(scaler);

      // debounce rebuilding/adapting datatables
      dtLayoutRebuilder.schedule(resultDiv);
    }
  });


  $("div.scaler").each(function (i, obj) {
    scalerResizeObserver.observe(obj);
  });
  // Also react to browser window resizes
  $(window).on("resize", relayout_after_reveiling_dom_parts);
}

/**
 * Updates the layout of a plot-table container based on the available space.
 * This function checks if the table within the container overflows or if the plot occupies too much space,
 * and adjusts the layout by stacking the table on top if necessary.
 *
 * @param {HTMLElement} scaler_div - The DOM element that triggers the layout update (e.g., a plot or scaler control).
 *
 * @returns {void}
 */
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
    console.error(e);
  }
}

/**
 * Recomputes the layout for all relevant plot-table results and DataTables after revealing DOM parts.
 * This function loops through each plot-table result and applies the layout adjustments
 * for the plot and table grids, and also schedules a layout rebuild for the parent result container.
 * It also handles "plain" DataTables that are not part of the plot-table result.
 *
 * @returns {void}
 */
function relayout_after_reveiling_dom_parts() {
  // For each plot+table result, recompute layout based on available space
  $(".dataquieR_result.dq-plot-table-result div.scaler").each(function (_, scaler) {
    updatePlotTableLayoutForScaler(scaler);
    let resultDiv = scaler.closest("div.dataquieR_result");
    if (resultDiv) {
      dtLayoutRebuilder.schedule(resultDiv);
    }
  });

  // Also handle "plain" DataTables (not part of dq-plot-table-result)
  $("table.myDT").each(function () {
    var resultDiv = this.closest("div.dataquieR_result");
    if (resultDiv) dtLayoutRebuilder.schedule(resultDiv);
  });
}

//#endregion


//#region adjust_all

/**
 * Recalculates all visible DataTables (columns, FixedHeader, Responsive) and redraws them.
 */
function util_dt_adjust_all_now() { // TODO: This may obsolete some other recalcuations
  if (!$.fn.dataTable || !$.fn.dataTable.settings) return;

  $.each($.fn.dataTable.settings, function () {
    try {
      var api = new $.fn.dataTable.Api(this);

      // Only touch visible tables (avoid hidden tabs/accordions unless you want them too)
      var node = api.table().node();
      if (!node) return;
      if (!$(node).is(":visible")) return;

      api.columns.adjust();

      // FixedHeader (most commonly stored here)
      if (this._fixedHeader && typeof this._fixedHeader.adjust === "function") {
        this._fixedHeader.adjust();
      } else if (api.fixedHeader && typeof api.fixedHeader.adjust === "function") {
        // some builds expose it on the API
        api.fixedHeader.adjust();
      }

      // Responsive (if enabled)
      if (this.responsive && typeof this.responsive.recalc === "function") {
        this.responsive.recalc();
      } else if (api.responsive && typeof api.responsive.recalc === "function") {
        api.responsive.recalc();
      }

      api.draw(false);
    } catch (e) { }
  });
}

/**
 * Runs DataTable adjustments on pageshow (handles bfcache restore).
 */
function init_adjust_all(){
  window.addEventListener("pageshow", function () {
  requestAnimationFrame(function () {
    setTimeout(util_dt_adjust_all_now, 80);
  });
});
}

//#endregion
