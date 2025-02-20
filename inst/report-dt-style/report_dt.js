 // https://stackoverflow.com/a/8747204
jQuery.expr[':'].icontains = function(a, i, m) {
  return jQuery(a).text().toUpperCase()
      .indexOf(m[3].toUpperCase()) >= 0;
};

window.dtConfig = {}

search_curr_colvis = function(event) {
  var evt=event||window.event;
  var cur_colvis = $(evt.target).parent();
  var input_el = cur_colvis.find("input.search_curr_colvis_input")
  var search_text = $(input_el).val();
  cur_colvis.find("button.buttons-columnVisibility").css("backgroundColor", "");
  cur_colvis.find("button.buttons-columnVisibility:icontains(" + search_text + ")").css(
    "backgroundColor", "#cccc55");
  if (evt instanceof KeyboardEvent) {
    if (evt.key == "Enter") {
      cur_colvis.find("button.buttons-columnVisibility:icontains(" + search_text + ")").click()
    }
  } else if (evt instanceof PointerEvent || evt instanceof MouseEvent) {
    cur_colvis.find("button.buttons-columnVisibility:icontains(" + search_text + ")").click()
  }
}

// https://stackoverflow.com/a/77266386
is_html = function(content) {
    let elem = document.createElement('p');
    elem.innerHTML = content;
    let res=elem.children.length > 0;
    elem = null;
    return(res);
}

escape_html = function(content) {
  var text = $("<div/>").html(content).text();
  return text;
}

function isNumeric(str) {
  str = str.replaceAll(/\s+/g, "")
  return str == "NaN" || !isNaN(Number(str))
}

sort_vert_dt = function(data, type, row, meta) {
  /* https://www.pierrerebours.com/2017/09/custom-sorting-with-dt.html */
     if (type == 'filter') {
        var string = data;
        var regex = new RegExp('[\\s\\r\\t\\n]*([a-z0-9\\-_]+)[\\s\\r\\t\\n]*=[\\s\\r\\t\\n]*([\'"])((?:\\\\\\2|(?!\\2).)*)\\2', 'ig');
        var attributes = {};
        while ((match = regex.exec(string))) {
            attributes[match[1]] = match[3];
        }
        if (attributes.hasOwnProperty("filter")) {
          return(attributes.filter);
        } else {
          if (is_html(data)) data = escape_html(data);
          return(data) // if html formated, remove this format, first
        }
     } else if (type == 'sort') {
        var tb = new $.fn.dataTable.Api( meta.settings ).table()
        var id = $(tb.container()).attr("id");
        var cfg = window.dtConfig[id]
        if (!cfg) {
          cfg = {}
        }
        var column = tb.column(meta.col).header().textContent
        if (cfg.grading_cols && cfg.grading_cols.includes(column) && cfg.grading_order) {
          if (cfg.secondary_order && cfg.secondary_order[column]) {
            var basis = 0.0;
            for (var i = 0; i < cfg.secondary_order[column].length; i++) {
              if (cfg.grading_cols.indexOf(colName) >= 0) { // column is a grading column
                var basis = basis + cfg.grading_order.length-1
              } else {
                var basis = basis + tb.rows().count()
              }
            }
            while (basis > 2 ** 16) {
              basis = basis / 2
            }
            res = 0.0;
            for (var i = 0; i < cfg.secondary_order[column].length; i++) {
              var cur_col_idx = cfg.secondary_order[column][i];
              var colName = tb.column(cur_col_idx).header().textContent
              if (cfg.grading_cols.indexOf(colName) >= 0) { // column is a grading column
                var cur_order = cfg.grading_order.indexOf(row[cur_col_idx])
                if (cur_order < 0) {
                  cur_order = cfg.grading_order.length;
                }
              } else {
                var colCnt = tb.column(cur_col_idx).data().toArray()
                var cur_order = colCnt.sort().indexOf(row[cur_col_idx])
              }
              res = res * basis + cur_order;
            }

            return (1*res).toString().padStart(50, '0');
          }
          return(cfg.grading_order.indexOf(data))
        }
        var string = data;
        var regex = new RegExp('[\\s\\r\\t\\n]*([a-z0-9\\-_]+)[\\s\\r\\t\\n]*=[\\s\\r\\t\\n]*([\'"])((?:\\\\\\2|(?!\\2).)*)\\2', 'ig');
        var attributes = {};
        while ((match = regex.exec(string))) {
            attributes[match[1]] = match[3];
        }
        if (attributes.hasOwnProperty("sort")) {
          return(1 * attributes.sort);
        } else {
          if (is_html(data)) data = escape_html(data);
          var num_with_pct = new RegExp('^\\s*([0-9\\.]+)\\s+\\([0-9\\.]+%?\\)\\s*$');
          if (match = num_with_pct.exec(data)) {
            return(Number(match[[1]]))
          }

          return(data) // if html formated, remove this format, first
        }
     } else if (type == 'type') {
        var tb = new $.fn.dataTable.Api( meta.settings ).table()
        var cfg = window.dtConfig[$(tb.node()).closest(".datatables").attr("id")]
        if (!cfg) {
          cfg = {}
        }
        var column = tb.column(meta.col).header().textContent
        if (cfg.grading_cols && cfg.grading_cols.includes(column) && cfg.grading_order) {
          return "xx"; // 1.0
        }

        var string = data + "";
        string = string.trim()
        var regex = new RegExp('[\\s\\r\\t\\n]*([a-z0-9\\-_]+)[\\s\\r\\t\\n]*=[\\s\\r\\t\\n]*([\'"])((?:\\\\\\2|(?!\\2).)*)\\2', 'ig');
        var attributes = {};
        while ((match = regex.exec(string))) {
            attributes[match[1]] = match[3];
        }
        var num_with_pct = new RegExp('^\\s*([0-9\\.]+)\\s+\\([0-9\\.]+%?\\)\\s*$');
        if (match = num_with_pct.exec(data)) {
          return(Number(match[[1]]))
        }
        if (isNumeric(string.replace(/\s*%\s*$/, ""))) {
          return(Number(string.replace(/\s*%\s*$/, "")))
        }
        return(data)
     } else {
       return(data);
     }
}

// Convert filter widgets for all traffic lights to drop-downs:
// https://stackoverflow.com/a/28625937
$(function() {
/*
  var availableQS = [
    "",
    "grey",
    "green",
    "yellow",
    "red"
  ];
*/

  $(".matrixTable .dataTables_filter").hide();

  $(".matrixTable thead td input[type=search]:not(:first)").autocomplete({
//    source: availableQS,
   source: function(req, res) {
          let colIdx = this.element.parent().parent().index()
          let cats = $($(this.element.closest(".datatables").find("table")[1]).DataTable().column(colIdx).cells().nodes()).find("pre").map(function(x) { return $(this).attr("filter"); })
          cats = Array.from(new Set(cats)).sort(); // requires ES6, https://stackoverflow.com/a/36270406
          // https://stackoverflow.com/a/2405109
          let re = $.ui.autocomplete.escapeRegex(req.term);
          let matcher = new RegExp( "^" + re, "i" );
          let a = $.grep( cats, function(item, index){
          return matcher.test(item);
      });
      res(a)
    },
    select: function(event, ui) {
          let colIdx = $(this).parent().parent().index();
          //let tName = $(this).closest(".html-widget").find("table[aria-describedby]").first()[0].
          //    getAttribute("aria-describedby").replaceAll("_info", "");
          //let oTable = new $.fn.dataTable.Api("#" + tName)
          let oTable = $($(this).closest(".datatables").find("table")[1]).DataTable();
          let that = $(this);
          let my_value = ui.item.value;
          that.val(my_value);
          $(function() {
            oTable.column(colIdx).search(my_value).draw();
          })
          return true;
    }
  });
});

// XLSX with colors inspired by https://stackoverflow.com/a/73793683

function getFgColor(cl) {
  var cl1 = rgba2hex(cl)
  var r = Number("0x" + cl1.substr(0, 2))
  var g = Number("0x" + cl1.substr(2, 2))
  var b = Number("0x" + cl1.substr(4, 2))
  // var a = Number("0x" + cl1.substr(6, 2))
  var brightness = r * 0.299 + g * 0.587 + b * 0.114
  if (brightness > 160) {
    return "000000" + cl1.substr(6, 2)
  } else {
    return "ffffff" + cl1.substr(6, 2)
  }
}


function addCellColorStyles(styles, stylesCount, stylesDict, fillColors) {
  var fgColors = fillColors.map(getFgColor);
  // add font styles:
  let fontsCount = parseInt($( 'fonts', styles ).attr("count"), 10);
  fgColors.forEach((color) => {
    $( 'fonts', styles ).append( fontTmplt(color.toUpperCase()) );
  });
  $( 'fonts', styles ).attr("count", (fontsCount + fgColors.length).toString());

  // add fill styles:
  let fillsCount = parseInt($( 'fills', styles ).attr("count"), 10);
  fillColors.forEach((color) => {
    $( 'fills', styles ).append( fillTmplt(color.toUpperCase()) );
  });
  $( 'fills', styles ).attr("count", (fillsCount + fillColors.length).toString());

  var cellStyles = Array(fillColors.length).fill().map(
    function (element, index) {
      return { fontIdx: index + fontsCount, fillIdx: index + fillsCount };
  });

  // add cell styles:
  cellStyles.forEach(function (style, i) {
    var nm = fillColors[style.fontIdx - fontsCount].toUpperCase();
    $( 'cellXfs', styles ).append( cellXfTmplt(style.fontIdx,
      style.fillIdx, false, false) );
    stylesDict[nm] = stylesCount + 4 * i;
    $( 'cellXfs', styles ).append( cellXfTmplt(style.fontIdx,
      style.fillIdx, true, false) );
    stylesDict[nm + "pct"] = stylesCount + 4 * i + 1;
    $( 'cellXfs', styles ).append( cellXfTmplt(style.fontIdx,
      style.fillIdx, false, true) );
    stylesDict[nm + "int"] = stylesCount + 4 * i + 2;
    $( 'cellXfs', styles ).append( cellXfTmplt(style.fontIdx,
      style.fillIdx, true, true) );
    stylesDict[nm + "pctint"] = stylesCount + 4 * i + 3;

  });
  $( 'cellXfs', styles ).attr("count", (stylesCount +
                                    Object.keys(stylesDict).length).toString());
}

function highlightCells(that, cfg, sheet, stylesCount, stylesDict, colIndex, rowIndex) {
  let isInt = Array(that.columns().nodes().length).fill().map(function (element, cl) {
    let is_int = true;
    that.columns(cl).data()[0].forEach(function(x) {
      if (is_int) {
        let txt = $('<div />').append(x).text();
        if (isNumeric(txt.replace(/\s*%\s*$/, "")) &&
            !Number.isInteger(Number(txt.replaceAll(/[\s%]/g, "").trim()))) {
          is_int = false;
        }
      }
    })
    return is_int;
  });
  that.cells().every( function ( rowIdx, colIdx, tableLoop, cellLoop ) { // IDEA: Start from visible table, not from all data, see pdf!
    var xlRow ;
    if (cfg.messageTop != null) {
      xlRow = rowIdx + 4; // +1 for DT zero index; +1 for title row; +1 for row heading in Excel
    } else {
      xlRow = rowIdx + 3; // +1 for DT zero index; +1 for title row; +1 for row heading in Excel
    }
    var tdNode = $(that.cell(rowIndex.dom2data[rowIdx], colIndex.dom2data[colIdx]).nodes()[0])
    if (rowIndex.dom2data[rowIdx] != undefined && tdNode.is(":visible")) {
      var bgColor = tdNode.find("[style]").map(function() {
        return(rgba2hex($(this).css("background-color")));
      }).get()
      // find out, if %
      var txt = tdNode.text().trim()
      var pct = txt.endsWith("%");
      if (pct) {
        txt = txt.replace(/%\s*$/, "")
      }
      //var tpe = typeof tp ;
      var n = isNumeric(txt);

      let int
      if (n) {
        int = isInt[colIndex.dom2data[colIdx]];
      } else {
        int = false;
      }

      bgColor = unique( bgColor );
      var bgColor = bgColor.filter(function(x) { return x != "00000000" })
      if (bgColor.length > 0) {
        bgColor = bgColor[0];
        let cellStyle;
        if (n && pct) {
          if (int) {
            cellStyle = stylesDict[bgColor.toUpperCase() + "pctint"]
          } else {
            cellStyle = stylesDict[bgColor.toUpperCase() + "pct"]
          }
        } else {
          if (int) {
            cellStyle = stylesDict[bgColor.toUpperCase() + "int"]
          } else {
            cellStyle = stylesDict[bgColor.toUpperCase()]
          }
        }
        let xlCol = createXlColLetter(colIdx);
        let xlRef = xlCol + xlRow;
        let cellSelector = 'c[r=' + xlRef + ']';
        let cell = $(cellSelector, sheet);
        if (cell.length == 0) { // cell is empty, but we want to have it for color
          // row can also be missing
          if ($("sheetData row[r="+xlRow+"]", sheet).length == 0) {
            // https://stackoverflow.com/a/29259101
            // Create non-jq element
            let rwel = $(sheet.createElement("row")); // not with jquery
            rwel.attr({"r": "" + xlRow}) // need to be 2nd step
            alert("should never happen, sorry. please report excel trouble #1")
            //
            // insertAfter xx
            $("sheetData", sheet).append(rwel)
          }
          let cel = $(sheet.createElement("c")); // not with jquery
          cel.attr({
            "t": "inlineStr",
            "r": xlRef,
            "s": cellStyle.toString()
          })
          let is = $(sheet.createElement("is")); // not with jquery
          is.attr({
            "xml:space": "preserve"
          })
          is.append(" ")
          cel.append(is)
          var all_cells_in_row =
            $("sheetData row[r="+xlRow+"] c[r]", sheet).map(function() {return idxFromColLetter(this.getAttribute("r").replaceAll(/[0-9]+$/g, "")) - 1})

          var where_to_append = getPositionForXLXML(all_cells_in_row.toArray(),
            colIdx);
          if (where_to_append == 0) {
            $("sheetData row[r="+xlRow+"]", sheet).prepend(cel)
          } else {
            var where = $("sheetData row[r="+xlRow+"] c:nth-child("+where_to_append+")", sheet);
            cel.insertAfter(where);
          }
        } else {
          cell.attr("s", cellStyle.toString());
        }
      }
    }
  } );
}

function getPositionForXLXML(list, element) {
  var index = list.findIndex(function(el) {
    return (element < el)
  });
  if (index == -1) {
    return list.length;
  } else {
    return index ;
  }
};

// https://stackoverflow.com/a/9906193
function idxFromColLetter(l) {
  var base = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', i, j, result = 0;

  for (i = 0, j = l.length - 1; i < l.length; i += 1, j -= 1) {
    result += Math.pow(base.length, j) * (base.indexOf(l[i]) + 1);
  }

  return result;
};

// to build an Excel column letter reference from an
// integer (1 -> A, 2 -> B, 28 -> AB, and so on...);
function createXlColLetter( n ){
  var ordA = 'A'.charCodeAt(0);
  var ordZ = 'Z'.charCodeAt(0);
  var len = ordZ - ordA + 1;
  var s = "";
  while( n >= 0 ) {
    s = String.fromCharCode(n % len + ordA) + s;
    n = Math.floor(n / len) - 1;
  }
  return s;
}

// style templates
function fontTmplt(color) {
  color = rgba2argbxl(color);
  return `<font><sz val="11" /><name val="Calibri" /><color rgb="${ color}" /></font>`;
}
function fillTmplt(color) {
  color = rgba2argbxl(color);
  return `<fill><patternFill patternType="solid"><fgColor rgb="${ color}" /><bgColor indexed="64" /></patternFill></fill>`;
}
function cellXfTmplt(fontIdx, fillIdx, pct, int) {
  var numFmtId;
  if (pct) {
    if (int) {
      numFmtId = 9
    } else {
      numFmtId = 10
    }
  } else {
    if (int) {
      numFmtId = 1
    } else {
      numFmtId = 2
    }
  }
  return `<xf numFmtId="${numFmtId}" fontId="${fontIdx}" fillId="${fillIdx}" borderId="0" applyFont="1" applyFill="1" applyBorder="1" />`;
}

function unique(array) { // https://stackoverflow.com/a/10192255
    return $.grep(array, function(el, index) {
        return index === $.inArray(el, array);
    });
}

function addCellColorStylesPdf(styles, fillColors) {
  var fgColors = fillColors.map(getFgColor);
  fillColors.forEach(function (color, i) {
    var c = fgColors[i]
    var f = fillColors[i]
    styles["s" + color] = {
      'color': "#" + c.substr(0, 6),
      'fillColor': "#" + f.substr(0, 6)
    }
  })
}


// https://stackoverflow.com/a/72506073
function sumOfArray(nums) {
  return nums.reduce((s, n) => (s.push((s.at(-1) ?? 0) + n), s), []);
}

function computeRowIndex(that) {
  var dom2data = that.rows({search:'applied'}).indexes().toArray();
  return {
    "dom2data": dom2data
  }
}

function computeColIndex(that) {
  var data2dom = sumOfArray(that.columns().visible()).map(
    function(x) {return x - 1});
  var maxdom = data2dom[data2dom.length-1];
  var dom2data = Array(maxdom).fill().map(
    function (element, index) {
      return data2dom.findIndex(function(x) { return x == index + 1; });
  });
  dom2data.unshift(0);
  return {
    "dom2data": dom2data,
    "data2dom": data2dom
  }
}

function customize_pdf(pdf, cfg, that) {
  var colIndex = computeColIndex(that);
  var rowIndex = computeRowIndex(that)
  var fillColors = $(that.nodes()).find("[style]").map(function(){return(rgba2hex($(this).css("background-color")))}).get()
  fillColors = unique( fillColors );
  var colors = fillColors.filter(function(x) { return x != "00000000" })

  addCellColorStylesPdf(pdf.styles, fillColors)
  let tabEl =
    pdf.content.find(function (docEl) { return docEl.table != undefined});

  tabEl.table.body.forEach(function(row, rowIdx) {
    row.forEach(function(col, colIdx) {
      if (col.style != "tableHeader") {
        var tdNode = $(that.cell(rowIndex.dom2data[rowIdx-1], colIndex.dom2data[colIdx]).nodes()[0])
        var bgColor = tdNode.find("[style]").map(function() {
          return(rgba2hex($(this).css("background-color")));
        }).get()
        bgColor = unique( bgColor );
        var bgColor = bgColor.filter(function(x) { return x != "00000000" })
        if (bgColor.length > 0) {
          bgColor = bgColor[0];
          col.style = "s" + bgColor
        }
      }
    })
  })
  pdf.styles.tableHeader.fontSize = 8;
  pdf.defaultStyle.fontSize = 6;
  pdf.pageSize = "A3"
}

function customize_excel(xlsx, cfg, that) {
  var sheet = xlsx.xl.worksheets["sheet1.xml"];
  $(sheet.documentElement).find("[r=A1]").attr("r", "B1")
  if (cfg.messageTop != null) {
    $(sheet.documentElement).find("[r=A2]").attr("r", "B2")
  }
  $(sheet.documentElement).find("mergeCells mergeCell").remove()
  customize_excel_new(xlsx, cfg, that)

  var freezePanes =
    '<sheetViews><sheetView tabSelected="1" workbookViewId="0"><pane xSplit="1" ySplit="' + ( 2 + (cfg.messageTop == null ? 0 : 1) ) + '" topLeftCell="' + "B" + ( 3 + (cfg.messageTop == null ? 0 : 1) ) +'" activePane="bottomRight" state="frozen"/></sheetView></sheetViews>';
  var current = sheet.children[0].innerHTML;
  current = freezePanes + current;
  sheet.children[0].innerHTML = current;
}

function removeNull(sheet) {
  $(sheet.documentElement).find('t:contains("null")').filter(function() {
    return($(this).attr('xml:space') == "preserve")
  }).text("")
}

function customize_excel_new(xlsx, cfg, that) {
  var colIndex = computeColIndex(that);
  var rowIndex = computeRowIndex(that)
  var fillColors = $(that.nodes()).find("[style]").map(function(){return(rgba2hex($(this).css("background-color")))}).get()
  fillColors = unique( fillColors );
  var colors = fillColors.filter(function(x) { return x != "00000000" })

  // styles dictionary
  let stylesDict = new Object();

  // set up new styles:
  let styles = xlsx.xl['styles.xml'];
  let stylesCount = parseInt($( 'cellXfs', styles ).attr("count"), 10);
  addCellColorStyles(styles, stylesCount, stylesDict, fillColors);

  // use new styles:
  let sheet = xlsx.xl.worksheets['sheet1.xml'];
  //$( 'row c', sheet ).attr( 's', (stylesCount + 1).toString() );
  highlightCells(that, cfg, sheet, stylesCount, stylesDict, colIndex, rowIndex);
  removeNull(sheet);
}

// https://stackoverflow.com/a/49974627
function rgba2hex(orig) {
  var a, isPercent,
    rgb = orig.replace(/\s/g, '').match(/^rgba?\((\d+),(\d+),(\d+),?([^,\s)]+)?/i),
    alpha = (rgb && rgb[4] || "").trim(),
    hex = rgb ?
    (rgb[1] | 1 << 8).toString(16).slice(1) +
    (rgb[2] | 1 << 8).toString(16).slice(1) +
    (rgb[3] | 1 << 8).toString(16).slice(1) : orig;

  if (alpha !== "") {
    a = alpha;
  } else {
    a = 01;
  }
  // multiply before convert to HEX
  a = ((a * 255) | 1 << 8).toString(16).slice(1)
  hex = hex + a;

  return hex;
}

function rgba2argbxl(cl) {
  var a = "FF"
  if (cl.length > 6) {
    a = cl.slice(-2);
  }
  var rgb = cl.substr(0, 6);
  return a + rgb ;
}

function rowFilter(idx, data, node) {
  return ( $(node).attr('style') != 'display: none;')
}

function setMsgTop(cap) {
  var target = event.currentTarget;
  var _target = $(target);
  var dt_id = _target.closest("[id]").attr("id");
  var msgTop = sessionStorage.getItem(dt_id + ".ActiveFilter");
  if (msgTop == null) {
    msgTop = $(_target.closest("[id]").find(".buttons-colvisGroup")[0]).text();
  }
  return cap + " -- " + msgTop;
}

function dataquieRcolorize(config) {
    var tb = config.table.table()
    var cfg = config.additional_init_args
    if (!cfg) {
      cfg = {}
    }
    if (cfg.grading_cols && cfg.grading_order && cfg.grading_colors && cfg.fg_colors) {
      var grading_order = cfg.grading_order
      var grading_colors = cfg.grading_colors
      var fg_colors = cfg.fg_colors
      cfg.grading_cols.forEach(function(column) {
        $(tb.column(column + ":name").nodes()).each(function() {
          var cls = grading_order.indexOf($(this).text());
          if (cls >= 0) {
            $(this).css("background-color", grading_colors[cls]);
            $(this).css("color", fg_colors[cls]);
          }
        })
      })
    }
}

function dataquieRdtCallback(config) {
  if (config.initialColTag != null)
    config.table.button(config.initialColTag + ":name").trigger()
  if (config.initSearch != null)
    config.table.searchBuilder.rebuild(config.initSearch)
  if (!window.dtConfig) {
    window.dtConfig = {}
  }
  window.dtConfig[$(config.table.table().container()).attr("id")] = config.additional_init_args
  dataquieRcolorize(config)
  // window.tb = config.table
  return config.table ;
}

$(function() {
  $(".buttons-colvisGroup").each(function(){
    $(this).on("click", function() {
      var _this = $(this);
      // var dt = _this.closest(".datatables").find("table").DataTable();
      var dt_id = _this.closest("[id]").attr("id");
      var filterText = _this.text();
      sessionStorage.setItem(dt_id + ".ActiveFilter", filterText);
    })
  })
})
