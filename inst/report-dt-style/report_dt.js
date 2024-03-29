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
          return(data)
        }
     } else if (type == 'sort') {
        var string = data;
        var regex = new RegExp('[\\s\\r\\t\\n]*([a-z0-9\\-_]+)[\\s\\r\\t\\n]*=[\\s\\r\\t\\n]*([\'"])((?:\\\\\\2|(?!\\2).)*)\\2', 'ig');
        var attributes = {};
        while ((match = regex.exec(string))) {
            attributes[match[1]] = match[3];
        }
        if (attributes.hasOwnProperty("sort")) {
          return(1 * attributes.sort);
        } else {
          return(data)
        }
     } else if (type == 'type') {
        var string = data;
        var regex = new RegExp('[\\s\\r\\t\\n]*([a-z0-9\\-_]+)[\\s\\r\\t\\n]*=[\\s\\r\\t\\n]*([\'"])((?:\\\\\\2|(?!\\2).)*)\\2', 'ig');
        var attributes = {};
        while ((match = regex.exec(string))) {
            attributes[match[1]] = match[3];
        }
        if (attributes.hasOwnProperty("sort")) {
          return(1);
        } else {
          return("string")
        }
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
