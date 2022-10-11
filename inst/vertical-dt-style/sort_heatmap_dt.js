sort_heatmap_dt = function(data, type, row, meta) {
  /* https://www.pierrerebours.com/2017/09/custom-sorting-with-dt.html */
     if (type == 'sort') {
        var string = data;
        var regex = new RegExp('[\\s\\r\\t\\n]*([a-z0-9\\-_]+)[\\s\\r\\t\\n]*=[\\s\\r\\t\\n]*([\'"])((?:\\\\\\2|(?!\\2).)*)\\2', 'ig');
        var attributes = {};
        while ((match = regex.exec(string))) {
            attributes[match[1]] = match[3];
        }
        return(1 * attributes.sort);
     } else if (type == 'type') {
       return(1);
     } else {
       return(data);
     }
}
