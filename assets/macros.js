// scales an image
remark.macros.scale = function (percentage) {
  var url = this;
  return '<img src="' + url + '" style="width: ' + percentage + '" />';
};

// adds class to an image
remark.macros['class'] = function(cl) {
  var url = this;
  return '<img src="' + url + '" class="' + cl + '" />';
};

// width and class of an image
remark.macros['gen'] = function(w, cl="") {
  var url = this;
  return '<img src="' + url + '" style="width: ' + w + '" class="' + cl + '" />';
};
