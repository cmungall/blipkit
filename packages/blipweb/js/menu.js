// Suckerfish JS for IE compat:
// http://www.htmldog.com/articles/suckerfish/
pseudoHover = function() {
  var pseudo_hover_elements =
  document.getElementById("menu").getElementsByTagName("LI");
  for( var i = 0; i < pseudo_hover_elements.length; i++ ){
    pseudo_hover_elements[i].onmouseover = function() {
      this.className+=" pseudohover";
    }
    pseudo_hover_elements[i].onmouseout = function() {
      this.className =
      this.className.replace(new RegExp(" pseudohover\\b"), "");
    }
  }
}
  
//
if (window.attachEvent) window.attachEvent("onload", pseudoHover);
