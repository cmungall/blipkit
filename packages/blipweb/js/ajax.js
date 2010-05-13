//////////
//
// File:      ajax.js (bbop/ajax.js)
// Namespace: BBOP.Ajax
// Comment:   General purpose AJAX module.
// Depends:   None.
// Status:    Completed.
// Passed:    FF 1.5+, Safari 2, IE7, IE6.
// Failed:    
// TODO:      
// BUGS:      No known bugs.
//
//////////


// Module and namespace checking.
if ( typeof BBOP == "undefined" ){ BBOP = {}; }


// Constructor. Wraps the complexities of MS's non-standard
// implementation.
BBOP.AjaxAction = function(callback){

  // Capture a new XHR in a closure.
  var xhr = (function(){
    var new_request_object = undefined;
    if( window.XMLHttpRequest ) {
      new_request_object = new XMLHttpRequest();
    } else if(window.ActiveXObject) {
      try {
	new_request_object = new ActiveXObject("Msxml2.XMLHTTP");
      } catch(e) {
	new_request_object = new ActiveXObject("Microsoft.XMLHTTP");
      }
    }
    return new_request_object;
    })();

  xhr.onreadystatechange = function(){
    if ( xhr.readyState == 4 ){
      if ( xhr.status == 200 ){
	if (callback){
	  //callback(xhr.responseXML);
	  callback(new XML(xhr.responseText));
	}
      }
    }else if ( xhr.readyState == 3 ){
    }else if ( xhr.readyState == 2 ){
    }else if ( xhr.readyState == 1 ){
    }else if ( xhr.readyState == 0 ){
    }else{
      throw new Error("Whoa! How did I get here?");
    } 
  }
  
  this.start = function(server, query, type){
    if ( type == "GET" ){
      url = "http://localhost:8182"+server;
      xhr.open("GET", server,true);
      xhr.send(null); // firefox requires an argument here
    }else{
      xhr.open("POST", server, true);
      xhr.setRequestHeader("Content-Type",
			   "application/x-www-form-urlencoded");
      xhr.send(query);   
    }
  }
}


// Constructor. Wraps the complexities of MS's non-standard
// implementation.
BBOP.Ajax = function(callback){ 

  // Define "this.xhr". Branch for native XMLHttpRequest object an
  // branch for IE/Windows ActiveX version
  if( window.XMLHttpRequest ) {
    this.xhr = new XMLHttpRequest();
  } else if(window.ActiveXObject) {
    try {
      this.xhr = new ActiveXObject("Msxml2.XMLHTTP");
    } catch(e) {
      this.xhr = new ActiveXObject("Microsoft.XMLHTTP");
    }
  }

  // Bind the callback if extant.
  if ( callback ){
    this.xhr.onreadystatechange = callback;
  }
}


// Wrapper for actual request. The default type is POST.
BBOP.Ajax.prototype.startAction = function(server, query, type){

  if ( type == "GET" ){
    this.xhr.open("GET", server + "?" + query, true);
    this.xhr.send(null);
  }else{
    this.xhr.open("POST", server, true);
    this.xhr.setRequestHeader("Content-Type",
			      "application/x-www-form-urlencoded");
    this.xhr.send(query);   
  }
}


// Return the ready state of the object.
BBOP.Ajax.prototype.actionStatus = function(){
    return this.xhr.readyState;
}


// Get the XML from the response.
BBOP.Ajax.prototype.getXML = function(){
    return this.xhr.responseXML;
}


// Get the text from the response.
BBOP.Ajax.prototype.getText = function(){
    return this.xhr.responseText;
}
