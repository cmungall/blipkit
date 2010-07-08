
function fetch_graph_image(base_url, form) {
    var url = base_url + '&'; // assumes already '?'
    for (i=0; i<form.rel.length; i++) {
        var re = form.rel[i];
        if (re.name == 'rel' && re.checked) {
            url += 'rel='+re.value+'&';
        }
    }
    for (i=0; i<form.cr.length; i++) {
        var re = form.cr[i];
        if (re.name == 'cr' && re.checked) {
            url += 'cr='+re.value+'&';
        }
    }
    document.main_img.src=url;
}

function fetch_graph_image_all_relations(base_url) {
    var url = base_url + '&rel=all';
    document.main_img.src=url;
}

function replaceContents(e, url){
    e.innerHTML = "<i>fetching...</i>";
    dojo.xhrGet({url: url,
		 load: function(o) { 
                     e.innerHTML = o;
                     return o;
                 },
                 error: function(o) {
                     alert("Could not retrieve from "+url);
                 },
	        });
}

function replaceElement(e, url){
    dojo.xhrGet({url: url,
		 load: function(o) { 
                     //e.innerHTML = o;
                     //e.insertBefore("foo");
                     //e.appendChild("foo");
                     return o;
                 },
                 error: function(o) {
                     alert("Could not retrieve from "+url);
                 },
	        });
}

function addRowsToTBody(tb, e, url){
    //nr.innerHTML = "<td colspan=5>xxxx</td>";
    //tb.insertBefore(nr,e.nextElementSibling);
    dojo.xhrGet({url: url,
		 load: function(o) { 
                     rowlist = eval(o);
                     for (i in rowlist) {
                         row = document.createElement('tr');
                         tb.insertBefore(row,e.nextElementSibling);
                         row.innerHTML += rowlist[i];
                     }
                     //tb.insertBefore(nr,e.nextElementSibling);
                     return o;
                 },
                 error: function(o) {
                     alert("Could not retrieve from "+url);
                 },
	        });
}

function toggleTable(div_id, showLabel, hideLabel){

    var detailsDiv = document.getElementById(div_id);
    var toggler = document.getElementById((div_id+'_toggler'));
	
    if (detailsDiv && toggler){
					
	if (detailsDiv.style.display == "none"){
	    detailsDiv.style.display='table-cell';
            toggler.innerHTML = hideLabel;
	} else {
	    detailsDiv.style.display="none";
            toggler.innerHTML = showLabel;
            
	}			
    }
}
