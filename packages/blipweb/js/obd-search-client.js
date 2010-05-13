//////////
//
// OBD Search Client
//
// Purpose:
//
// Depends: 
//
// Status:
//
// Passed:  Firefox 1.5,
// Failed:  
//
// QUIRKs:
//
//   Grep these files for "QUIRK".
//
// WARNINGs:
//
//   Grep these files for "WARNING".
//
// BUGs:
//   
//   Grep these files for "BUGS".
//
// TODO:
//
//   Grep these files for "TODO".
//
//////////


// Make sure that the BBOP and OBD namespaces are defined somwhere.
if( typeof BBOP == "undefined" ){
  throw new Error("BBOP namespace is nowhere to be found"); }
if( typeof BBOP.Progress == "undefined" ){
  throw new Error("BBOP.Progress namespace is nowhere to be found"); }
if( typeof BBOP.Timer == "undefined" ){
  throw new Error("BBOP.Timer namespace is nowhere to be found"); }
if( typeof BBOP.Progress.Tag == "undefined" ){
  throw new Error("BBOP.Progress.Tag namespace is nowhere to be found"); }
if( typeof BBOP.Ajax == "undefined" ){
  throw new Error("BBOP.Ajax namespace is nowhere to be found"); }
if( typeof BBOP.ProtoControl == "undefined" ){
  throw new Error("BBOP.ProtoControl namespace is nowhere to be found"); }
if( typeof AmiGO == "undefined" ){
  throw new Error("AmiGO namespace is nowhere to be found"); }
if( typeof AmiGO.Store == "undefined" ){
  throw new Error("AmiGO.Store namespace is nowhere to be found"); }
if( typeof OBD == "undefined" ){
  throw new Error("OBD namespace is nowhere to be found"); }
if( typeof OBD.Model == "undefined" ){
  throw new Error("OBD.Model namespace is nowhere to be found"); }


//
var global_cgi_path = undefined;
var global_htdocs_path = undefined;
var global_cookie_name = undefined;

//
var global_amigo_load_div = "load_tag";
var global_amigo_load_tag = undefined;
var global_amigo_work_div = "work_tag";
var global_amigo_work_tag = undefined;

//
//var global_ontology_filter_select = "ontologies";
//var global_evcode_filter_select = "evcodes";
//var global_speciesdb_filter_select = "speciesdbs";
var global_search_filter_input = "query";

//
var global_cgi_string_div = "cgi_string";
var global_search_summary_div = "search_summary";
//var global_bool_term_summary_div = "bool_term_summary";
var global_timer_div = "timer";
var global_search_results_div = "search_results";
var global_parser_status_div = "parser_status";
var global_parsed_div = "parsed";

//
var global_cgi_query = '';


// The main onload function.
// Establishes the buttons and search box.
function AmiGO2_Init(htdocs_path, cgi_path, cookie_name){  
  
  global_cgi_path = cgi_path;
  global_htdocs_path = htdocs_path;
  global_cookie_name = cookie_name;

  // Get tags ready.
  global_amigo_load_tag =
    new BBOP.Progress.Tag( document.getElementById(global_amigo_load_div),
			   htdocs_path);
  global_amigo_load_tag.label('Loading...');
  global_amigo_work_tag =
    new BBOP.Progress.Tag( document.getElementById(global_amigo_work_div),
			   htdocs_path);
  global_amigo_work_tag.label('Working...');

  var inputs = document.getElementsByTagName("input");
  for( var i = 0; i < inputs.length; i++ ){
    //
    var input = inputs[i];
    if( input.name == 'fields' ){
      input.onclick = updateSummaryQuery;
    }else if( input.name == 'type' ){
      input.onclick = updateSummaryQuery;
    }
  }

  // Activiate live selects.
  //Event.observe(global_speciesdb_filter_select, 'click', updateSummaryQuery);
  //Event.observe(global_evcode_filter_select, 'click', updateSummaryQuery);
  //Event.observe(global_ontology_filter_select, 'click', updateSummaryQuery);
  var foo = document.getElementById(global_search_filter_input);
  foo.onkeyup = updateSummaryQuery;

  // TODO: Get forst batch of results.
  updateSummaryQuery();
}


//////////
//
// Handle the query string.
//
//////////


// Event argument not needed.
function updateSummaryQuery(event){

  global_cgi_query = 'request=summary';
  updateQuery(event);
  summarySearch();
}


// Event argument not needed.
function updateDataQuery(event){
  global_cgi_query = 'request=data';
  updateQuery(event);
}


// Event argument not needed.
function updateQuery(event){

  // First, scrap the inputs. BUG/TODO: This works 'cause we know
  // there's one input.
  var inputs = document.getElementsByTagName("input");
  for( var i = 0; i < inputs.length; i++ ){
    //
    var input = inputs[i];
    if( input.name == 'fields' && input.checked){
      global_cgi_query = global_cgi_query + '&' + input.value + "=yes";
    }else if( input.name == 'type' && input.checked ){
      global_cgi_query = global_cgi_query + '&' + 'type=' + input.value;      
    }else if( input.name == 'query' ){
      global_cgi_query =
	global_cgi_query + "&query=" + encodeURIComponent(input.value);
    }
  }

  // Next, scrap all of the selects.
  var selects = document.getElementsByTagName("select");
  for( var i = 0; i < selects.length; i++ ){
    for( var j = 0; j < selects[i].options.length; j++ ){
      
      var select = selects[i];        
      var option = select.options[j];
      
      if( select.name && option.selected == true ){     
	if( select.name == 'speciesdbs' ){
	  global_cgi_query = global_cgi_query + "&speciesdb=" + option.value;
	}else if( select.name == 'evcodes' ){
	  global_cgi_query = global_cgi_query + "&evcode=" + option.value;
	}else if( select.name == 'ontologies' ){
	  global_cgi_query = global_cgi_query + "&ontology=" + option.value;
	}
      }
    } 
  }
  
  // Finally, update the panel for debugging.
  var foobug = document.getElementById(global_cgi_string_div);
  foobug.innerHTML = global_cgi_query;
}


//////////
//
// Handle the summary search.
//
//////////


//
function summarySearch(){

  // Start AJAX action: Request initial data from server.
  var server = global_cgi_path + "/obd-search-data-server.cgi";
  var ajax = new BBOP.AjaxAction(_summarySearch_Response);
  ajax.start(server, global_cgi_query, "POST");
}


//
function _summarySearch_Response(xml_response){

  //var bool_term_summary_div =
  //  document.getElementById(global_bool_term_summary_div);

  //var bool_term_results = xml_response.getElementsByTagName('bool_terms');

  var search_summary_div = document.getElementById(global_search_summary_div);
  var search_results = xml_response.getElementsByTagName('nodes');
  var search_result = 0;
  if( search_results[0] && 
      search_results[0].firstChild &&
      search_results[0].firstChild.data ){
    search_result = search_results[0].firstChild.data;
  }
  search_summary_div.innerHTML = search_result;


  var parser_status_div = document.getElementById(global_parser_status_div);
  var parser_status = xml_response.getElementsByTagName('parser_status');
  var status = '';
  if( parser_status[0] && 
      parser_status[0].firstChild &&
      parser_status[0].firstChild.data ){
    status = parser_status[0].firstChild.data;
  }
  parser_status_div.innerHTML = status;

  var parsed_div = document.getElementById(global_parsed_div);
  var parsed = xml_response.getElementsByTagName('parsed');
  var parsed_string = '';
  if( parsed[0] && 
      parsed[0].firstChild &&
      parsed[0].firstChild.data ){
    parsed_string = parsed[0].firstChild.data;
  }
  parsed_div.innerHTML = parsed_string;

//   var bool_term_result = 0;
//   if( bool_term_results[0] && 
//       bool_term_results[0].firstChild &&
//       bool_term_results[0].firstChild.data ){
//     bool_term_result =  bool_term_results[0].firstChild.data;
//   }

  // Update table.
//   bool_term_summary_div.innerHTML = bool_term_result;
}


//////////
//
// Handle the data (traditional) search.
//
//////////


// Click on the submit button.
function dataSearch(){

  global_amigo_work_tag.start();

  // A timer for debugging.
  var timer = new BBOP.Timer("123");
  timer.displayClear();
  timer.start();

  // Start AJAX action: Request initial data from server.
  updateDataQuery();
  var server = global_cgi_path + "/obd-search-data-server.cgi";
  var ajax = new BBOP.AjaxAction(_search_Response);
  ajax.start(server, global_cgi_query, "POST");
}


// Respond to response data by ammending tree.
function _search_Response(xml_response){

  global_amigo_work_tag.stop();
  global_amigo_load_tag.start();

  // Parse AJAX response into OBD Data Model.
  var data_handle = new OBD.Handle("xml-dom");
  var obdm = data_handle.parse(xml_response);
 
  // Display the response.
  var search_table_array = new Array;
  var number_of_search_results = 0;

  // Build a table for results
  var search_sortabletable =
    new BBOP.SortableTable("search_results_table_1",
			   new Array("Name", "ID", "Description",
				     "Synonym(s)", "Source"));

  for( var i = 0; i < obdm.graphs.length ; i++){
    
    obdm.graphs[i].compile();
    
    for( var j = 0; j < obdm.graphs[i].get_node_number(); j++){
      var current_node = obdm.graphs[i].get_node(j);

      if( current_node.metatype() == 'term' ){

	var label_val = "[Label is N/A]";
	var id_val = "[ID is N/A]";
	var desc_val = "[No Description]";
	var syn_val = "[No Synonyms]";
	var src_val = "[Source is N/A]";

	if( current_node.id() ){
	  id_val = current_node.id(); }
	if( current_node.label() ){
	  label_val = current_node.label(); }
	if ( current_node.get_source() ){
	  src_val = current_node.get_source(); }

	// Concatenate the descriptions.
	if( current_node.number_of_descriptions() > 0 ){
	  var desc_array = new Array;
	  for( var k = 0; k < current_node.number_of_descriptions(); k++ ){
	    desc_val = current_node.get_description(k).label();
	    desc_array.push(desc_val);
	  }
	  desc_val = desc_array.join('<br />');
	}

	// Concatenate the synonyms.
	if( current_node.number_of_aliases() > 0 ){
	  var syn_array = new Array;
	  for( var l = 0; l < current_node.number_of_aliases(); l++ ){
	    syn_val = current_node.get_alias(l).label();
	    syn_array.push(syn_val);
	  }
	  syn_val = syn_array.join('<br />');
	}

	// TODO: Highlight found points.
	var highlights = global_cgi_query.match(/\w+|\"[\w\s]+\"|\(|\)/g);
	for(  var m = 0; m < highlights.length; m++ ){
	  if( highlights[m] != 'OR' &&
	      highlights[m] != 'AND' &&
	      highlights[m] != 'SANS' &&
	      highlights[m] != ')' &&
	      highlights[m] != '(' ){
	    label_val =
	      label_val.replace(/(highlights[m])/gi,
				"<span class=\"highlight\">$1</span>");
	  }
	}

	    // Finally, add to table and increment results.
	search_sortabletable.addRow(new Array(label_val, id_val,
					    desc_val, syn_val, src_val));
	number_of_search_results++;
      }
    }
  }

  // Display term results.
  var search_display_div = document.getElementById(global_search_results_div);
  search_display_div.innerHTML = number_of_search_results + " result(s) found.";
  search_display_div.appendChild(search_sortabletable.substantiate());

  // Recall invoked timer.
  var timer = new BBOP.Timer("123");
  timer.stop();
  timer.display(global_timer_div);

  global_amigo_load_tag.stop();
}
