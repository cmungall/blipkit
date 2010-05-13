// Respond to response data by ammending tree.
function _search_Response_DEPREC(xml_response){

  //global_amigo_work_tag.stop();
  //global_amigo_load_tag.start();
  var table = resultTable(xml_response);
  document.getElementById("content_container").innerHTML = table;
  //document.getElementById("content_container").innerHTML = xml_response;
}

function nodeSearch(contextName){

    //global_amigo_work_tag.start();
    // A timer for debugging.
    var timer = new BBOP.Timer("123");
    var dataSource = document.getElementById("dataSource").value;
    timer.displayClear();
    timer.start();
	
    // Start AJAX action: Request initial data from server.
    var resource = "/" + contextName + "/" + dataSource + "/obdxml/search/contains/" + document.getElementById("search_term").value;
    //var ajax = new BBOP.AjaxAction(_search_Response);
    var ajax = new BBOP.AjaxAction( 
                                    function(xml_response) {
                                        //var table = resultTable(xml_response,dataSource,contextName);
                                        var table ='Hello, world!';
                                        document.getElementById("content_container").innerHTML = table;
                                    } 
                                   );
  
    ajax.start(resource, "", "GET");
}

function updateDetailDiv(id){

  //global_amigo_work_tag.start();

  // A timer for debugging.
  var timer = new BBOP.Timer("123");
  timer.displayClear();
  timer.start();

  var resource = "/obdxml/nodes/" + id + "/statements/about";
  //var ajax = new BBOP.AjaxAction(_search_Response);
  var ajax = 
    new BBOP.AjaxAction( 
                        function(xml_response) {
                          document.getElementById("content_container").innerHTML = nodeDetailDiv(xml_response);
                        }
                        );

  ajax.start(resource, "", "GET");
  return false;
}


function nodeDetailDiv(graph) {
  statements = graph.LinkStatement;
  table = '<table border="1"/>';
  for (var i=0; i<statements.length(); i++) {
    table.tr += statementRow(statements[i]);
  }
  return table;
}

function statementRow(s) {
  nHref = nodeHref(s.node.@about);
  rHref = nodeHref(s.relation.@about);
  tHref = nodeHref(s.target.@about);
  sHref = nodeHref(s.source.@about);
  inferredVal = "";
  if (s.@isInferred=='true') {
    inferredVal="[Implied]";
  }
  row =<tr/>;
  row.tr += <td>{nHref}</td>;
  row.tr += <td>{rHref}</td>;
  row.tr += <td>{tHref}</td>;
  row.tr += <td>{sHref}</td>;
  row.tr += <td>{inferredVal}</td>;

  return row;                       
}


function resultTable(graph,dataSource,contextName) {
  nodes = graph.Node;
  table = '<table border="1"/>';
  for (var i=0; i<graph.Node.length(); i++) {
    table.tr += tableRow(nodes[i],dataSource,contextName);
  }
  return table;
}

function tableRow(node,dataSource,contextName) {
  href = nodeHref(node.@id,dataSource,contextName);
  row =<tr/>;
  row.tr += <td>{href}</td>;
  row.tr += <td>{node.label}</td>;
  row.tr += <td>{node.source.@about}</td>;

  return row;                       
}

function nodeHref(id,dataSource,contextName) {

  //href = new XML('<a href="#" onClick="updateDetailDiv( '+id+'} )">{id}</a>');
  href = <a>{id}</a>;
  href.@href = "/" + contextName + "/" + dataSource + "/view/entity/"+id;
  //href.@onClick = "updateDetailDiv('"+id+"')";
  //href = <a href="#" onClick="updateDetailDiv(&apos;{id}&apos;)">{id}</a>;
  
  return href;
}

function testSearch() {
  var foo=5;

  table =
    resultTable(<Graph><Node id="x"><label>ooo</label></Node><Node/></Graph>);
  document.getElementById("content_container").innerHTML = 
    table;

}

function xxtestSearch() {
  var foo=5;
  document.getElementById("content_container").innerHTML = 
 <table border="1">
  <tr>
    <td>foo</td>
    <td>{foofun()}</td>
  </tr>
 </table>
;
}

function foofun() {
  return "5";
}


function AmiGO2_Init(htdocs_path, cgi_path, cookie_name){  
  
  global_cgi_path = cgi_path;
  global_htdocs_path = htdocs_path;
  global_cookie_name = cookie_name;

  // Get tags ready.
  /*
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
  */
}
