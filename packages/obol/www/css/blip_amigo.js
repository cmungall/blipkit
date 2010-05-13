var webSearchUpdater = null;

function bodyOnLoad() 
{

  var opts = {   prefetchBuffer: false, 
                 //onscroll : updateWebHeader,
                 sortAscendImg: 'http://jerome.sadou.free.fr/grid/riimages/sort_asc.gif',
                 sortDescendImg: 'http://jerome.sadou.free.fr/grid/images/sort_desc.gif'
  };

  //webSearchUpdater = new Rico.LiveGrid("data_grid",3,21, "api?api=data_grid", opts);     
}

function updateWebHeader( liveGrid, offset ) {
  $('webResultRange').innerHTML = 'Results ' + (offset+1) + ' - ' + (offset + liveGrid.metaData.pageSize) + '&nbsp;';
  updateScrollerTip( liveGrid, offset );
}


function doAmigoSearch() {
         
  webSearchUpdater.setRequestParams('data_class=' + obd_query_basic.data_class.value + '&search_text=' + obd_query_basic.search_text.value);
  webSearchUpdater.resetContents();
  webSearchUpdater.requestContentRefresh(0);
  //$('webResultRange').innerHTML = 'Results 1 - 5&nbsp;';
}

function xxupdateHeader( liveGrid, offset ) {
  $('bookmark').innerHTML = "Listing records " + (offset+1) + " - " + (offset+liveGrid.metaData.getPageSize()) + " of " + 
    liveGrid.metaData.getTotalRows();
  var sortInfo = "";
  if (liveGrid.sortCol) {
    sortInfo = "&data_grid_sort_col=" + liveGrid.sortCol + "&data_grid_sort_dir=" + liveGrid.sortDir;
  }
  $('bookmark').href="http://jerome.sadou.free.fr/grid//index.php" + "?data_grid_index=" + offset + sortInfo;
}

function debug(str) {
   new Insertion.Bottom( $('debug'), str + "</br>" );
}

function insertNodeHtml( AnchorID, Params ) {
  element = document.getElementById(AnchorID);
  nextElement = document.getElementById(AnchorID+"-content");

  widgetElement = element.getElementsByTagName('span')[0];
  contentElement = nextElement.getElementsByTagName('span')[0];

  if (widgetElement.innerHTML == "-") {
    widgetElement.innerHTML = "+";
    //contentElement.innerHTML = "";
    contentElement.style.display='none';
  }
  else if (contentElement.style.display=='none') {
    widgetElement.innerHTML = "-";
    contentElement.style.display='';
  }
  else {
    widgetElement.innerHTML = "-";
    //contentElement.innerHTML = "<h2>please wait...</h2>";
    request = '/?show=innerHtml&'+Params;
    //    alert("updating "+AnchorID+" with "+request+" to "+contentElement.innerHTML);
    new Ajax.Request(request,
                     {
                     method:'get',
                         onSuccess: function(resp) 
                         {
                           contentElement.innerHTML = 
                             resp.responseText;
                         },
                         inFailure: function(resp) 
                         {
                           alert(resp);
                         }
                         
                     });

    //new Ajax.Updater(element, '/?innerNodeHtml='+NodeID, { method: 'get' });
  }
}
