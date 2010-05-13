function nodeSearch(contextName,dataSource){

  var searchTerm = document.getElementById("search_term");
  var target = document.getElementById("search_target_data");
  if (dataSource == null) {
    dataSource = document.getElementById("dataSource").value;
  }

  if ((searchTerm != null)&&(dataSource != null)&&(target != null)){
    var url = "";
    if (target.value == 'statements'){
      url = "/" + contextName + "/" + dataSource + "/html/search/all/any/contains_all/" + searchTerm.value + "/statements/all";
    } else {
      url = "/" + contextName + "/" + dataSource + "/html/search/all/any/contains_all/" + searchTerm.value;
    }
    window.location = url;
  }
  return false;

}