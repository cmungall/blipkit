if (document.getElementById && document.getElementsByTagName && document.createElement && document.createTextNode) {
//	document.write('<style type="text/css"> <!-- .toggleable { display: none; } --> <\/style>');
	var checkBoxes;
	var numberChecked;
	var pathToDir;
	window.onload = initialize;
}

function initialize()
{	initResultsTable();
	initFormHilite();
	startToggles();
	if (YAHOO.util.Event.addListener)
	{	advSearchFncns();
	}
	addressMaker();
}

function makeLink(href, id, text, title, className)
{	if (!title) { title = text; }
	var link = makeElement('A', id, title, className);
	link.href = href;
	var linkTxt = document.createTextNode(text);
	link.appendChild(linkTxt);
	return link;
}

function makeElement(type, id, title, className)
{	var el = document.createElement(type);
	if (id) { el.id = id; }
	if (title) { el.title = title; }
	if (className) { el.className = className; }
	return el;
}

function advSearchFncns()
{	if (document.getElementById('advanced_query'))
	{	var fields = ['term', 'gp'];
		if (document.getElementById('div-gp') && document.getElementById('div-term'))
		{	var term = document.getElementById('div-gp');
			var gp = document.getElementById('div-term');
			YAHOO.util.Event.addListener(term, "click", checkSearchType, term, true);
			YAHOO.util.Event.addListener(gp, "click", checkSearchType, gp, true);
		}
		checkSearchTypeStatus('term');
	}
}

function startToggles()
{	var metaList = document.getElementsByTagName('META');
	for(var i=0;i<metaList.length;i++)
	{	if (metaList[i].name=='html_url')
		{	pathToDir=metaList[i].content;
			break;
		}
	}
	if (document.getElementById('results'))
	{	defToggle();
		listToggle();
	}
	if (document.getElementById('xrefs'))
	{ xrefToggle();
	}
//	if (document.getElementById('seqToggle'))
//	{	seqToggle();
//	}
//	
	filterToggle();
}

function defToggle()
{	var sr = document.getElementById('results');
	var defs = sr.getElementsByTagName('P');
	for (var i=0; i<defs.length;i++)
	{	if (defs[i].className.indexOf('def') != -1 && defs[i].id)
		{	var togShow = makeLink('#' + defs[i].id, defs[i].id + "toggle", '[show def]', 'Show term definition', 'def-link');
			var togHide = makeLink('#' + defs[i].id, defs[i].id + "toggle-hide", '[hide def]', 'Hide term definition', 'def-link');
			togShow.onclick = function()
			{	switchVisUrl(this);
				changeToggleText(this, 'inline');
				return false;
			}
			togHide.onclick = function()
			{	switchVisUrl(this);
				changeToggleText(this, 'inline');
				return false;
			}

			var par = defs[i].parentNode;
			var links = par.getElementsByTagName('A');
			for (var j=0; j<links.length;j++)
			{	if (links[j].className == 'name')
				{	insertAfter(togShow, links[j].nextSibling);
					insertAfter(togHide, togShow);
					break;
				}
			}
			if (defs[i].className.indexOf('src') == -1)
			{	togHide.style.display = 'none';
				defs[i].style.display = 'none';
			}
			else
			{	togShow.style.display = 'none';
			}
		}
	}
}

function insertAfter(node, referenceNode) {
	referenceNode.parentNode.insertBefore(node, referenceNode.nextSibling);
}

function changeToggleText(url, display)
{	var id = url.href.match(/#(\w.+)/)[1];
	var showMe = document.getElementById(id + 'toggle');
	var hideMe = document.getElementById(id + 'toggle-hide');
	if (showMe && hideMe)
	{	if (!display)
		{ display = ''; }
		if (showMe.style.display == 'none')
		{	showMe.style.display = display;
			hideMe.style.display = 'none';
		}
		else
		{	hideMe.style.display = display;
			showMe.style.display = 'none';
		}
	}
}

function listToggle()
{	var sr = document.getElementById('results');
	var uls = sr.getElementsByTagName('UL'); // find any lists in the results
	if (uls)
	{	for (var i=0; i<uls.length;i++)
		{	if (uls[i].id)
			{	var lis = uls[i].getElementsByTagName('LI');
				var lisl = lis.length - 2;
				var tog = document.createElement('LI');
				var togA = makeLink('#' + uls[i].id, '', 'and ' + lisl + ' more',  'View all synonyms', '');
				togA.onclick = function()
				{	showLis(this);
					return false;
				}
				tog.appendChild(togA);
				for (var j=2; j<lis.length;j++)
				{	lis[j].style.display = 'none';
				}
				uls[i].insertBefore(tog,lis[2]);
			}
		}
	}
}

function showLis(url)
{	var id = url.href.match(/#(\w.+)/)[1];
	var ul = document.getElementById(id);
	if (ul)
	{	var lis = ul.getElementsByTagName('LI');
		for (var j=0; j<lis.length;j++)
		{ lis[j].style.display = 'inline'; }
		lis[2].style.display = 'none';
	}
}

/*
function seqToggle()
{	var seqlink = document.getElementById('seqToggle');
	var barseqlink = document.getElementById('barSeqToggle');

	if (seqlink && barseqlink)
	{	seqlink.onclick = function()
		{	switchVisUrl(this);
			return false;
		}
		barseqlink.onclick = function()
		{	var id = barseqlink.href.match(/#(\w.+)/)[1];
			if (id.style.display == 'none')
			{	switchVis(id);
			}
		}
		switchVisUrl(seqlink);
	}
}
*/
function xrefToggle()
{	var dts = document.getElementById('xrefs').getElementsByTagName('DT');
	for (var i=0; i<dts.length; i++)
	{ dtLinkToggle(dts[i], 'cross references', 1); }
}

function filterToggle()
{	var toggle = document.getElementById('filterToggle');
	var section = document.getElementById('filterDiv');
	if (toggle && section)
	{	var togA = makeElement('A', '', 'Set or reset filters', 'arrow-img');
		togA.href = '#filterDiv';
		var togImg = makeElement('IMG', 'filterDivImg', "View filters", '');
		togImg.src = pathToDir+"/images/toggle-open.gif";
		togA.onclick = function()
		{	switchImg(this, 'filters');
			return false;
		}
		togA.appendChild(togImg);
		toggle.insertBefore(togA, toggle.firstChild);
//		preSwitchImg(togA, 'filters');
	}
}

function dtLinkToggle(dt, linkName, checkLis)
{	var links = dt.getElementsByTagName('A');
	for (var a=0; a<links.length; a++)
	{	links[a].onclick = function()
		{	switchImg(this, linkName);
			return false;
		}
		preSwitchImg(links[a], linkName, checkLis);
	}
}

function switchImg(url, linkName)
{	var id = url.href.match(/#(\w.+)/)[1];
	var idDiv = document.getElementById(id);
	var i = document.getElementById(id + 'Img');
	if (idDiv && i)
	{	if (!linkName) { linkName = 'section'; }
		if (idDiv.style.display == 'none')
		{	idDiv.style.display = '';
			i.src = pathToDir+"/images/toggle-open.gif"
			i.title = "Hide " + linkName;
		}
		else
		{	idDiv.style.display = 'none';
			i.src = pathToDir+"/images/toggle-closed.gif";
			i.title = "Show " + linkName;
		}
	}
}

function preSwitchImg(url, linkName, checkLis)
{	var id = url.href.match(/#(\w.+)/)[1];
	var idDiv = document.getElementById(id);
	var i = document.getElementById(id + 'Img');
	var switchMe = 1;
	if (checkLis && checkLis == 1)
	{	var lis = idDiv.getElementsByTagName('LI');
		if (lis.length < 3)
		{	switchMe = 0;
		}
	}

	if (idDiv && i && switchMe == 1)
	{	if (!linkName) { linkName = 'section'; }
		if (idDiv.style.display == 'none')
		{	idDiv.style.display = '';
			i.src = pathToDir+"/images/toggle-open.gif"
			i.title = "Hide " + linkName;
		}
		else
		{	idDiv.style.display = 'none';
			i.src = pathToDir+"/images/toggle-closed.gif";
			i.title = "Show " + linkName;
		}
	}
}

function switchVisUrl(url)
{	var id = url.href.match(/#(\w.+)/)[1];
	switchVis(id);
}

function switchVis(obj) {
	var el = document.getElementById(obj);
	el.style.display = (el.style.display != 'none' ? el.style.display = 'none' : el.style.display = '');
}

function initFormHilite()
{	if (document.getElementById('frontForm'))
	{	document.frontForm.query.focus();
		for (var i=0; i < document.frontForm.search_constraint.length; i++)
		{	document.frontForm.search_constraint[i].onclick = function()
			{	hilite();
			}
		}
		hilite();
	}
	else return false;
}

function hilite()
{	// find all the inputs with name "search_constraint"
	for (var i=0; i < document.frontForm.search_constraint.length; i++)
	{	if (document.frontForm.search_constraint[i].checked)
		{	document.frontForm.search_constraint[i].parentNode.className = "formHilite";
		}
		else
		{	document.frontForm.search_constraint[i].parentNode.className = "";
		}
	}
	return true;
}

function initResultsTable()
{	var results = document.getElementById("results");
	if (results)
	{	checkBoxes = document.getElementsByName("item");
		for (var i = 0; i < checkBoxes.length; i++)
		{	checkBoxes[i].onclick = DataTableCheckBox_Click;
		}

		var buttons = new Array();
		var all = makeElement('INPUT', 'selectallrows', 'Select or deselect all');
		all.setAttribute("type", "checkbox");
		all.onclick = SelectAllRows_Click;

		var check = makeElement('INPUT', 'checkall', 'Select all', 'button');
		var check2 = makeElement('INPUT', 'checkall2', 'Select all', 'button');
		check.setAttribute("value", "Select all");
		check2.setAttribute("value", "Select all");
		buttons.push(check);
		buttons.push(check2);

		var clear = makeElement('INPUT', 'clearall', 'Clear all', 'button');
		var clear2 = makeElement('INPUT', 'clearall2', 'Clear all', 'button');
		clear.setAttribute("value", "Clear all");
		clear2.setAttribute("value", "Clear all");
		buttons.push(clear);
		buttons.push(clear2);

		var len = buttons.length;
		for(var x=0;x<len;x++)
		{	buttons[x].setAttribute("type", "button");
			buttons[x].onclick = SelectAllRows_Click;
		}

		var tf = document.getElementsByTagName('TFOOT');
		for(var j=0;j<tf.length;j++)
		{	var tds = tf[j].getElementsByTagName('TD');
			tds[0].insertBefore(all, tds[0].firstChild);
			tds[1].insertBefore(clear, tds[1].firstChild);
			tds[1].insertBefore(check, tds[1].firstChild);
		}
		var th = document.getElementsByTagName('THEAD');
		for(var j=0;j<th.length;j++)
		{	var tds = th[j].getElementsByTagName('TH');
			tds[1].insertBefore(clear2, tds[1].firstChild);
			tds[1].insertBefore(check2, tds[1].firstChild);
		}
		numberChecked = 0;
	}
	else return false;
}

function SelectAllRows_Click()
{	var status = document.getElementById('selectallrows').checked;
	if(this.id.indexOf('clear') != -1 || this.id.indexOf('check') != -1)
	{	status = (this.id.indexOf('clear') == -1 ) ? true : false;
	}
	if (status)
	{	for (var i = 0; i < checkBoxes.length; i++)
		{	var oTR = checkBoxes[i].parentNode.parentNode;
			checkBoxes[i].checked = status;
			if (oTR.className)
			{	if (oTR.className.indexOf('selected') == -1) oTR.className += ' selected';
			}
			else oTR.className = 'selected';
		}
	}
	else
	{	for (var i = 0; i < checkBoxes.length; i++)
		{	checkBoxes[i].checked = status;
			if (checkBoxes[i].parentNode.parentNode.className == 'selected')
			{ checkBoxes[i].parentNode.parentNode.className = '';
			}
			else
			{	checkBoxes[i].parentNode.parentNode.className = checkBoxes[i].parentNode.parentNode.className.replace(/ selected/g, "")
			}
		}
	}
	document.getElementById('selectallrows').checked = status;
	numberChecked = (status) ? checkBoxes.length : 0;
}

function DataTableCheckBox_Click()
{	var oTR = this.parentNode.parentNode;
	if(this.checked) 
	{	if(oTR.className) oTR.className += ' selected';
		else oTR.className = 'selected';
		numberChecked++;
	}
	else 
	{	if (oTR.className == 'selected') oTR.className = '';
		else
		{ oTR.className = oTR.className.replace(/ selected/g, ""); }
		numberChecked--;
	}
	if (document.getElementById('selectallrows'))
	{	document.getElementById('selectallrows').checked = (numberChecked == checkBoxes.length) ? true : false;
	}
}

function checkSearchType(e, type) {
	var searchType = this.id.substring(4);
	checkSearchTypeStatus(searchType);
}

function checkSearchTypeStatus(sType) {
// find the status of the caller of the event
	if (document.getElementById('sc-' + sType).checked == false)
	{	// don't need to do anything if the sType is true
		document.getElementById('sc-' + sType).checked = true;
	}
	var divs = document.getElementById('searchType').getElementsByTagName('DIV');
	for (var i=0; i<divs.length; i++)
	{	if (divs[i].id == 'div-' + sType) // this is the active div
		{ divs[i].className = ''; }
		else 
		{ divs[i].className = 'deselected'; }
	}
	if (document.getElementById('filters-gp'))
	{	var fgp = document.getElementById('filters-gp');
		(sType == 'gp') ? fgp.style.display = 'block' : fgp.style.display = 'none';
	}
}

function addressMaker()
{	var addrs = document.getElementsByTagName("span");
	for (var i = 0; i < addrs.length; i++)
	{	if (addrs[i].className == "addr")
		{	/*	there will be two or three parts; id, place and name */
			var id, place;
			var name = "";
			for (var j = 0; j < addrs[i].childNodes.length; j++)
			{	var p = addrs[i].childNodes[j];
				/* if p is a span, get the class and the contents */
				if (p.nodeName == 'SPAN')
				{	if (p.className == "id")
					{	id = p.firstChild.data;
					}
					else if (p.className == "place")
					{	place = p.firstChild.data;
					}
					else if (p.className == "name")
					{	name = p.firstChild.data;
					}
				}
			}
			if (id != "" && place != "")
			{	/* do the replacement */
				while (addrs[i].childNodes.length > 0)
				{	addrs[i].removeChild(addrs[i].childNodes[0]);
				}
				var link = document.createElement('a');
				link.href = "mailto:" + id + "@" + place;
				
				var txt;
				if (name.length != 0)
				{	txt = document.createTextNode(name);
					link.title = name;
				}
				else
				{	txt = document.createTextNode(id + "@" + place);
					link.title = id + "@" + place;
				}
				link.appendChild(txt);
				addrs[i].appendChild(link);
			}
		}
	}
}
