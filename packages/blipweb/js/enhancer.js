function addressMaker()
{	var addrs = document.getElementsByTagName("span");
	for (var i = 0; i < addrs.length; i++)
	{	if (addrs[i].className == "addr")
		{	
			/*	there will be two or three parts; id, place and name */
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

//	what we need our function to do
//	get all the uls with an id tag
function toggleNodes()
{	var as = document.getElementById('tree').getElementsByTagName('a');
	for (var a = 0; a < as.length; a++)
	{	if (as[a].id)
		{	//	this is a toggle link
			//	the ID of the section to toggle is in the href of the link
			//	also need to toggle the image
			as[a].onclick = function()
			{	toggleMe(this);
				return false;
			}
			toggleMe(as[a]);
		}
	}
}

function toggleMe(x)
{	var id = x.href.match(/#(\w.+)/)[1];
	var ul = document.getElementById(id);
	var i = document.getElementById(id + 'Img');
	if (ul && i)
	{	if (ul.style.display == 'none')
		{	ul.style.display = 'block';
			i.src = "images/minus.gif"
		}
		else
		{	ul.style.display = 'none';
			i.src = "images/plus.gif"
		}
	}
}

function initialise()
{	addressMaker();
	toggleNodes();
}

if (document.getElementById && document.createElement) {
	window.onload = initialise;
}

