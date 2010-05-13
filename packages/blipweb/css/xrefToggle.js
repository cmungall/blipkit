function xrefToggle()
{	// go to section extRefs
	// each dt has a dd with a list in it
	// choose to show or hide the dd
	var dts = document.getElementById('xrefs').getElementsByTagName('DT');
	for (var i=0; i<dts.length; i++)
	{	var links = dts[i].getElementsByTagName('A');
		for (var a=0; a<links.length; a++)
		{	links[a].onclick = function()
			{	switchImg(this);
				return false;
			}
			preSwitchImg(links[a]);
		}
	}
}

function switchImg(url)
{	var id = url.href.match(/#(\w.+)/)[1];
	var idDiv = document.getElementById(id);
	var i = document.getElementById(id + 'Img');
	if (idDiv && i)
	{	if (idDiv.style.display == 'none')
		{	idDiv.style.display = 'block';
			i.src = "/amigo/images/ominus.png"
			i.title = "Hide cross references";
		}
		else
		{	idDiv.style.display = 'none';
			i.src = "/amigo/images/plus.png";
			i.title = "Show cross references";
		}
	}
}

function preSwitchImg(url)
{	var id = url.href.match(/#(\w.+)/)[1];
	var idDiv = document.getElementById(id);
	var i = document.getElementById(id + 'Img');
	var lis = idDiv.getElementsByTagName('LI');
	if (lis.length > 2)
	{	if (idDiv && i)
		{	if (idDiv.style.display == 'none')
			{	idDiv.style.display = 'block';
				i.src = "/amigo/images/ominus.png"
				i.title = "Hide cross references";
			}
			else
			{	idDiv.style.display = 'none';
				i.src = "/amigo/images/plus.png";
				i.title = "Show cross references";
			}
		}
	}
}
