function toggleTable(div_id,hostname){

	plus = new Image();
	plus.src = "/OBDUI/images/plus-box.gif";
	minus = new Image();
	minus.src = "/OBDUI/images/min-box.gif";
	
	var detailsDiv = document.getElementById(div_id);
	var toggleImage = document.getElementById((div_id+'_image'));
	
	if (detailsDiv && toggleImage){
					
		if (detailsDiv.style.display == "none"){
			detailsDiv.style.display='table-cell';
			toggleImage.src=minus.src;
		} else {
			detailsDiv.style.display="none";
			toggleImage.src=plus.src;
		}			
	}
}

function openTable(div_id,hostname){
	var detailsDiv = document.getElementById(div_id);
	if (detailsDiv){
		if (detailsDiv.style.display == "none"){
			toggleTable(div_id,hostname);
		}
	}
	
	
}
