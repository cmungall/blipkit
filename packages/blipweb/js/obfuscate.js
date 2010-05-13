function obfuscate( domain, name, optionalText )
{
	document.write('<a href="mai' + 'lto:' + name + '@' + domain + '">');
	if (optionalText == "")
		document.write(name + '@' + domain);
	else
		document.write(optionalText);
	document.write('<\/a>');
}
