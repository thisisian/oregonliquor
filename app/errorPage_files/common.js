function show(elementId, display)
{
	if (display)
	{
		document.getElementById(elementId).style.display = display;
	}
	else
	{
		document.getElementById(elementId).style.visibility = 'visible';
	}
}
function hide(elementId, display)
{
	if (display)
	{
		document.getElementById(elementId).style.display = 'none';
	}
	else
	{
		document.getElementById(elementId).style.visibility = 'hidden';
	}
}
function popupWindow(href, w, h, settings, offsett, offsetl) 
{
	offsett = (offsett) ? offsett:0;
	offsetl = (offsetl) ? offsetl:0;
	leftpos = (screen.width) ? (screen.width/2)-(w/2) : 0;
	toppos = (screen.height) ? (screen.height/2)-(h/2) : 0;
	settings = 'height='+h+',width='+w+',top='+(toppos+offsett)+',left='+(leftpos+offsetl)+','+settings;
	return window.open(href, 'popupwindow'+Math.floor(Math.random() * 10000), settings)
}
function setLocationCookie()
{	
	// check if checkbox is turned on
	if (document.getElementById("default").checked)
	{
		// set cookie
		createCookie("dsiLocation",document.getElementById("location").value,365);
	}
	else
	{
		// if cookie exists, unset it
		createCookie("dsiLocation","",365);	
	}
}
function validateSearch()
{
	var valid = true;
	if (document.getElementById("product").value == "" && document.getElementById("location").value == "")
	{
		alert("Please specify a product and/or a location.");
		document.getElementById("product").focus();
		valid = false;
	}
	return valid;
}
function createCookie(name,value,days) 
{
  if (days) 
  {
    var date = new Date();
    date.setTime(date.getTime()+(days*24*60*60*1000));
    var expires = "; expires="+date.toGMTString();
  }
  else expires = "";
  document.cookie = name+"="+escape(value)+expires+"; path=/";
}
function readCookie(name) 
{
  var nameEQ = name + "=";
  var ca = document.cookie.split(';');
  for(var i=0;i < ca.length;i++) 
  {
    var c = ca[i];
    while (c.charAt(0)==' ') c = c.substring(1,c.length);
    if (c.indexOf(nameEQ) == 0) return unescape(c.substring(nameEQ.length,c.length));
  }
  return null;
}
function displayFavLink(url, title)
{
	if (window.external) 
	{
  		document.write('<a href = "javascript:addToFav(\''+url+'\',\''+title+'\')");">Add To Favorites</a>'); 
  	} 
  	else  if (window.sidebar) 
  	{
  		document.write('<a href = "javascript:addToFav(\''+url+'\',\''+title+'\')");">Add Bookmark</a>'); 
 	} 
 	else if (window.opera && window.print) 
 	{	
   		document.write('<a href = "javascript:addToFav(\''+url+'\',\''+title+'\')");">Add Bookmark</a>');
 	} 
}
function addToFav(url, title) 
{
	if (window.sidebar) 
	{ // Mozilla Firefox Bookmark
		window.sidebar.addPanel(title, url,"");
	} 
	else if (window.external) 
	{ // IE Favorite
		window.external.AddFavorite( url, title); 
	}
	else if (window.opera && window.print) 
	{ // Opera Hotlist
		return true; 
	}
}