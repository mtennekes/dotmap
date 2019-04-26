// Pass the checkbox name to the function
function getCheckedBoxes(chkboxName) {
  var checkboxes = document.getElementsByName(chkboxName);
  var checkboxesChecked = [];
  // loop over them all
  for (var i=0; i<checkboxes.length; i++) {
	 // And stick the checked ones onto an array...
	 if (checkboxes[i].checked) {
		checkboxesChecked.push(checkboxes[i]);
	 }
  }
  // Return the array if it is non-empty, or null
  return checkboxesChecked.map(function(i) {return i.value;});
}

console.log(settings);

console.log(localhost);

function changeLayers(layer) {
	var id = getCheckedBoxes("layeritems")[0][5];
	
  var name = meta[id].name;
  var label = meta[id].label;
  var levels = meta[id].levels;

  
  //dotmap.setUrl('http://127.0.0.1/htmlserver/tiles_adam_' + name + '/{z}/{x}/{y}.png');
  //dotmap.setUrl('http://research.cbs.nl/colordotmap/tiles_adam_' + name + '/{z}/{x}/{y}.png');
  dotmap.setUrl(localhost + "/" + name + '/{z}/{x}/{y}.png');
	
	
  //document.getElementById("dotmapLegend").innerHTML = <div id = "leg2a" style = "display:block">
	
  div = document.getElementById("dotmapLegend");
	  
  div.innerHTML="<div style = 'display:block'><div class = 'legendtitle'><strong>" + label + "</strong></div></div>";
	
	for(var i=0; i <levels.length; i++){
		div.innerHTML+="<div><img class='legimage1' src='index_files/icons/icon_" + name + "_" + (i + 1) + ".png'/><span class = 'cel0'> " + levels[i] + "</span></div>";
	}
	
	  
	  
//for (i = 0; i < levels.length; i++) {
//  document.getElementById("lab" + (i + 1)).innerHTML = " " + levels[i];
//  document.getElementById("pixels" + (i+1)).src = "index_files/icons" + name + "_" + (i + 1) + ".png";
//}

//document.getElementById("varname").innerHTML = "<strong>" + label + "</strong>";
  //document.getElementById("i3").style.display = levels.length==2 ? "none" : "block";
}


function style_nbhd(feature) {
	return {
		weight: 1.25,
		opacity: 1,
		color: '#555555',
		fillOpacity: 0
	};
}	

function moveLabelsToMarkerPane(event) {
	var k = document.getElementsByClassName("leaflet-layer").length
	for (k = 0; k < 4; k++) {
		if (document.getElementsByClassName("leaflet-layer")[k].style.getPropertyValue("z-index") == "6" ) {
			document.getElementsByClassName("leaflet-marker-pane")[0].appendChild(
				document.getElementsByClassName("leaflet-layer")[k]);
			break;
		}
	}
}

// Determine if device is mobile phone (less than 8 inches)
var pdiag = Math.sqrt(Math.pow(window.screen.height, 2) + Math.pow(window.screen.width, 2)),
	idiag = pdiag / res.dpi(),
	is_mobile = idiag < 8;
if (is_mobile) {
	var collapse = true;
} else {
	var collapse = false;
}

// Map contributes
var mapLink = 
	'<a href="http://openstreetmap.org">OSM</a>',
	cartoDB = 
	'<a href="http://cartodb.com/attributions#basemaps">CartoDB</a>',
	CBS = 
	'<a href="https://www.cbs.nl">CBS</a>',
  PBL =
  '<a href="http://www.pbl.nl/">PBL</a>',
  CCdata =
  '<a href="https://claircitydata.cbs.nl/">ClairCity data portal</a>';
	
// Map layers
var base = L.tileLayer(
  'http://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}' + ((L.Browser.retina ||  is_mobile) ? '@2x' : '') + '.png', {
//	'http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png'
    attribution: cartoDB + ', ' + mapLink,
	minZoom: settings.zmin,
	maxZoom: settings.zmax,
	opacity: 0.5,
	zIndex: 1
}),
base2 = L.tileLayer(
  'http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}.png', {
  attribution: 'Tiles &copy; Esri &mdash; Source: Esri, i-cubed, USDA, USGS, AEX, GeoEye, Getmapping, Aerogrid, IGN, IGP, UPR-EGP, and the GIS User Community',
	minZoom: settings.zmin,
	maxZoom: settings.zmax,
	opacity: 1,
	zIndex: 1
}),
dotmap = L.tileLayer(
  //'http://127.0.0.1/adam/income/{z}/{x}/{y}.png', {
//'http://localhost/adam/hh_type/{z}/{x}/{y}.png'	
//	localhost + "/" + meta[0].name + '/{z}/{x}/{y}.png'), {
	'http://localhost/adam/hh_type/{z}/{x}/{y}.png', {
	attribution: '&copy; ' + CBS + ", " + PBL + ", " + CCdata,
	minZoom: settings.zmin,
	maxZoom: settings.zmax,
	zIndex: 5
}),
nbhd = L.geoJson(shp, {
	style: style_nbhd,
	clickable:false,
	zIndex: 2
}),
labels = L.tileLayer(
'http://{s}.basemaps.cartocdn.com/light_only_labels/{z}/{x}/{y}' + ((L.Browser.retina ||  is_mobile) ? '@2x' : '') + '.png', {
	minZoom: settings.zmin,
	maxZoom: settings.zmax,
	zIndex: 6,
	className: "test"
});

// Move labels layer up (op top of everything)
labels.on('load', moveLabelsToMarkerPane);


var primary = true;
var czoom = settings.zcurrent, // current zoom
	mzoom = czoom, // last zoom settings for mixed (zoom 7-14) legend
	dzoom = settings.zmax; // last zoom settings for dot legend (zoom 15-17)



var map = L.map('map', {
	center: [parseFloat(settings.y), parseFloat(settings.x)],
	zoom: czoom,
	maxBounds: [[parseFloat(settings.y1), parseFloat(settings.x1)], [parseFloat(settings.y2), parseFloat(settings.x2)]] // without buffer [[52.28, 4.75], [52.43, 5.02]]
});

// Add layers to map
var base_grp = L.layerGroup([base]).addTo(map);
var dotmap_grp = L.layerGroup([dotmap]).addTo(map);
var admin_grp = L.layerGroup([nbhd]).addTo(map);
var labels_grp = L.layerGroup([labels]).addTo(map);

// Create layer selection box
var overlays = {};
overlays[dotmap_text.boundaries] = admin_grp;
overlays[dotmap_text.labels] = labels_grp;

var baselays = {
  "Greyscale": base,
  "Satellite": base2
};


var LG = L.control.layers(baselays, overlays, {position: 'topleft', autoZIndex: false, collapsed: collapse}).addTo(map);
LGid = document.getElementsByClassName('leaflet-control-layers-list')[0];
var layersdiv = document.createElement('div');
layersdiv.innerHTML = document.getElementById('layerscb').innerHTML;
LGid.insertBefore(layersdiv, LGid.childNodes[0]);

// Create legend
var info = L.control();
info.onAdd = function (map) {
	this._div = L.DomUtil.create('div', 'info legend dotmaplegendclass'); // create a div with a class "info"
	this._div.id = "dotmapLegend";
	//this._div.innerHTML = document.getElementById('leg1').innerHTML;
	
	//this.update();
	return this._div;
};
info.addTo(map);


// Fill radio button
rform = document.getElementById("radioform");

for (i=0; i < meta.length; i++) {
  // Create an <input> element, set its type and name attributes
  var input = document.createElement("input");
  input.type = "radio";
  input.value = "radio" + i;
  input.name = "layeritems";
  input.checked = i==0;
  input.addEventListener("click", changeLayers);
  rform.appendChild(input);
  // Label
  rform.appendChild(document.createTextNode(meta[i].label));
  // Append a line break 
  rform.appendChild(document.createElement("br"));  
  //document.getElementById("radioform")
  //radiocontent += "<input type='radio' name='layeritems' onclick='changeLayers(this.value)' value='" + meta[i].name + "' " + (i==0 ? "checked" : "") + ">" + meta[i].label + "<br>"
}
//document.getElementById("radioform").



// Add warning box

/*
var warndiv = document.createElement('div');
warndiv.className = 'info legend dotmapwarnbox leaflet-control';
warndiv.id = 'warntext';
warndiv.style.display = "block";
warndiv.innerHTML = document.getElementById('info2').innerHTML;
document.getElementsByClassName('leaflet-right')[0].appendChild(warndiv);
document.getElementById('warnclose').addEventListener("click", function(){
	warnclosed = true;
    document.getElementById("warntext").style.display = "none";
});


var infoclosed = false,
	warnclosed = false;
*/

changeLayers(null);
