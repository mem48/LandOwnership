mapboxgl.accessToken = 'pk.eyJ1IjoiZmFsY3MiLCJhIjoiY2w3MGR1YTIwMGQ4czNwcXliNnllazg1MiJ9.yeTfnwFemnZaOteDIw_iMg'
// Setup Map
const map = new mapboxgl.Map({
container: 'map', 
style: 'mapbox://styles/mapbox/streets-v11',
center: [0, 52], 
zoom: 6,
maxZoom: 16,
minZoom: 6

});
 
// Add controls to the map.
map.addControl(new mapboxgl.NavigationControl());
map.addControl(new mapboxgl.AttributionControl({
customAttribution: 'Contains OS data © Crown copyright 2022'
}));
map.addControl(new mapboxgl.GeolocateControl({
positionOptions: {
enableHighAccuracy: true
},
trackUserLocation: true
})
,'top-right');
map.addControl(new mapboxgl.ScaleControl({
  maxWidth: 80,
  unit: 'metric'
}),'bottom-right');
    
map.on('load', function() {
map.addSource('inspire', {
	'type': 'vector',
	'tiles': [
	'https://storage.googleapis.com/vector_tiles/inspire_full/{z}/{x}/{y}.pbf'
	],
	'minzoom': 6,
	'maxzoom': 16
});

map.addSource('landowners', {
	'type': 'vector',
	'tiles': [
	'https://www.wisemover.co.uk/tiles/landowners/{z}/{x}/{y}.pbf'
	],
	'minzoom': 6,
	'maxzoom': 16
});

toggleLayer('landowners');
toggleLayer('inspire');



});

// Click on inspire
map.on('click', 'inspire', function (e) {
var coordinates = e.lngLat;
var INSPIREID = e.features[0].properties.INSPIREID;
var local_authority = e.features[0].properties.local_authority;
var area = e.features[0].properties.area;

var description = '<p> INSPIRE ID: ' + INSPIREID + '</p>' +
'<p> Local Authority: ' + local_authority + '</p>' +
'<p> Area: ' + area + '</p>';
 

new mapboxgl.Popup()
.setLngLat(coordinates)
.setHTML(description)
.addTo(map);
});
 
// Change the cursor to a pointer when the mouse is over the places layer.
map.on('mouseenter', 'landowners', function () {
map.getCanvas().style.cursor = 'pointer';
});
 
// Change it back to a pointer when it leaves.
map.on('mouseleave', 'landowners', function () {
map.getCanvas().style.cursor = '';
});

// Change the cursor to a pointer when the mouse is over the places layer.
map.on('mouseenter', 'inspire', function () {
map.getCanvas().style.cursor = 'pointer';
});
 
// Change it back to a pointer when it leaves.
map.on('mouseleave', 'inspire', function () {
map.getCanvas().style.cursor = '';
});


function toggleLayer(layerName){
  var checkBox = document.getElementById(layerName.concat('checkbox'));
  // If the checkbox is checked add the layer to the map
  if (checkBox.checked === true){
    switch(layerName) {
      case 'inspire':
        map.addLayer({
            'id': 'inspire',
            'type': 'fill',
            'source': 'inspire',
            'source-layer': 'inspire',
            'paint': {
              'fill-color': 'rgba(188,128,189, 0.3)'
            }
        });
      case 'landowners':
        switchLayer('landowners');
        break;
      default:
        console.log('unknown layer selected');
    } 
  } else {
    if (map.getLayer(layerName)) map.removeLayer(layerName);
  }
}



function switchLayer(layer) {
  
  var checkBox = document.getElementById('landownerscheckbox');
  var layerId = document.getElementById("layerinput").value;
  var layers = map.getStyle().layers;
  
  if (checkBox.checked === true){
    if (map.getLayer('landowners')) map.removeLayer('landowners');
  
    switch(layerId) {
      case 'Category':

        map.addLayer({
            'id': 'landowners',
            'type': 'circle',
            'source': 'landowners',
            'source-layer': 'landowners',
            'paint': {
              // make circles larger as the user zooms from z12 to z22
              'circle-radius': {
                'base': 2.5,
                'stops': [
                  [8, 3],
                  [22, 180]
                ]
              },
              'circle-stroke-width': 1,
              "circle-color": [
          			'match',
          			['get', 'Category'],
          			'Co-operative Society (Company)','#a6cee3',
                'Co-operative Society (Corporate Body)','#1f78b4',
                'Community Benefit Society (Company)','#fdbf6f',
                'Community Benefit Society (Corporate Body)','#ff7f00',
                'Housing Association Co-operative Society (Company)','#e5f5f9',
                'Housing Association Co-operative Society (Corporate Body)','#ccece6',
                'Housing Association Community Benefit Society (Company)','#99d8c9',
                'Housing Association Community Benefit Society (Corporate Body)','#66c2a4',
                'Housing Association Registered Society (Company)','#41ae76',
                'Housing Association Registered Society (Corporate Body)','#238b45',
                'Housing Association/Society (Company)','#238b45',
                'Housing Association/Society (Corporate Body)','#00441b',
                'Industrial and Provident Society (Company)','#fb9a99',
                'Industrial and Provident Society (Corporate Body)','#e31a1c',
                'Local Authority','#cab2d6',
                'County Council','#6a3d9a',
                'Registered Society (Company)','#ffed6f',
                'Registered Society (Corporate Body)','#ffffb3',
                'Unlimited Company','#d7b5d8',
                'Corporate Body','#df65b0',
                'Limited Company or Public Limited Company','#dd1c77',
                'Limited Liability Partnership','#980043',
          			/* other */ '#e0e0e0'
          			]
            }
        });
        
        break;
      case 'geocode_type':
        map.addLayer({
            'id': 'landowners',
            'type': 'circle',
            'source': 'landowners',
            'source-layer': 'landowners',
            'paint': {
              // make circles larger as the user zooms from z12 to z22
              'circle-radius': {
                'base': 2.5,
                'stops': [
                  [8, 3],
                  [22, 180]
                ]
              },
              'circle-stroke-width': 1,
              "circle-color": [
          			'match',
          			['get', 'geocode_type'],
          			'Address','#a6cee3',
                'RoadBlock','#1f78b4',
          			/* other */ '#e0e0e0'
          			]
            }
        });
        break;
      default:
      console.log("Unknown layer");
    }
  }
}