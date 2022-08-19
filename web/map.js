mapboxgl.accessToken = 'pk.eyJ1IjoiZmFsY3MiLCJhIjoiY2w3MGR1YTIwMGQ4czNwcXliNnllazg1MiJ9.yeTfnwFemnZaOteDIw_iMg'
// Setup Map
const map = new mapboxgl.Map({
container: 'map', 
style: 'mapbox://styles/mapbox/light-v10',
center: [0, 52], 
zoom: 6,
maxZoom: 18,
minZoom: 6,
attributionControl: false

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
                'Co-operative Society (Corporate Body)','#a6cee3',
                'Community Benefit Society (Company)','#1f78b4',
                'Community Benefit Society (Corporate Body)','#1f78b4',
                'Housing Association Co-operative Society (Company)','#33a02c',
                'Housing Association Co-operative Society (Corporate Body)','#33a02c',
                'Housing Association Community Benefit Society (Company)','#33a02c',
                'Housing Association Community Benefit Society (Corporate Body)','#33a02c',
                'Housing Association Registered Society (Company)','#33a02c',
                'Housing Association Registered Society (Corporate Body)','#33a02c',
                'Housing Association/Society (Company)','#33a02c',
                'Housing Association/Society (Corporate Body)','#33a02c',
                'Industrial and Provident Society (Company)','#b2df8a',
                'Industrial and Provident Society (Corporate Body)','#b2df8a',
                'Local Authority','#6a3d9a',
                'County Council','#6a3d9a',
                'Registered Society (Company)','#ff7f00',
                'Registered Society (Corporate Body)','#ff7f00',
                'Unlimited Company','#cab2d6',
                'Corporate Body','#b15928',
                'Limited Company or Public Limited Company','#e31a1c',
                'Limited Liability Partnership','#fb9a99',
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
          			'Address','#4daf4a',
                'AdminDivision1','#bd0026',
                'AdminDivision2','#f03b20',
                'AdminDivision3','#fd8d3c',
                'CountryRegion','#fecc5c',
                'PopulatedPlace','#ffffb2',
                'Postcode1','#377eb8',
                'RoadBlock','#e41a1c',
                'RoadIntersection','#f781bf',
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