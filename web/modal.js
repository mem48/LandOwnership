// Get the modal
var modal = document.getElementById("myModal");

// Get the <span> element that closes the modal
var span = document.getElementsByClassName("close")[0];

// When the user clicks on <span> (x), close the modal
span.onclick = function() {
  modal.style.display = "none";
};

// When the user clicks anywhere outside of the modal, close it
window.onclick = function(event) {
  if (event.target == modal) {
	modal.style.display = "none";
  }
};


// How map triggers the modal 
// On click open modal
map.on('click', 'landowners', function(e) {
  
  // Block Modal when clicking on other layers
  
  let f = map.queryRenderedFeatures(e.point);
  /*
  f = f.filter(function (el) {
    console.log("modal blocked" + f);
    return el.source != 'composite';
  });
  */
  
  //if (f.length == 1) {
    modal.style.display = "block";
    console.log("modal triggered");
	
    document.getElementById("Title").innerHTML = e.features[0].properties.Title;
    document.getElementById("Tenure").innerHTML = e.features[0].properties.Tenure;
    document.getElementById("Property_Address").innerHTML = e.features[0].properties.Property_Address;
    document.getElementById("Company_No").innerHTML = e.features[0].properties.Company_No;
    document.getElementById("Category").innerHTML = e.features[0].properties.Category;
    document.getElementById("geocoded_address").innerHTML = e.features[0].properties.geocoded_address;
    document.getElementById("geocode_type").innerHTML = e.features[0].properties.geocode_type;
    document.getElementById("Proprietor").innerHTML = e.features[0].properties.Proprietor;
    
  //} 
	
});