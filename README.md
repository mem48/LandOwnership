# LandOwnership
Analysis of land ownership data in England and Wales

Insipred by the the book and blog [Who Owns Englang](https://whoownsengland.org/) this repository contains analysis of [Land Registry data](https://landregistry.data.gov.uk/) about Land Onwesrhip in England and Wales.

The code focuses on two main objectives:

1. Making the INSPIRE polygons more accessible
2. Geocoding the UK and Oversees ownership data

## Mapping the INSIPRE polygons

The [INSPIRE polygons](https://www.gov.uk/guidance/inspire-index-polygons-spatial-data) show the freehold land in England and Wales. Thye are published as Open Data with a few [conditions](https://use-land-property-data.service.gov.uk/datasets/inspire#use_the_data). But as you might imagine every parcel of land in England and Wales is a massive dataset so it is not that easy to actually use. There was a [viewing service](https://www.data.gov.uk/data/map-preview?e=1.74944&n=60.8433&s=49.9553&url=http%3A%2F%2Finspire.landregistry.gov.uk/inspire/ows?Service=WMS&Request=Getcapabilities) but is has now been disabled and was pretty awful anyway.

So the first task was to build a modern vector tile set for Inspire polygons. I also decided to do some light cleaning of the data.

### Clenaing the polygons
Land registry maps are often digitised versions of old paper maps, because of this they are often split into grids where a property crossed the boundary of one paper map to another. I wrote code to detect straight boundaries that align with the 500m grids of the British National Grid and remove them. 

<figure>
<img src='images/grid_detection.JPG'/>
<figcaption align = "center">
<b>Polygons boarders that align with the grid have been detected and highlighted in red.</b>
</figcaption>
</figure>


While this is not prefect, the INSIPRE Polygon data is very messy, it does help clean up the data and make it clearer where large titles are.

Once the polygons have been cleaned and duplicates have been removed each Local Authorities data can be combined into a single 24 GB geojson. I also created smaller files containing just the polygons greater than 100 acres and 10 acres.

I then built these into a tileset. As showing all the polygons in the country is not possible even for Vector Tiles I chose just to map the large polygons when zoomed out and then add in smaller polygons the closer in you zoom.

<img src='images/zoomedout.JPG'/>
<figcaption align = "center">
<b>When zoomed out only the largest polygons are shown</b>
</figcaption>


<figure>
<img src='images/zoomedin.JPG'/>
<figcaption align = "center">
<b>Zoom in a little and you can see all the polygons</b>
</figcaption>
</figure>

