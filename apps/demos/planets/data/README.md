# The maps of the Planets demo

Each is a map of the whole globe, east to the right and north at the top,
twice as wide as it is high, made 1024 by 512 from its source and kept as
a `.cpm` of 16 bits a pixel, packed. They are works of the United States
government, NASA and the USGS, and are in the public domain.

* `earth.cpm`. NASA Earth Observatory, Blue Marble Next Generation with
  topography and bathymetry, December 2004. From
  `eoimages.gsfc.nasa.gov/images/imagerecords/73000/73909/world.topo.bathy.200412.3x5400x2700.jpg`,
  5400 by 2700.
* `moon.cpm`. NASA, GSFC and Arizona State University, the Lunar
  Reconnaissance Orbiter Camera wide angle mosaic of June 2013, a grey
  map. The 1024 sample of the USGS Astrogeology Science Center,
  `moon_lro_lroc-wac_mosaic_global_1024.jpg`.
* `mars.cpm`. NASA, JPL and the USGS, the Viking colour mosaic on the
  MDIM 2.1 base. The 1024 sample of the USGS Astrogeology Science
  Center, `mars_viking_mdim21_clrmosaic_global_1024.jpg`.
* `jupiter.cpm`. NASA, JPL and the Space Science Institute, the Cassini
  map of Jupiter of December 2000, PIA07782, 3601 by 1801.

* `pluto.cpm`. NASA, the Johns Hopkins University Applied Physics
  Laboratory and the Southwest Research Institute, the New Horizons map
  of Pluto in extended color, PIA19956, 5999 by 3000. The spacecraft
  flew past the once, and what it did not see lit is black on the map,
  so the globe is dark where nothing is known.

To make one: resample the source to 1024 by 512, save it as a TGA that
is not packed, 24 bits a pixel, and `tocpm -f 16 -l file.tga`.
