# The map of Flight's moon

`moon.cpm` is a map of the whole Moon, east to the right and north at the
top, twice as wide as it is high, 512 by 256, kept as a `.cpm` of 16 bits a
pixel, packed. It is a work of the United States government, and in the
public domain: NASA, GSFC and Arizona State University, the Lunar
Reconnaissance Orbiter Camera wide angle mosaic of June 2013, a grey map,
the 1024 sample of the USGS Astrogeology Science Center,
`moon_lro_lroc-wac_mosaic_global_1024.jpg`.

That sample has columns at its edges that are not of the map, its first and
its last two, so a globe had a seam where they meet. They were made again,
row by row, a blend across the seam from the good column on one side to that
on the other. It was then made half the size, each pixel the mean of four.

To make one: resample the source to the size wanted, save it as a TGA that
is not packed, 24 bits a pixel, and `tocpm -f 16 -l file.tga`.
