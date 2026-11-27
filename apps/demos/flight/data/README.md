# Flight's data

## The map of the moon

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

## A cow's moos

`moo1.wav` to `moo4.wav`: a cow lifted off the ground by a craft's beam, one
played at random each time, heard from its side and fading with distance.
Four moos cut from "Cow Moo" by DRAGON-STUDIO, published on Pixabay on 12
May 2025, pixabay.com/sound-effects/nature-cow-moo-339729/, under the
Pixabay Content License. Each runs from just before it starts to just after
it ends, faded in over 30ms and out over 200ms so neither end clicks, made
mono, 16 bits at 22050 a second, and brought to the same peak, 2dB under
full.
