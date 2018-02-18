ors-measure
===========

Provides a few Racket functions for calculating Omnidirectional Relief and Steepness (ORS) for any coordinate on Earth, so long as a grid of Digital Elevation Model data (such as SRTM) is provided.

What is Omnidirectional Relief and Steepness?
---------------------------------------------

This question is best answered by reading the papers which first introduced the measure:
- [http://www.peaklist.org/spire/theory/paper.pdf](A new topographic functional): Edward Earl, David Metzler
- [https://www.degruyter.com/view/j/quageo.2015.34.issue-4/quageo-2015-0033/quageo-2015-0033.xml](Cloud-Capped Towers: Capturing Terrain Characteristics Using Topographic Functionals): Edward Earl, David Metzler

ORS is essentially a way to compare how steep the terrain is surrounding the point your standing at, and how 'impressive' or expansive the view is if you were to do a full 360 turn at that point. If you are standing in the middle of a flat expanse, you will have a low ORS no matter the elevation: a point in Kansas will have a similarly low ORS to a point on the Tibetan plateau. However, a summit with low prominence or low elevation will not necessarily have a low ORS: some classic examples of this contrast by Earl and Metzler include Hozomeen Mountain and Mount Index.

What data do I need to calculate it?
------------------------------------

This module is geared to work with Digital Elevation Model data, with each point in the grid containing an elevation in *meters*. The functions within have been tested with SRTM GL1 grids in ESRI ASCII format, which is publicly available at many places on the web, including [http://www.opentopography.org/](OpenTopography).
