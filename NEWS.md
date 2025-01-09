# remap 0.3.2

## Patches

* Update ggplot geom_line size parameter to linewidth.

* Fixed gcc-UBSAN errors when finding distances for 0 points.

* Restructured package data to work with sf when sf isn't in search path.

# remap 0.3.1

## Minor changes

* Updated citation to [R Journal article](https://doi.org/10.32614/RJ-2023-004).

* Remove some suggested packages and simplified vignette.

# remap 0.3.0

## Major changes

* Added "se" option to the predict.remap function which allows a smooth 
calculation of an upper bound of combined model standard errors when regional 
model predict function returns standard errors rather than predictions. 
(Backwards compatible.)

## Minor changes

* Simplified weighted average calculation. Calculated values are unaltered
from previous version.

* Updated example data source to remove ftp website.

* Made updates to vignette and examples in documentation to accommodate
the jump to sf version 1.0.0.

# remap 0.2.1

## Minor changes

* remap now makes a model for a region using the min_n nearest observations
regardless of whether or not there are observations within a region. This
eliminates the case when discontinuities in the prediction surface appears
when smaller regions are used which may not contain any observations.
Previously, each region had to contain at least one observation to be
eligible for a regional model.

* Added Brennan Bean as package author.

* Changed citation to recent publication 
(https://digitalcommons.usu.edu/etd/8065/).

# remap 0.2.0
Final version of development, ready for CRAN submission.
