# remap 0.2.1.001

* Made updates to vignette and examples in documentation to accommodate
the jump to sf version 1.0.0.

# remap 0.2.1

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
