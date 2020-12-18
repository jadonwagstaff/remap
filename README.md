# remap
**Regional Spatial Modeling with Continuous Borders**

Automatically creates separate regression models for different spatial regions. If regional 
models are continuous, the resulting prediction surface is continuous across the spatial 
dimensions, even at region borders.

## Installation
Install this package from the CRAN repository.

```
install.packages("remap")
```

Alternatively, use devtools to install the development version of this package.

To install devtools on R run:

```
install.packages("devtools")
```

After devtools is installed, to install the remap package on R run:

```
devtools::install_github("jadonwagstaff/remap")
```

## Functions

*remap* - Builds regional models able to make continuous predictions in space.

*redist* - Pre-calculates distances. Useful in cases where multiple models are made with the same data.

## Author
Jadon Wagstaff

## License
GPL-3
