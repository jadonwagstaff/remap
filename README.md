# remap
**Regional Spatial Modeling with Continuous Borders**

Automatically creates separate regression models for different spatial regions. If regional 
models are continuous, the resulting prediction surface is continuous across the spatial 
dimensions, even at region borders.

See the R Journal publication below for more details:
>Wagstaff, J. & Bean, B. (2023). remap: regionalized models with spatially smooth predictions. *The R Journal, 14(4),* 160-178. <https://doi.org/10.32614/RJ-2023-004>

## Functions

*remap* - Builds regional models able to make continuous predictions in space.

*redist* - Pre-calculates distances. Useful in cases where multiple models are made with the same data.

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

## Author
Jadon Wagstaff

## License
GPL-3
