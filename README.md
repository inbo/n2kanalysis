[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)
[![License](http://img.shields.io/badge/license-GPL--3-blue.svg?style=flat)](http://www.gnu.org/licenses/gpl-3.0.html)
[![Release](https://img.shields.io/github/release/qubyte/rubidium.svg)](https://github.com/inbo/n2kanalysis/releases)
[![wercker status](https://app.wercker.com/status/0dc9e0e76caa7b834a0ac6fa9c3ce908/s/master "wercker status")](https://app.wercker.com/project/bykey/0dc9e0e76caa7b834a0ac6fa9c3ce908)
[![codecov](https://codecov.io/gh/inbo/n2kanalysis/branch/master/graph/badge.svg)](https://codecov.io/gh/inbo/n2kanalysis)

# The n2kanalysis package

The `n2kanalysis` package constains the main infrastructure for the analysis of the Natura 2000 Monitoring. The import from the raw data into the analysis object is done by dedicated packages, one for each monitoring scheme.

## Rationale

`n2kanalysis` is part of the collection of [R packages](https://github.com/search?q=topic%3Anatura2000+org%3Ainbo&type=Repositories) created to analyse the data gathered during the Natura 2000 monitoring.

- [n2kanalysis](https://github.com/inbo/n2kanalysis): R package with generic functions for the analysis
- [watervogelanalysis](https://github.com/inbo/watervogelanalysis): R package to extract the raw data from the wintering bird survey database and prepare the analyses
- [abvanalysis](https://github.com/inbo/abvanalysis): R package to extract the raw data from the common breeding birds survey database and prepare the analyses
- [n2khelper](https://github.com/inbo/n2khelper): auxiliary functions used in the other packages
- [n2kresult](https://github.com/inbo/n2kresult): Liquibase scripts to setup the database in which the results of the analyses are stored
- [n2kupdate](https://github.com/inbo/n2kupdate): R package to read and write to the `n2kresult` database
- [Rn2k](https://github.com/inbo/Rn2k): Docker image with all the required dependencies to run the analyses

The main goal of `n2kanalysis` is to assist traceable analysis of monitoring data. The S4 classes in `n2kanalysis` allow to defines self-contained analysis objects. Besides the required data and definition on the model, the object also contains relevant meta-data. Each object contains two fingerprints: the file fingerprint and the status fingerprint. Both are [SHA-1](https://en.wikipedia.org/wiki/SHA-1) hashes based on the current object. The file fingerprint is based on the parts of the object which won't change during the analysis. E.g. most meta-data, the data and the definition of the model. The status fingerprint is based on both the file fingerprint and all relevants part of the object that do change during the fitting process. Publishing both fingerprints along with the results ensures an easy matching between results and the object from which they originated.

## Folder structure

The folder structure is that of a typical R packages with the mandatory `R` folder (definition of the functions) and `man` (helpfiles in Rd format). `inst` is an optional folder in which some auxiliary scripts are stored. The optional `test` folder contains the unit tests using the infrastructure from the `testthat` package.

```
n2kanalysis
|-- inst
|-- man
|-- R
|-- tests
   |-- testthat
```
