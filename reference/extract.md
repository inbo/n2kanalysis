# Extract the relevant coefficients

Extract the relevant coefficients

## Usage

``` r
extract(extractor, object, base, project)

# S4 method for class 'ANY,character'
extract(extractor, object, base, project)

# S4 method for class 'ANY,n2kInla'
extract(extractor, object, base = NULL, project = NULL)

# S4 method for class 'ANY,n2kModelImputed'
extract(extractor, object, base = NULL, project = NULL)
```

## Arguments

- extractor:

  the extractor function

- object:

  the `n2kModel` object

- base:

  the optional base location of the object

- project:

  the optional subdirectory

## Value

the relevant coefficients
