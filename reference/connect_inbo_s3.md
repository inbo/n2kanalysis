# Set environment variables for INBO S3 bucket

Before running this function you must have an `.aws` folder in your home
directory with a `credentials` file containing the credentials for the
INBO shared infrastructure. Run the `aws assume role` command to get the
credentials for the INBO shared infrastructure before running this
function.

## Usage

``` r
connect_inbo_s3()
```
