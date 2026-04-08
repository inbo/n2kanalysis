# Display a message.

This is a short cut for `if(verbose) message(x)`.

## Usage

``` r
display(verbose, message, linefeed = TRUE)
```

## Arguments

- verbose:

  A logical. When `TRUE` print the message. When `FALSE` do nothing.

- message:

  a vector passed to [`message()`](https://rdrr.io/r/base/message.html).

- linefeed:

  A logical. When `TRUE` append a newline character at the end of the
  message.
