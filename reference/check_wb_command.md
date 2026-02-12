# Check for Connectome Workbench

Verifies that `wb_command` is available. If `wb_path` is `NULL`, checks
`ciftiTools` default and system PATH.

## Usage

``` r
check_wb_command(wb_path = NULL)
```

## Arguments

- wb_path:

  Optional explicit path to `wb_command`.

## Value

Path to `wb_command` executable.
