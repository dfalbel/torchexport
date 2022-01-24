
# torchexport

<!-- badges: start -->
<!-- badges: end -->

The goal of torchexport is to generate code to handle errors and common caveats
when writing C++ extensions for torch.
In general, you won't need to use this package directly as it's called automatically
in the template CMake file.

## Installation

You can install the development version of torchexport from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("dfalbel/torchexport")
```

## Example

Annotate the function you want to export with `[[ torch::export ]]`:

``` r
// [[ torch::export ]]
void* c_lltm_forward(void* input,
                     void* weights,
                     void* bias,
                     void* old_h,
                     void* old_cell) {
  return make_raw::TensorList(lltm_forward(
      from_raw::Tensor(input),
      from_raw::Tensor(weights),
      from_raw::Tensor(bias),
      from_raw::Tensor(old_h),
      from_raw::Tensor(old_cell)
  ));
}
```

Calling `torchexport::export()` will generate `csrc/src/exports.cpp` and `csrc/include/<<NAME>>/exports.h` with definitions that correctly handle cross-compile
exceptions as well as import/export declarations.
