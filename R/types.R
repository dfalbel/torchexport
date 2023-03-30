types_env <- rlang::new_environment()
types_env$supported_types <- list()

export_type <- function(torch, lantern, c_type, rcpp) {
  type <- list(
    "lantern" = lantern,
    "c" = c_type,
    "rcpp" = rcpp
  )
  type <- list(type)
  names(type) <- torch
  types_env$supported_types <- append(types_env$supported_types, type)
  invisible(NULL)
}

export_type(
  "torch::Tensor",
  "Tensor",
  "void*",
  "torch::Tensor"
)
export_type(
  "std::vector<torch::Tensor>",
  "TensorList",
  "void*",
  "torch::TensorList"
)
export_type(
  "torch::TensorList",
  "TensorList",
  "void*",
  "torch::TensorList"
)
export_type(
  "torch::optional<torch::Tensor>",
  "optional::Tensor",
  "void*",
  "torch::optional::Tensor"
)
export_type(
  "std::string",
  "string",
  "void*",
  "torch::string"
)

