

get_package_name <- function() {
  toupper(desc::desc_get("Package"))
}

get_src_files <- function() {
  src_files <- fs::dir_ls("csrc/src/")
}

types_file_name <- function() {
  name <- tolower(get_package_name())
  paste0(name, "/", name, "_types.h")
}

types_file_include <- function() {
  paste0('#include "', types_file_name(), '"')
}

has_types_file <- function() {
  fs::file_exists(fs::path("csrc/include/", types_file_name()))
}

get_declarations <- function() {
  src_files <- get_src_files()
  decors <- decor::cpp_decorations(files = src_files)
  decors <- subset(decors, decoration == "torch::export")

  # register new types
  register_types(decors$params)
  decors
}

register_types <- function(params) {
  for (param in params) {
    if (rlang::is_list(param) && rlang::is_named(param)) {
      if (!is.null(param$register_types)) {
        types <- eval(param$register_types)

        if (is.character(types)) {
          types <- list(types)
        }

        lapply(types, function(x) {
          do.call(export_type, as.list(x))
        })
      }
    }
  }
}
