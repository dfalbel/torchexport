#' Generates code that exports code from C++ extensions
#'
#' This function is used by the CmakeList.txt file template to autogenerate
#' headers and declarations for exported functions in torch C++ extensions.
#'
#' @inheritParams pkgload::load_all
#'
#' @export
export <- function(path = pkg_path()) {
  withr::with_dir(path, {
    decls <- get_declarations()

    if (nrow(decls) == 0)
      return(invisible(NULL))

    decls <- lapply(decls$context, generate_decls)
    decls <- purrr::transpose(decls)

    export_cpp <- exports_cpp(decls$error_handled)
    export_h <- exports_h(decls$empty_decl, decls$wrapper)

    readr::write_lines(export_cpp, "csrc/src/exports.cpp")
    readr::write_lines(export_h, fs::path("csrc/include/", tolower(get_package_name()), "exports.h"))
  })

  invisible(NULL)
}

pkg_path <- pkgload::pkg_path


generate_decls <- function(context) {
  parsed <- decor::parse_cpp_function(context)
  list(
    error_handled = make_error_handled(parsed),
    wrapper = make_wrapper(parsed),
    empty_decl = make_empty_declaration(parsed)
  )
}


make_error_handled <- function(parsed) {
  glue_code("
<<make_declaration(parsed, macro = '', prefix = '')>>;
<<make_declaration(parsed)>> {
  try {
    <<make_return(parsed)>> <<parsed$name>>(<<make_call(parsed)>>);
  } <<get_package_name()>>_HANDLE_EXCEPTION
  <<if (parsed$return_type != 'void') paste('return (',parsed$return_type,') NULL')>>;
}
")
}

make_wrapper <- function(parsed) {
  glue_code("
<<make_declaration(parsed, macro = '', prefix = '', inline = TRUE)>> {
  <<if (parsed$return_type != 'void') 'auto ret = ' else ''>> _<<parsed$name>>(<<make_call(parsed)>>);
  host_exception_handler();
  <<if (parsed$return_type != 'void') 'return ret;' else ''>>
}
")
}

make_empty_declaration <- function(parsed) {
  glue_code("<<make_declaration(parsed)>>;")
}


glue_code <- function(..., .envir = parent.frame()) {
  glue::glue(..., .open = "<<", .close = ">>", .envir = .envir)
}

make_declaration <- function(parsed, prefix = "_", macro = NULL, inline = FALSE) {
  if (is.null(macro)) {
    macro <- glue_code("<<get_package_name()>>_API ")
  } else {
    macro <- macro
  }
  inline <- ifelse(inline, "inline ", "")
  glue_code("<<macro>><<inline>><<parsed$return_type>> <<prefix>><<parsed$name>> (<<make_signature(parsed)>>)")
}

make_signature <- function(parsed) {
  args <- parsed$args[[1]]
  args <- paste(args$type, args$name)
  paste(args, collapse = ", ")
}

make_call <- function(parsed) {
  args <- parsed$args[[1]]
  paste(args$name, collapse = ", ")
}

make_return <- function(parsed) {
  if (parsed$return_type == "void")
    ""
  else
    "return "
}
