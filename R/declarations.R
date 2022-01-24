

get_package_name <- function() {
  toupper(desc::desc_get("Package"))
}

get_src_files <- function() {
  src_files <- fs::dir_ls("csrc/src/")
}

get_declarations <- function() {
  src_files <- get_src_files()
  decors <- decor::cpp_decorations(files = src_files)
  subset(decors, decoration == "torch::export")
}


