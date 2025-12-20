# Package load hook
.onLoad <- function(libname, pkgname) {
  # Execute any registered on_load hooks
  rlang::run_on_load()
}
