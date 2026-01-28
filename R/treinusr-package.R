#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang %||%
## usethis namespace: end
NULL

# Global variables used in dplyr pipelines
utils::globalVariables(c(
  "timestamp",
  "position_lat",
  "position_long",
  "distance",
  "speed"
))
