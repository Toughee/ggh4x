#' RGB Palette
#'
#' Palette function for \code{scale_colour/fill_rgb()}.
#'
#' @param x An \code{rgb_spec} vector with values between 0-1.
#'
#' @return A \code{character} vector with colours.
#' @export
#'
#' @examples
#' rgb_pal(rgb_spec(c(0, 1), c(0.5, 0.5), c(0.25, 0.75)))
rgb_pal <- function(x) {
  try_require("farver", "rgb_pal")
  if (!inherits(x, "rgb_spec")) {
    rlang::abort("Invalid palette input. Expected `rgb_spec` input.")
  }
  farver::encode_colour(as.matrix(x) * 255, from = "rgb")
}
