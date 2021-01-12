# Constructor RGB ---------------------------------------------------------

#' RGB specification
#'
#' This function creates a vector that stores values for red green and blue
#' channels in the RGB colourspace.
#'
#' @param r,g,b \code{numeric()} vectors with values for the red, green and blue
#'   channels respectively.
#'
#' @return An \code{rgb_spec}, \code{vctrs_rcrd} S3 class.
#' @export
#'
#' @examples
#' # Creating an `rgb_spec` vector
#' rgb <- rgb_spec(r = c(1, 2, 3), g = c(40, 50, 60), b = c(-700, -800, -900))
#'
#' # Rescaling vector to have values between 0-255
#' rgb <- scales::rescale(rgb, to = c(0, 255))
#'
#' # Rescale vector to the 0-1 range is the default
#' rgb <- scales::rescale(rgb)
#'
#' # Converting the rescaled vector to colours assumes 0-1 range
#' rgb_pal(rgb)
#'
#' # Example usage as colour aesthetic
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point(aes(colour = rgb_spec(mpg, cyl, drat)))
rgb_spec <- function(r = 0, g = 0, b = 0) {
  rgb <- vec_cast_common(r = r, g = g, b = b, .to = double())
  rgb <- vec_recycle_common(r = rgb$r, g = rgb$g, b = rgb$b)
  new_rgb_spec(r = rgb$r, g = rgb$g, b = rgb$b)
}

new_rgb_spec <- function(r = double(),
                         g = double(),
                         b = double()) {
  r <- vec_assert(r, double())
  g <- vec_assert(g, double())
  b <- vec_assert(b, double())
  new_rcrd(list(r = r, g = g, b = b),
           class = c("rgb_spec", "colour_spec"))
}

# Methods RGB -------------------------------------------------------------

# We need to be able to cast `double()` to `rgb_spec()` due to
# `scales::oob_censor()` sub-assigning `NA_real_`s

#' @export
vec_ptype2.rgb_spec.rgb_spec <- function(x, y, ...) rgb_spec()

#' @export
vec_ptype2.rgb_spec.double <- function(x, y, ...) rgb_spec()

#' @export
vec_cast.rgb_spec.rgb_spec <- function(x, to, ...) x

#' @export
vec_cast.rgb_spec.double <- function(x, to, ...) rgb_spec(x, x, x)

# Methods colour spec -----------------------------------------------------

#' @export
#' @method format colour_spec
format.colour_spec <- function(x, ...) {
  out <- lapply(vec_data(x), signif, 3)
  out <- lapply(out, format)
  out <- paste0("[", do.call(paste, c(out, sep = ",")), "]")
  out[is.na(x)] <- NA
  out
}

#' @export
#' @method vec_math colour_spec
vec_math.colour_spec <- function(.fn, .x, ...) {
  switch(.fn,
         is.finite = rowSums(!is.finite(as.matrix(.x))) > 0,
         is.infinite = rowSums(is.infinite(as.matrix(.x))) > 0,
         vec_math_base(.fn, .x, ...))
}

#' @export
#' @method vec_arith colour_spec
vec_arith.colour_spec <- function(op, x, y, ...) {
  UseMethod("vec_arith.colour_spec", y)
}

#' @export
#' @method vec_arith.colour_spec default
vec_arith.colour_spec.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}

#' @export
#' @method vec_arith.colour_spec colour_spec
vec_arith.colour_spec.colour_spec <- function(op, x, y, ...) {
  xfields <- fields(x)
  if (!all(xfields == fields(y))) {
    stop_incompatible_op(op, x, y, details = "colourspaces are not the same")
  }
  new <- vec_init_along(x)
  for (f in xfields) {
    field(new, f) <- vec_arith_base(op, field(x, f), field(y, f))
  }
  return(new)
}

#' @export
#' @method range colour_spec
range.colour_spec <- function(..., na.rm = FALSE, finite = FALSE) {
  x <- vec_c(!!!list(...), .ptype = new_rgb_spec())
  x <- lapply(vec_data(x), range, na.rm = na.rm, finite = finite)
  do.call(new_rgb_spec, x)
}

#' @method rescale colour_spec
#' @export
rescale.colour_spec <- function(x, to = c(0, 1),
                                from = range(x, na.rm = TRUE, finite = TRUE)) {
  if (is.numeric(to)) {
    to <- list(to)
  } else if (inherits(to, "colour_spec")) {
    to <- vec_data(to)
  }

  x <- mapply(rescale, x = vec_data(x), to = to, from = vec_data(from),
              SIMPLIFY = FALSE)
  do.call(new_rgb_spec, x)
}

#' @export
#' @method as.matrix colour_spec
as.matrix.colour_spec <- function(x, ...) {
  as.matrix(vec_data(x))
}

is_colour_spec <- function(x) {
  inherits(x, "colour_spec")
}
