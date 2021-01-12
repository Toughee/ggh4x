# Constructors ------------------------------------------------------------

#' RGB scale
#'
#' This specialised colour scale maps three variables to the red, green and blue
#' channels of the RGB colour space. This scale only works with colour and fill
#' aesthetics containing \code{rgb_spec} vectors.
#'
#' @param ... Other arguments passed on to
#'   \code{\link[ggplot2]{continuous_scale}()} to control name, breaks, labels
#'   and so forth.
#' @param na.value A \code{character(1)} with the colour to use for missing
#'   values.
#' @param guide Type of legend (not compatible yet).
#' @param limits One of the following: \itemize{
#'   \item{\code{NULL} to use the default scale range}.
#'   \item{A \code{rgb_spec} vector of length two providing limits of the
#'         scale. Use \code{NA} to refer to the existing minimum or maximum.}
#'   \item{A \code{function} that accepts the existing (automatic)
#'         \code{rgb_spec} limits and returns new limits in \code{rgb_spec}.}
#' }
#' @param spectrum_limits A \code{rgb_spec} vector of length two with channel
#'   values between 0-1, to limit the ranges of the output channels.
#' @param aesthetics The names of the aesthetics that this scale works with.
#'
#' @seealso The \code{\link[ggh4x]{rgb_spec}()} constructor.
#'
#' @return A \code{ScaleContinuousColourspec} ggproto object that can be added
#'   to a plot.
#' @export
#' @name rgb_scale
#'
#' @examples
#' # `rgp_spec` automatically triggers the rgb scale
#' p <- ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point(aes(colour = rgb_spec(drat, mpg, wt)))
#' p
#'
#' # Limits need to be defined as a length 2 `rgb_spec` vector
#' p + scale_colour_rgb(limits = rgb_spec(c(0, NA), c(0, NA), c(0, NA)))
#'
#' # The channel output can be limited with `spectrum_limits`
#' p + scale_colour_rgb(
#'   spectrum_limits = rgb_spec(c(0, 0), c(0, 1), c(0.25, 0.75))
#' )
scale_colour_rgb <- function(..., na.value = "grey50", guide = "none",
                             limits = NULL, spectrum_limits = NULL,
                             aesthetics = "colour") {
  scale <- continuous_scale(
    aesthetics, "rgb", limits = limits,
    rgb_pal, na.value = na.value, guide = guide, ...,
    super = ScaleContinuousColourspec
  )
  scale$range <- colourspec_range()
  scale$spectrum_limits <- validate_spectrum_limits(spectrum_limits, rgb_spec())
  scale$ptype <- new_rgb_spec
  scale
}

#' @rdname rgb_scale
#' @export
scale_fill_rgb <- function(..., na.value = "grey50", guide = "none",
                           limits = NULL, spectrum_limits = NULL,
                           aesthetics = "fill") {
  scale <- continuous_scale(
    aesthetics, "rgb", limits = limits,
    rgb_pal, na.value = na.value, guide = guide, ...,
    super = ScaleContinuousColourspec
  )
  scale$range <- colourspec_range()
  scale$spectrum_limits <- validate_spectrum_limits(spectrum_limits, rgb_spec())
  scale$ptype <- new_rgb_spec
  scale
}

#' @keywords internal
#' @export
#' @noRd
scale_type.rgb_spec <- function(x) {
  "rgb"
}

# Range -------------------------------------------------------------------

#' @usage NULL
#' @format NULL
#' @export
#' @rdname ggh4x_extensions
RangeColourspec <- ggproto(
  "RangeColourspec", NULL,
  range = NULL,
  reset = function(self) {
    self$range <- NULL
  },
  train = function(self, x) {
    if (is.null(x)) {
      return(invisible())
    }
    if (!inherits(x, "rgb_spec")) {
      rlang::abort("Inappropriate value supplied to colour spec scale")
    }
    existing <- self$range
    self$range <- suppressWarnings(range(x, existing, na.rm = TRUE,
                                         finite = TRUE))
  }
)

colourspec_range <- function() {
  ggproto(NULL, RangeColourspec)
}

# Scales ------------------------------------------------------------------

#' @usage NULL
#' @format NULL
#' @export
#' @rdname ggh4x_extensions
ScaleContinuousColourspec <- ggproto(
  "ScaleContinuousColourspec", ScaleContinuous,
  range = colourspec_range(),
  clone = function(self) {
    new <- ggproto(NULL, self)
    new$range <- colourspec_range()
    new
  },
  get_limits = function(self) {
    if (self$is_empty()) {
      return(vec_cast(c(0, 1), self$ptype()))
    }
    if (is.null(self$limits)) {
      self$range$range
    } else if (is.function(self$limits)) {
      # if limits is a function, it expects to work in data space
      self$trans$transform(self$limits(self$trans$inverse(self$range$range)))
    } else {
      # NA limits for a continuous scale mean replace with the min/max of data
      substitute_na(self$limits, self$range$range)
    }
  },
  map = function(self, x, limits = self$get_limits()) {
    x <- self$rescale(self$oob(x, range = limits), limits)

    uniq <- unique(x)
    pal  <- self$palette(uniq)
    scaled <- pal[vec_match(x, uniq)]

    ifelse(!is.na(scaled), scaled, self$na.value)
  },
  rescale = function(self, x, limits = self$get_limits(), range = limits,
                     spectrum_limits = self$get_spectrum_limits()) {
    self$rescaler(x, to = spectrum_limits, from = range)
  },
  transform = function(self, x) {
    fun <- ggproto_parent(ScaleContinuous, self)$transform
    if (inherits(x, "colour_spec")) {
      field_apply(x, fun)
    } else {
      fun(x)
    }
  },
  get_spectrum_limits = function(self) {
    self$spectrum_limits
  },
  spectrum_limits = NULL,
  ptype = NULL
)


# Helpers -----------------------------------------------------------------

#' @keywords internal
validate_spectrum_limits <- function(limits, as) {
  natural_limits <- vec_cast(c(0, 1), as)
  if (is.null(limits)) {
    limits <- natural_limits
  } else {
    limits <- oob_squish(limits, natural_limits,
                         only.finite = TRUE)
  }
  limits
}

substitute_na <- function(x, from) {
  UseMethod("substitute_na")
}

substitute_na.default <- function(x, from) {
  x[is.na(x)] <- from[is.na(x)]
  return(x)
}

substitute_na.colour_spec <- function(x, from) {
  if (any(is.na(vec_data(x)))) {
    for (fieldname in fields(x)) {
      field(x, fieldname) <- substitute_na(field(x, fieldname),
                                           field(from, fieldname))
    }
  }
  return(x)
}
