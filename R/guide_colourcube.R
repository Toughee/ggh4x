
guide_colourcube <- function(
  # Title
  title = waiver(),
  title.position = NULL,
  title.theme = NULL,
  title.hjust = NULL,
  title.vjust = NULL,

  # Label
  label = TRUE,
  label.theme = NULL,
  label.hjust = NULL,
  label.vjust = NULL,

  # Cube
  cubewidth = NULL,
  cubeheight = NULL,
  nbin = 10,

  # Frame
  frame.colour = "black",
  frame.linewidth = 0.5,
  frame.linetype = 1,

  # Ticks
  ticks = TRUE,
  ticks.colour = "black",
  ticks.linewidth = 0.5,

  default.unit = "line",
  order = 0,
  available_aes = c("colour", "color", "fill"),
  ...
) {
  if (!is.null(cubewidth) && !is.unit(cubewidth)) {
    cubewidth <- unit(cubewidth, default.unit)
  }
  if (!is.null(cubeheight) && !is.unit(cubeheight)) {
    cubeheight <- unit(cubeheight, default.unit)
  }

  structure(list(
    # Title
    title = title,
    title.position = title.position,
    title.theme = title.theme,
    title.hjust = title.hjust,
    title.vjust = title.vjust,

    # Label
    label = label,
    label.theme = label.theme,
    label.hjust = label.hjust,
    label.vjust = label.vjust,

    # Cube
    cubewidth = cubewidth,
    cubeheight = cubeheight,
    nbin = nbin,

    # Frame
    frame.colour = frame.colour,
    frame.linewidth = frame.linewidth,
    frame.linetype = frame.linetype,

    # Ticks
    ticks = ticks,
    ticks.colour = ticks.colour,
    ticks.linewidth = ticks.linewidth,

    # General
    default.unit = default.unit,
    order = order,

    # Parameter
    available_aes = available_aes,
    ...,
    name = "colourcube"
  ), class = c("guide", "colourcube", "colorbar"))
}

guide_train.colourcube <- function(guide, scale, aesthetic = NULL) {
  if (!inherits(scale, "ScaleContinuousColourspec")) {
    rlang::warn("colourcube guide needs rgb scales.")
    return(NULL)
  }


  if (length(intersect(scale$aesthetics, guide$available_aes)) == 0) {
    rlang::warn("colourcube guide needs appropriate scales")
    return(NULL)
  }
  if (scale$is_discrete()) {
    rlang::warn("colourcube guide needs continuous scales.")
    return(NULL)
  }

  breaks <- scale$get_breaks()
  if (length(breaks) == 0 || all(is.na(breaks))) {
    return(invisible())
  }

  limits <- scale$get_limits()
  mapped_breaks <- rescale(breaks, from = limits)

  # Create tick display
  ticks <- .int$new_data_frame(setNames(list(
    rep(scale$map(breaks), length(fields(breaks)))
    ), aesthetic %||% scale$aesthetics[1]))
  ticks$.position <- vec_c(!!!unname(vec_data(mapped_breaks)))
  ticks$.value <- vec_c(!!!unname(vec_data(breaks)))
  ticks$.label <- vec_c(!!!unname(scale$get_labels(breaks)))
  ticks$.channel <- rep(seq_along(fields(breaks)), each = length(breaks))
  ticks <- ticks[!is.na(ticks$.value),]
  ticks$.label[is.na(ticks$.label)] <- ""
  guide$key <- ticks

  # Create cube display
  limits <- vec_data(limits)
  cols <- lapply(limits, function(x) seq(x[1], x[2], length.out = guide$nbin))
  grid <- expand.grid(i = seq_len(guide$nbin), j = seq_len(guide$nbin))
  nr <- nrow(grid)

  # Create colours for cube faces
  cols <- with(grid, .int$new_data_frame(setNames(list(
    c(cols[[1]][c(i, j)], rep(limits[2, 1], nr)),
    c(cols[[2]][j], rep(limits[2, 2], nr), cols[[2]][i]),
    c(rep(limits[2, 3], nr), cols[[3]][c(i, j)])
  ), fields(scale$get_limits())[1:3])))
  cols <- do.call(scale$ptype, cols)

  # Create coordinates for cube faces
  cube <- init_cube_faces(guide$nbin)
  cube$colours <- scale$map(cols)[cube$id]

  guide$cube <- cube
  guide$hash <- with(guide, digest::digest(list(title, key$.label, name)))
  guide
}



guide_gengrob.colourcube <- function(guide, theme) {

  cubewidth <- width_cm(guide$cubewidth %||% theme$legend.key.width * 5)
  cubeheight <- height_cm(guide$cubeheight %||% theme$legend.key.height * 5)

  cubegrob <- draw_cube_grob(guide, theme)

  axes <- draw_cube_axes(guide, theme,
                         cubegrob$range_x, cubegrob$range_y,
                         cubegrob$cube_size)


  padding <- convertUnit(theme$legend.margin %||% margin(),
                         "cm", valueOnly = TRUE)
  widths <- c(padding[4], diff(axes$width), padding[2])
  heights <- c(padding[1], diff(axes$height), padding[3])


  gt <- gtable(widths = unit(widths, "cm"), heights = unit(heights, "cm"))
  gt <- gtable_add_grob(
    gt, element_render(theme, "legend.background"), clip = "off",
    t = 1, r = -1, b = -1, l = 1, name = "background"
  )
  gt <- gtable_add_grob(
    gt, cubegrob$grob, clip = "off",
    t = 2, l = 2, b = 2, r = 2, name = "cube"
  )
  gt <- gtable_add_grob(
    gt, axes$grob, clip = "off",
    t = 2, l = 2, b = 2, r = 2, name = "axes"
  )

 gt
}


# Helpers -----------------------------------------------------------------

draw_cube_grob <- function(guide, theme) {
  cube_width  <- theme$legend.key.width  %||% theme$legend.key.size
  cube_height <- theme$legend.key.height %||% theme$legend.key.size
  cube_width  <- width_cm(guide$cubewidth %||% cube_width * 5)
  cube_height <- height_cm(guide$cubeheight %||% cube_height * 5)
  cube_size <- min(cube_width, cube_height)

  cube <- guide$cube
  range_x <- range(cube$x)
  range_y <- range(cube$y)
  d <- max(diff(range_x), diff(range_y)) * 0.5
  xrange <- c(-d, d) + sum(range_x) / 2
  yrange <- c(-d, d) + sum(range_y) / 2
  cube <- transform(
    cube,
    x = rescale(x, from = range_x, to = c(0, cube_size)),
    y = rescale(y, from = range_y, to = c(0, cube_size))
  )
  first <- !duplicated(cube$id)
  grob <- with(cube, polygonGrob(
    x = x, y = y, id = id,
    gp = gpar(fill = colours[first], col = NA),
    default.units = "cm"
  ))

  if (!is.null(guide$frame.colour) & !is.na(guide$frame.colour)) {
    frame <- rotate_coordinates(
      x = c(0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0),
      y = c(0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 1),
      z = c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 0)
    )
    frame <- transform(
      frame,
      x = rescale(x, from = range_x, to = c(0, cube_size)),
      y = rescale(y, from = range_y, to = c(0, cube_size)),
      id = rep(seq_len(nrow(frame) / 2), each = 2)
    )
    frame_grob <- polylineGrob(
      x = frame$x, y = frame$y, id = frame$id,
      gp = gpar(col = guide$frame.colour,
                lty = guide$frame.linetype,
                lwd = guide$frame.linewidth * .pt),
      default.units = "cm"
    )
    grob <- grobTree(grob, frame_grob)
  }
  return(
    list(
      grob = grob,
      range_x = range_x, range_y = range_y,
      cube_size = cube_size
    )
  )
}

draw_cube_axes <- function(guide, theme, range_x, range_y, cube_size) {
  key <- guide$key
  nr <- nrow(key)
  values <- unname(split(key$.position, key$.channel))
  labels <- unname(split(key$.label, key$.channel))

  ticklength <- 0.05

  lens <- lengths(values)
  base_position <- .int$new_data_frame(list(
    x = c(values[[1]], rep(1, lens[2] + lens[3])),
    y = c(rep(0, lens[1]), values[[2]], rep(1, lens[3])),
    z = c(rep(0, lens[1] + lens[2]), values[[3]]),
    id = seq_len(sum(lens))
  ))

  offset <- .int$new_data_frame(list(
    x = c(rep(0,  lens[1]), rep(1, lens[2]), rep(0, lens[3])),
    y = c(rep(0,  lens[1]), rep(0, lens[2]), rep(1, lens[3])),
    z = c(rep(-1, lens[1]), rep(0, lens[2]), rep(0, lens[3]))
  ))

  labels <- rotate_coordinates(
    x = c(base_position$x + offset$x * ticklength * 2),
    y = c(base_position$y + offset$y * ticklength * 2),
    z = c(base_position$z + offset$z * ticklength * 2)
  )
  labels <- transform(
    labels,
    x = rescale(x, from = range_x, to = c(0, cube_size)),
    y = rescale(y, from = range_y, to = c(0, cube_size)),
    hjust = rep(c(0, 0, 0.5), lens),
    vjust = rep(c(0.5, 0.5, 0), lens),
    lab = key$.label
  )
  label.theme <- guide$label.theme %||% calc_element("legend.text", theme)
  grob <- element_grob(
    element = label.theme,
    label = labels$lab,
    x = unit(labels$x, "cm"),
    y = unit(labels$y, "cm"),
    hjust = labels$hjust, vjust = labels$vjust,
  )

  if (!is.null(guide$ticks.colour) && !is.na(guide$ticks.colour)) {
    ticks <- rotate_coordinates(
      x = c(base_position$x, base_position$x + offset$x * ticklength),
      y = c(base_position$y, base_position$y + offset$y * ticklength),
      z = c(base_position$z, base_position$z + offset$z * ticklength)
    )
    ticks <- transform(
      ticks,
      x = rescale(x, from = range_x, to = c(0, cube_size)),
      y = rescale(y, from = range_y, to = c(0, cube_size)),
      id = rep(base_position$id, 2)
    )
    ticks_grob <- polylineGrob(
      x = ticks$x, y = ticks$y, id = ticks$id,
      gp = gpar(col = guide$ticks.colour,
                lwd = guide$ticks.linewidth * .pt),
      default.units = "cm"
    )
    grob <- grobTree(grob, ticks_grob)
  }

  return(list(
    grob = grob,
    width = range(c(0, max(labels$x))),
    height = range(labels$y)
  ))
}

make_cubegrob <- function(cube, guide, theme) {
  nr <- nrow(guide$key)
  xbreaks <- rotate_coordinates(
    x = c(rep(guide$key$breaks, 2), rep(1, nr * 2)),
    y = c(rep(0, nr * 2), rep(guide$key$breaks, 2)),
    z = c(rep(c(0, -1) * 0.05, each = nr * 2))
  )
  xbreaks[, 1] <- rescale(xbreaks[, 1], from = c(-d, d) + sum(rx) / 2)
  xbreaks[, 2] <- rescale(xbreaks[, 2], from = c(-d, d) + sum(ry) / 2)
  xbreak_grob <- polylineGrob(
    x = unit(xbreaks[, 1], "snpc"),
    y = unit(xbreaks[, 2], "snpc"),
    gp = gpar(col = guide$ticks.colour,
              lwd = guide$ticks.linewidth),
    id = rep(seq_len(nrow(xbreaks) / 4), 4)
  )

  ybreaks <- rotate_coordinates(
    x = c(1, 1, 1 , 1, 1, 1),
    y = rep(guide$key$breaks, 2),
    z = c(0, 0, 0, -1, -1, -1) * 0.05
  )
  ybreaks[, 1] <- rescale(ybreaks[, 1], from = c(-d, d) + sum(rx) / 2)
  ybreaks[, 2] <- rescale(ybreaks[, 2], from = c(-d, d) + sum(ry) / 2)
  ybreak_grob <- polylineGrob(
    x = unit(ybreaks[, 1], "snpc"),
    y = unit(ybreaks[, 2], "snpc"),
    gp = gpar(col = guide$ticks.colour,
              lwd = guide$ticks.linewidth),
    id = rep(seq_len(nrow(ybreaks) / 2), 2)
  )


  grid.newpage(); grid.draw(grb)
  grid.draw(xbreak_grob)
  grid.draw(ybreak_grob)
}

# Rotates xyz coordinates with isometric projection
rotate_coordinates <- function(x, y, z, angle1 = 30, angle2 = 45) {
  # Degrees to radians
  angle1 <- asin(tan(angle1 * (pi / 180)))
  angle2 <- angle2 * (pi / 180)

  # Calculate rotation matrix
  angle1 <- c(1, 0, 0, 0, cos(angle1), -sin(angle1),
              0, sin(angle1), cos(angle1))
  dim(angle1) <- c(3, 3)

  angle2 <- c(cos(angle2), 0, sin(angle2), 0, 1,
              0, -sin(angle2), 0, cos(angle2))
  dim(angle2) <- c(3, 3)

  rot <- angle1 %*% angle2

  # Set up cube coordinates
  coords <- matrix(
    c(x, y, z), ncol = 3
  )

  # Rotate cube
  coords <- tcrossprod(rot, coords)

  # Project on xy plane
  coords <- t(diag(c(1, 1, 0)) %*% coords)
  .int$new_data_frame(list(
    x = coords[, 1],
    y = coords[, 2]
  ))
}

init_cube_faces <- function(nbin = 10) {
  # Initialise parameters
  sequence <- (seq_len(nbin) - 1) / nbin
  offset <- 1 / (nbin)
  nr <- prod(nbin, nbin)

  # Setup grid
  cube <- expand.grid(
    a = rev(sequence),
    b = rev(sequence)
  )
  id <- rep(seq_len(nr), 4)
  id <- c(id, id + nr, id + nr + nr)

  # Rotate coordinates
  coords <- with(cube, rotate_coordinates(
    x = c(rep(a, 2), rep(a + offset, 2), # xy plane
          rep(b, 2), rep(b + offset, 2), # xz plane
          rep(0, 4 * nr)), # yz plane
    y = c(rev(c(b, rep(b + offset, 2), b)), # xy plane
          rep(1, 4 * nr), # xz plane
          rev(c(rep(a, 2), rep(a + offset, 2)))), # yz plane
    z = c(rep(0, 4 * nr), # xy plane
          a, rep(a + offset, 2), a, # xz plane
          b, rep(b + offset, 2), b) # yz plane
  ))

  coords$id <- id
  coords
}

