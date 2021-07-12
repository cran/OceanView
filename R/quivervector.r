quiver2D.vector <- function(u, v, x = NULL, y = NULL, colvar = NULL, ...,
                    scale = 1, arr.max = 0.2, arr.min = 0, speed.max = NULL,
                    by = NULL, type = "triangle",
                    col = NULL, NAcol = "white", breaks = NULL, colkey = NULL,
                    clim = NULL, clab = NULL, add = FALSE, plot = TRUE)  {
# ------------------------------------------------------------------------------
# check input
# ------------------------------------------------------------------------------
  if (! is.vector(u) | ! is.vector (v))
    stop ("'u' anc 'v' should be a vector")

  dots  <- splitpardots( list(...) )
  dp    <- dots$points
  dm    <- dots$main

  if (add)
      plist <- getplist()
  else plist <- NULL
  setplist(plist)

  # colors and color variable
  if (! is.null(colvar)) {
    varlim <- clim
    if (is.null(varlim))
      varlim <- range(colvar, na.rm = TRUE)

    if (any (dim(colvar) - c(nrow(u), ncol(v)) != 0))
      stop ("dimension of 'colvar' not compatible with dimension of 'u' and 'v'")

    if (! is.null(by)) {
      ix <- seq(1, length(u), by = by)
      colvar <- colvar[ix]
    }

    if (is.null(col) & is.null(breaks))
      col <- jet.col(100)
    else if (is.null(col))
      col <- jet.col(length(breaks)-1)
    breaks <- check.breaks(breaks, col)

    if (dots$clog) {
      colvar <- log(colvar)
      if (! is.null(clim))
        clim <- log(clim)
    }

    iscolkey <- is.colkey(colkey, col)    # check if colkey is needed
    if (iscolkey) {
      colkey <- check.colkey(colkey)
      if (! add)
        plist$plt$main <- colkey$parplt
    }
    par (plt = plist$plt$main)

    if (is.null(clim))
      clim <- range(colvar, na.rm = TRUE)

    Col <- variablecol(colvar, col, NAcol, clim, breaks) # generate color scheme

    pltori <- plist$plt$ori
  } else {
    Col <- col
    if (is.null(Col))
      Col <- "black"
    iscolkey <- FALSE
  }
# ------------------------------------------------------------------------------
# size of the arrows
# ------------------------------------------------------------------------------
  Log <- FALSE
  if (! is.null(dm$log)) {
    if (length(grep("a", dm[["log"]])) > 0) {
      dm[["log"]] <- gsub("a", "", dm[["log"]])
      Log <- TRUE
      if (dm[["log"]] == "")
        dm[["log"]] <- NULL
    }
  }
  maxspeed <- speed.max
  speed <- sqrt(u^2 + v^2)
  if (is.null(maxspeed))
    maxspeed <- max(speed)
  else {
    if (maxspeed <= 0)
      stop ("'speed.max' should be >= 0")
    speed <- pmin(speed, maxspeed)
  }
  if (maxspeed == 0)
    maxspeed <- 1e-16

  if (!is.null(plist$quiver)) {
    xr <- plist$quiver$xr
    yr <- plist$quiver$yr
    maxspeed <- plist$quiver$maxspeed
    if (Log) maxspeed <- exp(maxspeed)
#  } else if (add) {
#    pusr <- par("usr")
#    xr <- diff (pusr[1:2]) / diff(range(u))
#    yr <- diff (pusr[3:4]) / diff(range(v))
  
  } else {
  xr <- diff (range(x)) /diff (range(u))
  yr <- diff (range(y)) / diff (range(v))
  }

  if (!is.null(scale)) {
    u <- u * scale / maxspeed * xr
    v <- v * scale / maxspeed * yr
  }

  xto <- x + u
  yto <- y + v
  if (Log) speed <- log(speed)
  if (Log) maxspeed <- log(maxspeed)


  dp$length <- speed / maxspeed * (arr.max - arr.min) + arr.min

  if (plot) {
    if (! add) {
      if (is.null(dm$xlab))
        dm$xlab <- "x"
      if (is.null(dm$ylab))
        dm$ylab <- "y"
    }

    dp$arr.type <- NULL
    if (is.null(dp$lwd))
      dp$lwd <- 1

    do.call("arrows2D", c(alist(x, y, xto, yto, col = Col, type = type, add = add),  dm, dp))

    if (iscolkey) {
      colkey$parleg <- colkey$parplt <- NULL
      do.call("colkey", c(alist(col = col, clim = varlim, clab = clab,
        clog = dots$clog, add = TRUE), colkey))
      par(plt = pltori)
    }
    par(mar = par("mar"))

  } #plot
  if (Log) maxspeed <- exp(maxspeed)
  if (! add) {
    plist <- getplist()
    plist$quiver <- list(xr = xr, yr = yr, maxspeed = maxspeed)
    setplist(plist)
  }
    
  
  invisible(list(x0 = x, y0 = y, x1 = xto, y1 = yto, col = Col, length = dp$length,
    speed.max = maxspeed))
}
