\name{Tracers in 2D}
\alias{tracers2D}
\alias{tracers2Drgl}
\title{
  Plots tracer distributions in 2-D.
}
\description{
  \code{tracers2D} plots a tracer distribution using traditional R graphics.
  The topography can be defined when calling this function. 
  
  \code{tracers2Drgl} plots a tracer distribution in open-GL graphics. 
  A suitable topography has to be created before calling this function.
}
\usage{
tracers2D(x, y, colvar = NULL, ..., 
          col = NULL, NAcol = "white", colkey = NULL, 
          mask = NULL, image = FALSE, contour = FALSE, 
          clim = NULL, clab = NULL) 

tracers2Drgl(x, y, colvar = NULL, ..., 
          col = NULL, NAcol = "white", clim = NULL) 
}

\arguments{
  \item{x, y }{Vectors with x- and y-coordinates of the tracers. 
    Should be of equal length.
    }
  \item{colvar }{The variable used for coloring. It need
     not be present, but if specified, it should be a vector of 
     dimension equal to \code{x}.
     Values of \code{NULL}, \code{NA}, or \code{FALSE}
     will toggle off coloration according to \code{colvar}. 
    }
  \item{col }{Colors to be used for coloring each individual point (if colvar not
    specified) or that define the colors as specified by the 
    \code{colvar} variable. 
    If \code{col} is \code{NULL} and \code{colvar} is specified, 
    then a red-yellow-blue colorscheme (\code{jet.col}) will be used.
    If \code{col} is \code{NULL} and \code{colvar} is not specified, then 
    \code{col} will be "black". 
    }
  \item{NAcol }{Colors to be used for \code{colvar} values that are \code{NA}.
    }
 \item{colkey }{A logical, \code{NULL} (default), or a \code{list} with parameters 
    for the color key (legend). List parameters should be one of 
    \code{side, plot, length, width, dist, shift, addlines, col.clab, cex.clab,
      side.clab, line.clab, adj.clab, font.clab}
    and the axis parameters \code{at, labels, tick, line, pos, outer, font, lty, lwd,
    lwd.ticks, col.box, col.axis, col.ticks, hadj, padj, cex.axis, mgp, tck, tcl, las}.
    The defaults for the parameters are \code{side = 4, plot = TRUE, length = 1, width = 1, 
      dist = 0, shift = 0, addlines = FALSE, col.clab = NULL, cex.clab = par("cex.lab"), 
      side.clab = NULL, line.clab = NULL, adj.clab = NULL, font.clab = NULL})
    See \link{colkey} from package \code{plot3D}.
    
    The default is to draw the color key on side = 4, i.e. in the right margin.
    If \code{colkey} = \code{NULL} then a color key will be added only if \code{col} is a vector.
    Setting \code{colkey = list(plot = FALSE)} will create room for the color key 
    without drawing it.
    if \code{colkey = FALSE}, no color key legend will be added.
    }
  \item{contour, image }{If \code{TRUE}, then a \link{contour2D} or \code{image2D} 
    plot will be added to the quiver plot. 
    Also allowed is to pass a \code{list} with arguments for the 
    \link{contour2D} or \link{image2D} function. 
    }
  \item{clim }{Only if \code{colvar} is specified, the range of the colors, used
    for the color key. 
    } 
  \item{clab }{Only if \code{colkey} is not \code{NULL} or \code{FALSE}, 
     the label to be written on top of the color key. 
     The label will be written at the same level as the main title.
     To lower it, \code{clab} can be made a vector, with the first values empty 
     strings. 
    } 
  \item{mask }{A \code{list} defining the grid cells outside 
    the domain as \code{NA}.  
    Use a list with argument \code{NAcol} to specify the color that the masked cells 
    (that are \code{NA}) should get; the default is
    \code{"black"}. The unmasked cells are left \code{"white"}. 
    
    \code{mask} should be a \code{list} that contains the 
    \code{x}, \code{y}, and \code{z} values (and that are named 'x', 'y', 'z'). 
    A \code{mask} cannot be combined with \code{add = TRUE}.
    }
  \item{\dots}{additional arguments passed to the plotting method \link{scatter2D}. 
    The arguments after \dots must be matched exactly.
    }
}
\value{
  returns nothing
}
\seealso{
  \link{tracers3D} for plotting time series of tracer distributions in 3D

  \link{Ltrans} for the output of a particle tracking model
  
}
\author{Karline Soetaert <karline.soetaert@nioz.nl>}

\examples{
# save plotting parameters
 pm <- par("mfrow")

## =======================================================================
## Create topography, data
## =======================================================================

# The topographic surface
 x <-  seq(-pi, pi, by = 0.2)
 y <-  seq(0, pi, by = 0.1)
 M <- mesh(x, y)
 z <- with(M, sin(x)*sin(y))

# Initial condition
 xi <- c(0.125 * rnorm(100) - pi/2, 0.125 * rnorm(100) - pi/4)
 yi <- 0.25 * rnorm(200) + pi/2

# the species
 species <- c(rep(1, 100), rep(2, 100))   

# set initial conditions 
 xp <- xi; yp <- yi

## =======================================================================
## using a mask and contour
## =======================================================================

 Z <- z; Z[abs(Z) < 0.1] <- NA
 par(mfrow = c(2, 2))

 for (i in 1:4) {
  # update tracer distribution
   xp <- xp + 0.25 * rnorm(200)
   yp <- yp + 0.025 * rnorm(200) 
  
  # plot new tracer distribution
   tracers2D(xp, yp, colvar = species, pch = ".", cex = 5, 
     main = paste("timestep ", i), col = c("orange", "blue"), 
     colkey = list(side = 1, length = 0.5, labels = c("sp1","sp2"),
     at = c(1.25, 1.75), dist = 0.075), NAcol = "black", 
     mask = list(x = x, y = y, z = Z), 
     contour = list(x = x, y = y, z = Z) )
 }

## =======================================================================
## using image and contour
## =======================================================================

 for (i in 1:4) {
  # update tracer distribution
   xp <- xp + 0.25 * rnorm(200)
   yp <- yp + 0.025 * rnorm(200) 
  
  # plot new tracer distribution
   tracers2D(xp, yp, colvar = species, pch = ".", cex = 5, 
     main = paste("timestep ", i), col = c("orange", "blue"), 
     colkey = list(side = 1, length = 0.5, labels = c("sp1","sp2"),
     at = c(1.25, 1.75), dist = 0.075), NAcol = "black", 
     contour = list(x = x, y = y, z = z),
     image = list(x = x, y = y, z = z, colkey = TRUE))
 }

## =======================================================================
## rgl tracer plot
## =======================================================================

# here the image has to be drawn first
 image2Drgl(x = x, y = y, z = z)

# set initial conditions 
 xp <- xi; yp <- yi
 nstep <- 40
 for (i in 1:nstep) {
  # update tracer distribution
   xp <- xp + 0.25 * rnorm(200)
   yp <- yp + 0.025 * rnorm(200) 
  
  # plot new tracer distribution                                                              n
   tracers2Drgl(xp, yp, colvar = species,  cex = 1,
     main = paste("timestep ", i), col = c("orange", "blue"))

 }

# reset plotting parameters
 par(mfrow = pm)
}
\keyword{ hplot }

