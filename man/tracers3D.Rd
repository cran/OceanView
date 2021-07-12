\name{Tracers in 3D}
\alias{tracers3D}
\alias{tracers3Drgl}
\alias{moviepoints3D}
\title{
 Plotting tracer distributions in 3D
}
\description{
  \code{tracers3D} plots 3D tracer distributions in traditional graphics. 
  The topography can be defined when calling this function or created before
  calling this function. 
  
  \code{tracers3Drgl} plots 3D tracer distributions in open-GL graphics. 
  A suitable topography has to be created before calling this function.
  It does not create a movie.  

  \code{moviepoints3D} creates a movie of tracer distributions
    in open-GL graphics. 
  
  It is based on the \code{plot3Drgl} function \link{points3Drgl}.
}
\usage{
  tracers3D (x, y, z, colvar = NULL, ..., 
      col = NULL, NAcol = "white", breaks = NULL,
      colkey = FALSE, clim = NULL, clab = NULL, surf = NULL) 

  tracers3Drgl (x, y, z, colvar = NULL, ..., 
      col = NULL, NAcol = "white", breaks = NULL,
      colkey = FALSE, clim = NULL, clab = NULL)

  moviepoints3D (x, y, z, colvar, t, by = 1, 
    col = jet.col(100), NAcol = "white", breaks = NULL,
    clim = NULL, wait  = NULL, ask = FALSE, add = FALSE,
    basename = NULL, ...) 
}

\arguments{
  \item{x, y, z }{Vectors with (x, y, z) positions of tracers. 
    Should be of equal length.
    }
  \item{colvar }{The variable used for coloring. It need
     not be present, but if specified, it should be a vector of 
     dimension equal to \code{x, y, z}.
     Values of \code{NULL}, \code{NA}, or \code{FALSE}
     will toggle off coloration according to \code{colvar}. 
    }
  \item{t }{Vectors with time points of tracers. 
    Should be of length equal to length of \code{x, y ,z, colvar}.
    }
  \item{by }{Number increment of the time sequence.
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
  \item{breaks }{a set of finite numeric breakpoints for the colors;
    must have one more breakpoint than color and be in increasing order.
    Unsorted vectors will be sorted, with a warning.
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
  \item{clim }{Only if \code{colvar} is specified, the range of the colors, used
    for the color key. 
    } 
  \item{clab }{Only if \code{colkey} is not \code{NULL} or \code{FALSE}, 
     the label to be written on top of the color key. 
     The label will be written at the same level as the main title.
     To lower it, \code{clab} can be made a vector, with the first values empty 
     strings. 
    } 
  \item{surf }{If not \code{NULL}, a list specifying a surface to be added on 
    the scatterplot. 
    They should include at least \code{x}, \code{y}, \code{z}, equal sized
    matrices, and optional: \code{colvar, col, NAcol, border, facets, 
    lwd, resfac, clim, ltheta, lphi, shade, lighting}. Note that the default is 
    that \code{colvar} is not specified.
    }
  \item{add }{Logical. If \code{TRUE}, then the points will be added to the current plot.
    If \code{FALSE} a new plot is started. 
    }     
  \item{ask }{Logical. If \code{TRUE}, then new points will only be drawn after a key has been struck. If \code{FALSE}, redrawing will depend on \code{wait} 
    }     
  \item{wait }{The time interval inbetween drawing of a set of new points, in seconds. 
    If \code{NULL}, the drawing will not be suspended. 
    }     
  \item{basename }{The base name of a \code{png} file to be produced for each movieframe. 
    }     
  \item{\dots}{additional arguments passed to \link{scatter3D} from package 
    \code{plot3D}.  Typical arguments are \code{cex, main} (both functions), 
    and \code{pch, ...} for \code{tracers3D}.
    }
}

\value{
  returns nothing
}

\author{Karline Soetaert <karline.soetaert@nioz.nl>}
\seealso{
  \link{tracers2D} for plotting time series of tracer distributions in 2D

  \link{movieslice3D} for plotting slices in 3D
  
  \link{Ltrans} for 3-D output of a particle tracking model
}
\examples{
# save plotting parameters
 pm <- par("mfrow")

## =======================================================================
## Create topography, data
## =======================================================================

# The topographic surface
 x <- seq(-pi, pi, by = 0.2)
 y <- seq(0, pi, by = 0.1)
 M <- mesh(x, y)
 z <- with(M, sin(x)*sin(y))

# Initial condition
 xi <- c(0.25 * rnorm(100) - pi/2, 0.25 * rnorm(100) - pi/4)
 yi <- 0.25 * rnorm(200) + pi/2
 zi <- 0.005*rnorm(200) + 0.5

# the species
 species <- c(rep(1, 100), rep(2, 100))   

# set initial conditions 
 xp <- xi; yp <- yi; zp <- zi

## =======================================================================
## Traditional graphics
## =======================================================================

 par(mfrow = c(2, 2))

# Topography is defined by argument surf
 for (i in 1:4) {
  # update tracer distribution
   xp <- xp + 0.25 * rnorm(200)
   yp <- yp + 0.025 * rnorm(200) 
   zp <- zp + 0.25 *rnorm(200)
  
  # plot new tracer distribution
   tracers3D(xp, yp, zp, colvar = species, pch = ".", cex = 5, 
     main = paste("timestep ", i), col = c("orange", "blue"), 
     surf = list(x, y, z = z, theta = 0, facets = FALSE),
     colkey = list(side = 1, length = 0.5, labels = c("sp1","sp2"),
                   at = c(1.25, 1.75), dist = 0.075))
 }

# same, but creating topography first
\dontrun{
# create the topography on which to add points 
 persp3D(x, y, z = z, theta = 0, facets = FALSE, plot = FALSE)

 for (i in 1:4) {
  # update tracer distribution
   xp <- xp + 0.25 * rnorm(200)
   yp <- yp + 0.025 * rnorm(200) 
   zp <- zp + 0.25 *rnorm(200)
  
  # plot new tracer distribution
   tracers3D(xp, yp, zp, colvar = species, pch = ".", cex = 5, 
     main = paste("timestep ", i), col = c("orange", "blue"), 
     colkey = list(side = 1, length = 0.5, labels = c("sp1","sp2"),
                    at = c(1.25, 1.75), dist = 0.075))
 }
}

## =======================================================================
## rgl graphics
## =======================================================================

# pause <- 0.05
# create a suitable topography
 persp3D(x, y, z = z, theta = 0, facets = NA, plot = FALSE)

 plotrgl( )
 xp <- xi; yp <- yi; zp <- zi
 
 nstep <- 10
 for (i in 1:nstep) {
   xp <- xp + 0.05 * rnorm(200) + 0.05
   yp <- yp + 0.0025 * (rnorm(200) + 0.0025)
   zp <- zp + 0.05 *rnorm(200)

#   tracers3Drgl(xp, yp, zp, col = c(rep("orange", 100), rep("blue", 100)),
#     main = paste("timestep ", i))
# or:
   tracers3Drgl(xp, yp, zp, colvar = species, col = c("orange", "blue"),
     main = paste("timestep ", i))
#   Sys.sleep(pause)
# or:  readline("hit enter for next")
 }

# using function moviepoints3D

\dontrun{
# first create the data in matrices
 xp <- matrix(nrow = 200, ncol = nstep, data = rep(xi, times=nstep)) 
 yp <- matrix(nrow = 200, ncol = nstep, data = rep(yi, times=nstep)) 
 zp <- matrix(nrow = 200, ncol = nstep, data = rep(zi, times=nstep))
 tp <- matrix(nrow = 200, ncol = nstep, data = 0)
 cv <- matrix(nrow = 200, ncol = nstep, data = rep(species, times=nstep))
 nstep <- 10
 for (i in 2:nstep) {
   xp[,i] <- xp[,i-1] + 0.05 * rnorm(200) + 0.05
   yp[,i] <- yp[,i-1] + 0.0025 * (rnorm(200) + 0.0025)
   zp[,i] <- zp[,i-1] + 0.05 *rnorm(200)
   tp[,i] <- i
 }
# create the topography
 persp3Drgl(x, y, z = z, theta = 0, lighting = TRUE, smooth = TRUE)

# add moviepoints:
  moviepoints3D (xp, yp, zp, colvar = cv, t = tp, 
    wait = 0.05, cex = 10, col = c("red", "orange")) 
 
}

# reset plotting parameters
 par(mfrow = pm)
}
\keyword{ hplot }

