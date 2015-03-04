\name{Quiver and flow paths}
\alias{quiver2D}
\alias{quiver2D.matrix}
\alias{quiver2D.array}
\alias{quiver2Drgl}
\alias{flowpath}
\title{
  Plots velocities as arrows or as trajectory plots.
}
\description{
  Function \code{quiver2D} displays velocity vectors as arrows, using ordinary graphics.

  Function \code{quiver2Drgl} displays velocity vectors as arrows using rgl. 
    
  Function \code{flowpath} displays the flow paths of particles, based on 
  velocity vectors.
  
}
\usage{
quiver2D(u, ...)

\method{quiver2D}{matrix}(u, v, x = NULL, y = NULL, 
           colvar = NULL, ..., 
           scale = 1, arr.max = 0.2, arr.min = 0, speed.max = NULL,  
           by = NULL, type = "triangle", col = NULL, NAcol = "white", 
           colkey = NULL, mask = NULL, 
           image = FALSE, contour = FALSE, 
           clim = NULL, clab = NULL, 
           add = FALSE, plot = TRUE) 

\method{quiver2D}{array} (u, v, margin = c(1, 2), subset, ask = NULL, ...)
                    
quiver2Drgl (u, v, x = NULL, y = NULL, colvar = NULL, ..., 
           scale = 1, arr.max = 0.2, arr.min = 0, speed.max = NULL, 
           by = NULL, type = "triangle", 
           col = NULL, NAcol = "white",  
           mask = NULL, image = FALSE, contour = FALSE, 
           clim = NULL, add = FALSE, plot = TRUE)

flowpath(u, v, x = NULL, y = NULL, startx = NULL, starty = NULL, ...,
           scale = 1, numarr = 0, arr.length = 0.2, maxstep = 1000, 
           add = FALSE, plot = TRUE)
}

\arguments{
  \item{u }{A matrix (\code{quiver2D}) or array (\code{quiver2D.array})
    with velocities in x-direction. 
    For \code{quiver2D} 
    the number of rows should be = Nx or Nx+1 (Nx = length(x), if x given),  
    the number of columns should be = Ny or Ny+1 (Ny = length(y), if y given). 
    }
  \item{v }{A matrix (\code{quiver2D}) or array (\code{quiver2D.array})
    with velocities in y-direction. 
    For \code{quiver2D} the number of rows should be = Nx or Nx+1,  
    the number of columns should be = Ny or Ny+1.  
    }
  \item{x }{Vector with x-coordinates of the velocities. 
    If \code{NULL}, it is taken to be a sequence between (0, 1), and with 
    length = \code{nrow(u)}. 
    }
  \item{y }{Vector with y-coordinates of the velocities. If \code{NULL}, it
    is taken to be a sequence between (0, 1), and with 
    length = \code{ncol(v)}.  
    }
  \item{startx }{Vector with the start position in x-direction of the flow paths. 
    Length > =1. If not specified, then all combinations of \code{x} and \code{y} at 
    the outer margins will be used as starting point.
    }
  \item{starty }{Vector with start position in y-direction of flow paths. 
    Length = length of \code{startx}. 
    }
  \item{colvar }{The variable used for coloring. It need
     not be present, but if specified, it should be a vector of 
     dimension equal to \code{c(nrow(u), ncol(v))}.
     Values of \code{NULL}, \code{NA}, or \code{FALSE}
     will toggle off coloration according to \code{colvar}. 
    }
  \item{col }{Colors to be used for coloring the arrows as specified by the 
    \code{colvar} variable. 
    If \code{col} is \code{NULL} and \code{colvar} is specified, 
    then a red-yellow-blue colorscheme (\link{jet.col}) will be used.
    If \code{col} is \code{NULL} and \code{colvar} is not specified, then 
    \code{col} will be "black". 
    }
  \item{NAcol }{Colors to be used for \code{colvar} values that are \code{NA}.
    }
  \item{scale }{Scaling factor for the arrows. 
    When \code{scale = 1}, the longest arrow will fill a grid cell in 
      x- and y- direction. When \code{scale = 2}, it will be twice as long.
    }
  \item{arr.max }{Maximal size of the arrowhead, in cm (approximately). 
    The arrows are scaled according to the velocity (\code{sqrt(u^2 + v^2)}). 
    \code{arr.max} is associated with the maximal velocity.
    }
  \item{arr.min }{Minimal size of the arrowhead, in cm (approximately). 
    Set \code{arr.min} = \code{arr.max} for constant size.
    }
  \item{speed.max }{Speed that corresponds to \code{arr.max}. 
    Everything with speed larger than \code{speed.max} will be depicted with 
    size equal to \code{arr.max}.
    If unspecified (\code{max(sqrt(u^2 + v^2))}). 
    }
  \item{by }{Number increment for plotting the vectors; one value or two (x, y) values. 
    For example, setting \code{by = 2} will plot every second velocity value in 
    \code{x} and in \code{y} direction.  Setting \code{by = c(1, 2)} will
    plot all vectors in \code{x} and every second vector in \code{y}.
    Useful if the vector density is too high. 
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
  \item{type }{The type of the arrow head, one of \code{"triangle"} (the default) or
    \code{"simple"}, which uses R-function \link{arrows}.
    }
  \item{contour, image }{If present, then a \link{contour2D} or \link{image2D} 
    plot will be added to the quiver plot. 
    They should be a \code{list} with arguments for the 
    \link{contour2D} or \code{image2D} function. 
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
  \item{margin }{A vector giving the subscripts which the plotting 
    function will be applied over.   
    The plotting function will loop over the index that is not in \code{margin}.
    For instance, \code{c(1, 2)}, indicates to plot rows(x) and columns(y) and to 
     loop over index \code{3};  
    \code{c(2, 1)} will do the same but transposed.
    \code{margin} should be a vector with two numbers inbetween \code{1}, and \code{3}.
    }
  \item{ask }{A logical; if \code{TRUE}, the user is asked before each plot, 
    if \code{NULL} the user is only asked if more than one page of plots is 
    necessary and the current graphics device is set interactive, 
    see \link{par}(ask) and \link{dev.interactive}.
    }
  \item{add }{If \code{TRUE}, will add to current plot. 
    Else will start a new plot. Note: to use this in a consistent way, the 
    previous plot should have been done with one of the \code{plot3D} functions.
    }
  \item{mask }{A \code{matrix} or \code{list} defining the grid cells outside 
    the domain as \code{NA}.  
    Use a list with argument \code{NAcol} to specify the color that the masked cells 
    (that are \code{NA}) should get; the default is
    \code{"black"}. The unmasked cells are left \code{"white"}. 
    
    If \code{x} and \code{y} are a vector, then \code{mask} can be a \code{matrix} with 
    dimension equal to \code{length(x), length(y)}. If either \code{x} or \code{y} 
    is itself a \code{matrix}, 
    then \code{mask} should be a \code{list} that contains the 
    \code{x}, \code{y}, and \code{z} values (and that are named 'x', 'y', 'z'). 
    A \code{mask} cannot be combined with \code{add = TRUE}.
    }
  \item{plot }{If \code{FALSE}, will not plot the flow paths, 
    but will return the matrix with path values instead.
    }
  \item{numarr }{The number of arrows added on the flow paths. 
    }
  \item{arr.length }{Constant size of the arrowhead, in cm (approximately). 
    }
  \item{maxstep }{Maximum number of steps for calculating the flow paths.  
    } 
  \item{\dots}{Additional arguments passed to the plotting methods (\link{arrows2D}),
    The arguments after \dots must be matched exactly.
    }
  \item{subset }{A logical expression indicating over which elements to loop;
    missing values are taken as \code{FALSE}.
    }
}
\details{
  S3 function \code{quiver2D} plots vectors specified by \code{u, v} at the coordinates
  \code{x, y}.
  
  \code{flowpath} uses the velocities \code{u, v} at the coordinates
  \code{x, y} to create trajectories, starting at points 
  \code{startx, starty}. It can also be used to return the flow path 
  points by setting \code{plot} equal to \code{FALSE}.
  It uses very simple Euler integration and may not be very accurate.
}

\value{
  \code{flowpath} returns (as \code{invisible}) a 2-column 
  matrix with the x-y coordinates of the flow paths. 
  Separate flow paths are separated with \code{NA}.

  \code{quiver2D} returns  (as \code{invisible}) a \code{list} containing the 
  coordinates of the arrows (\code{x0}, \code{x1}, \code{y0}, \code{y1}), 
  the color of each arrow (\code{col}), the length of the arrowhead
  (\code{length}) and the maximal speed corresponding to \code{arr.max} (\code{speed.max}). 
   This output can be used e.g. with function \link{arrows}.
}
\seealso{
  \link{arrows3D} for an arrows function from package \code{plot3D}.
  
  \link{vectorplot} for plotting velocity vectors as spikes.

  \link{Arrows} for the arrow function from package \code{shape} on which quiver2D is based.
}

\examples{
## =======================================================================
##  EXAMPLE 1: 
## =======================================================================
 pm <- par("mfrow")
 par(mfrow = c(2, 2))

# generate velocities
 x  <- seq(-1, 1, by = 0.2)
 y  <- seq(-1, 1, by = 0.2)
 dx <- outer(x, y , function(x, y) -y)
 dy <- outer(x, y , function(x, y) x)

# velocity plot, with legend
 F <- quiver2D(u = dx, v = dy, x = x, y = y)
 legend("topright", bg = "white", 
   legend = paste("max = ", format(F$speed.max, digits = 2))) 

# different color for up/downward pointing arrows
 quiver2D(u = dx, v = dy, x = x, y = y, colvar = dx > 0, 
        col = c("red", "blue"), colkey = FALSE,
        arr.max = 0.4, arr.min = 0.1)

# different scale
 quiver2D(u = dx, v = dy, x = x, y = y, by = 2, scale = 2)

# three flow paths
 flowpath(u = dx, v = dy, x = x, y = y, startx = 0.1, starty = 0.1)
 flowpath(u = dx, v = dy, x = x, y = y, 
          startx = c(0.9, -0.9), starty = c(0.0, 0.0), col = "red",
          numarr = 2, add = TRUE)

## =======================================================================
##  EXAMPLE 2: note: has changed in version 1.0.3 - uses contour2D!
## =======================================================================
 par(mfrow = c(1, 1))
 x <- seq(-2, 2, by = 0.2)
 y <- seq(-1, 1, by = 0.2)
 z <- outer (x, y, function(x, y) x^3 - 3*x -2*y^2)
 contour2D(x, y, z = z, col = jet.col(10))

# gradients in x- and y-direction (analytical)
 dX <- outer(x, y, function(x,y) 3*x^2 - 3)
 dY <- outer(x, y, function(x,y) -4*y)

 quiver2D(u = dX, v = dY, x = x, y = y, scale = 1, add = TRUE, by = 1)
 flowpath(u = dX, v = dY, x = x, y = y, startx = c(-2, 1.1), 
            starty = c(-1, -1), add = TRUE, arr.length = 0.5,
            col = "darkgreen", lwd = 3, numarr = 1)

## =======================================================================
##  EXAMPLE 3: 
## =======================================================================
 
 x <- y <- 1:20
 u <- outer (x, y, function (x, y) cos(2*pi*y/10))
 v <- outer (x, y, function (x, y) cos(2*pi*x/10))

 quiver2D(x = x, y = y, u = u, v = v, col = "grey")

# flowpaths using all combinations of x and y at edges
 flowpath(x = x, y = y, u = u, v = v, add = TRUE, 
          lwd = 2, col = "orange")

## =======================================================================
##  EXAMPLE 4: quiver of an array.. 
## =======================================================================
 
 x <- y <- 1:20
 u2 <- outer (x, y, function (x, y) sin(2*pi*y/10))
 v2 <- outer (x, y, function (x, y) sin(2*pi*x/10))

# merge u, u2 and v, v2 to create an "array"
 U <- array(dim = c(dim(u2), 2), data = c(u, u2), 
            dimnames = c("x", "y", "times"))
 V <- array(dim = c(dim(v2), 2), data = c(v, v2), 
            dimnames = c("x", "y", "times"))

 quiver2D(u = U, v = V, x = x, y = y, main = c("time 1", "time 2"))

# quiver over x and time, for a subset of y-values:
 quiver2D(u = U, v = V, x = x, y = 1:2, 
        margin = c(1, 3), main = paste("y ", y), 
        subset = y <= 4)

\dontrun{
 quiver2D(u = U, v = V, x = x, y = y, ask = TRUE, 
        mfrow = c(1, 1))

 quiver2D(u = U, v = V, x = x, y = 1:2, ask = TRUE, 
        margin = c(1, 3), main = paste("y ", y),
        mfrow = c(1, 1))

}

## =======================================================================
##  EXAMPLE 5: 
## =======================================================================
 par(mfrow = c(1, 1))

 image2D(x = 1:nrow(volcano), y = 1:ncol(volcano), 
       z = volcano, contour = TRUE)

# Assume these are streamfunctions, we calculate the velocity field as:
 dx <- dy <- 1
 v <-   (volcano[-1, ] - volcano[-nrow(volcano), ] )/dx
 u <- - (volcano[, -1] - volcano[ ,-ncol(volcano)] )/dy

 quiver2D(x = 1:nrow(u), y = 1:ncol(v), 
        u = u, v = v, add = TRUE, by = 3)
 
 flowpath(x = 1:nrow(u), y = 1:ncol(v), numarr = 10,
          u = u, v = v, add = TRUE, lwd = 2, col = "grey", 
          startx = 20, starty = 30)
 
## =======================================================================
##  EXAMPLE 6: boundary mask, images, contours
## =======================================================================
 par (mfrow = c(2, 2))
 
 mask <- volcano; mask[volcano < 120]  <- NA
 quiver2D(by = c(3, 2), u = u, v = v, mask = mask)

 quiver2D(by = c(3, 2), u = u, v = v,  
        image = list(z = mask, NAcol = "black"))

 quiver2D(by = c(4, 3), u = u, v = v,
        contour = list(z = volcano, lwd = 2))

 quiver2D(by = c(4, 3), u = u, v = v, 
        contour = list(z = volcano, col = "black"), 
        image = list(z = volcano, NAcol = "black"))

## =======================================================================
##  Same in rgl
## =======================================================================
\dontrun{
 quiver2Drgl(by = c(3, 2), u = u, v = v, mask = mask, NAcol = "black")

 quiver2Drgl(by = c(3, 2), u = u, v = v,  
        image = list(z = volcano, NAcol = "black"))

 quiver2Drgl(by = c(4, 3), u = u, v = v, scale = 2,
        contour = list(z = volcano, lwd = 2))

 quiver2Drgl(by = c(4, 3), u = u, v = v, 
        contour = list(z = volcano, col = "black"), 
        image = list(z = volcano, NAcol = "black"))
 cutrgl()
 uncutrgl()
}

## =============================================================================
## 2-D Data set SyltSurf
## =============================================================================

 par(mfrow = c(1, 1))
 with (Syltsurf,
   quiver2D(x = x, y = y, u = u[ , ,2], v = v[ , ,2], 
     xlim = c(5, 20), ylim = c(10, 25), by = 3, 
     main = paste(formatC(time[1]), " hr"), scale = 1.5, 
     image = list(z = depth, x = x, y = y, NAcol = "black", 
                  colkey = TRUE), 
     contour = list(z = depth, x = x, y = y, col = "black",
       drawlabels = FALSE)
     )
  )

## =============================================================================
## 2-D Data set SyltSurf, several time points
## =============================================================================

# now for an array (first and 4th time point only)
 ii <- c(1, 4)
 with (Syltsurf,
   quiver2D(x = x, y = y, u = u[ ,,ii], v = v[ ,,ii], 
     xlim = c(5, 20), ylim = c(10, 25), by = 4, 
     mask = list(z = depth, x = x, y = y, NAcol = "blue"),
     main = paste(formatC(time[ii]), " hr"), scale = 1.5,
     contour = list(z = depth, x = x, y = y, drawlabels = FALSE)
     )
  )



## =============================================================================
## Adding quivers ...
## =============================================================================
x <- 1:2
y <- 1:3
u <- matrix(1:6,2,3)
v <- matrix(6:1,2,3)
 par(mfrow = c(1, 1))

A <- quiver2D(x = x, y = y, u = u, v = v)
B <- quiver2D(x = x, y = y[-1], u = u[,-1], v = v[,-1], col = 2, add = TRUE)
C <- quiver2D(x = x, y = y[-3], u = u[,-3], v = v[,-3], col = 3, add = TRUE)

# restore parameter settings
 par(mfrow = pm)
}
\note{
There was a slight error in the scaling of the arrows in versions previous to
1.0.3, which has been corrected. See last example.

}
\keyword{ hplot }

