\name{vector plots}
\alias{vectorplot}
\title{
 Vector velocity plot.
}
\description{
  Displays (velocity) vectors as segments.
}
\usage{
vectorplot(u, v, x = 0, y = 0, colvar = NULL, ...,
           col = NULL, NAcol = "white", colkey = NULL, 
           by = 1, arr = FALSE, xfac = NULL, 
           clim = NULL, clab = NULL, add = FALSE) 
}
\arguments{
  \item{u }{A vector with quantities (velocities) in x-direction. 
    }
  \item{v }{A vector with quantities (velocities) in y-direction. 
    Should have the same length as \code{u}
    }
  \item{x }{A vector with x-axis values. If \code{0}, everything will 
    be radiating from the origin. Usually \code{x} will be equal to \code{time}.
    }
  \item{y }{The y-axis value. One number, or a vector of length = u.
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
  \item{by }{Number increment for plotting vectors. 
    Set this to an integer > \code{1} if the vector density is too high. 
    }
  \item{xfac }{Only for \code{x} not \code{NULL}, the proportionality
    factor with which the vectors on the x-axis must be drawn. A value of \code{1}
    means that the distance of one will be drawn as one x-unit on the x-axis.
    For a value of \code{2} a distance of 1 will appear as 2 x-units on the x-axis. 
    if \code{NULL}, the range on the y-axis is used. In that case, it may be
    necessary to manually set the \code{xlim} of the figure.
    }
  \item{arr }{If \code{TRUE}, then \link{Arrows} will be drawn; 
    if \code{FALSE}, \link{segments} will be drawn. 
    }
  \item{add }{If \code{TRUE}, will add to the current plot. 
    }
  \item{\dots}{additional arguments passed to the plotting methods. 
    }
}

\value{
 none
}
\seealso{
 \link{quiver2D}, \link{flowpath}, for other functions to plot velocities.
}

\examples{
# save plotting parameters
 mf <- par("mfrow")

## =======================================================================
##  EXAMPLE 1: 
## =======================================================================

 par(mfrow = c(2, 2))

 u <- cos(seq(0, 2*pi, 0.1))
 v <- sin(seq(0, 2*pi, 0.1)+ 1)

 vectorplot(u = u, v = v)
 vectorplot(u = u, v = v, col = 1:10)

 x <- seq(0, 1, length.out = length(u))
 vectorplot(u = u, v = v, x = x, xfac = 3)
 points(x, rep(0, length(u)), pch = "+", col = "red")

 vectorplot(u = u, v = v, x = 1:length(u), xfac = 10)

## =======================================================================
##  EXAMPLE 2:  adding to a plot
## =======================================================================

 par(mfrow = c(2, 2))
 x <- 1:length(u)
 plot(x, u)
 vectorplot(u = u, v = v, x = x, xfac = 10, 
            add = TRUE, col = "red")
 
 vectorplot(u = u, v = v, x = x, xfac = 10, 
   colvar = sqrt(u^2+v^2), clab = "m/s")
 
 vectorplot(u = u, v = v, x = x, xfac = 10, 
   colvar = sqrt(u^2+v^2), clab = "m/s", log = "c")

# reset plotting parameters
 par(mfrow = mf)
}
\keyword{ hplot }

