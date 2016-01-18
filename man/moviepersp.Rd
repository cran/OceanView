\name{Moving surfaces in 3D}
\alias{moviepersp3D}
\title{
 Plotting moving surfaces in 3D using rgl
}
\description{
  \code{moviepersp3D} plots moving perspective plots of a surface in open-GL.

  It is based on the \code{plot3Drgl} function \link{persp3Drgl}.
}
\usage{
moviepersp3D (z, x = NULL, y = NULL, t = NULL, colvar = z, tdim = 1,
  col = jet.col(100), NAcol = "white", breaks = NULL,
  colkey = FALSE, clim = NULL, clab = NULL,
  wait  = NULL, ask = FALSE, add = FALSE, basename = NULL, ... )
}

\arguments{
  \item{x, y, t }{Vectors with x, y and t-values.
    Their position in the z-array depends on \code{tdim}.
    }
  \item{z }{Three-dimensional array with the z-values to be plotted.
    }
  \item{tdim }{Index to where the \code{time} variable (over which the plot will loop)
    is to be found in \code{z} and \code{colvar}. The default is the first position,
    so that \code{z} and \code{colvar} are of dimension \code{(length(t), length(x), (length(y)}.
    }
  \item{colvar }{The variable used for coloring.
    It should be an array of dimension equal to the dimension of \code{z}.
    It need not be present.
    }
  \item{col }{Colors to be used for coloring the \code{colvar} variable. 
    If \code{col} is \code{NULL} 
    then a red-yellow-blue colorscheme (\link{jet.col}) will be used.
    }
  \item{NAcol }{Colors to be used for \code{colvar} values that are \code{NA}.
    }
  \item{breaks }{A set of finite numeric breakpoints for the colors;
    must have one more breakpoint than color and be in increasing order.
    Unsorted vectors will be sorted, with a warning.
    }
 \item{colkey }{A logical, \code{NULL} (default), or a \code{list} with parameters
    for the color key (legend).
    If \code{colkey} = \code{NULL} then a color key will be added only if \code{col} is a vector.
    Setting \code{colkey = list(plot = FALSE)} will create room for the color key
    without drawing it.
    if \code{colkey = FALSE}, no color key legend will be added.
    }
  \item{clim }{Only if \code{colvar} is specified, the range of the color variable values.
    Values of \code{colvar} that extend the range will be put to \code{NA} and colored
    as specified with \code{NAcol}.
    } 
  \item{clab }{Only if \code{colkey} is not \code{NULL} or \code{FALSE},
     the label to be written on top of the color key.
     The label will be written at the same level as the main title.
     To lower it, \code{clab} can be made a vector, with the first values empty
     strings.
    }
  \item{add }{Logical. If \code{TRUE}, then the slices will be added to the current plot.
    If \code{FALSE} a new plot is started. 
    }     
  \item{ask }{Logical. If \code{TRUE}, then the new slice will only be drawn after a key has been struck. If \code{FALSE}, redrawing will depend on \code{wait} 
    }     
  \item{wait }{The time interval inbetween drawing of a new slice, in seconds. 
    If \code{NULL}, the drawing will not be suspended. 
    }     
  \item{basename }{The base name of a \code{png} file to be produced for each movieframe. 
    }     
  \item{\dots}{additional arguments passed to \link{persp3Drgl} from package
    \code{plot3Drgl}.
    }
}

\value{
  returns nothing
}
\seealso{
  \link{Sylt3D} for a data set that can be displayed with \code{moviepersp3D}
  
  \link{moviepoints3D} for plotting moving points in 3D

  \link{movieslice3D} for plotting moving slices in 3D
}

\author{Karline Soetaert <karline.soetaert@nioz.nl>}

\examples{
 x <- y <- t <- seq(-1, 1, by = 0.1)
 grid   <- mesh(x, y, t)
 z <- with(grid, x*exp(-x^2 - y^2 - z^2))

 moviepersp3D  (x, y, z = z, colvar = z, colkey = TRUE,
   ticktype = "detailed", wait = 0.1, main = "t = ")

\dontrun{
 moviepersp3D (x, y, z = z, colvar = z, colkey = TRUE,
   aspect = TRUE, bty = "n", ask = FALSE, main = "t = ")
}

}
\keyword{ hplot }

