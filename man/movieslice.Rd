\name{Moving slices in 3D}
\alias{movieslice3D}
\alias{movieslice3D}
\title{
 Plotting volumetric data as moving slices in 3D using rgl
}
\description{
  \code{movieslice3D} plots 3D volumetric data as slices moving in one direction
    in open-GL graphics. 
  
  It is based on the \code{plot3Drgl} function \link{slice3Drgl}.
}
\usage{
movieslice3D (x, y, z, colvar = NULL, xs = NULL,
  ys = NULL, zs = NULL, along = NULL,
  col = jet.col(100), NAcol = "white", clim = NULL, 
  wait  = NULL, ask = FALSE, add = FALSE, 
  basename = NULL, ...) 
}

\arguments{
  \item{x, y, z }{Vectors with x, y and z-values. 
    They should be of length equal to the first, second and 
    third dimension of \code{colvar} respectively.
    }
  \item{colvar }{The variable used for coloring. 
    It should be an array of dimension equal to 
    \code{c(length(x), length(y), length(z))}.
    It must be present.
    }
  \item{col }{Colors to be used for coloring the \code{colvar} variable. 
    If \code{col} is \code{NULL} 
    then a red-yellow-blue colorscheme (\link{jet.col}) will be used.
    }
  \item{NAcol }{Colors to be used for \code{colvar} values that are \code{NA}.
    }
  \item{clim }{Only if \code{colvar} is specified, the range of the color variable values. 
    Values of \code{colvar} that extend the range will be put to \code{NA} and colored
    as specified with \code{NAcol}.
    } 
  \item{xs, ys, zs }{Vectors specify the positions in x, y or z where the slices (planes) are to be drawn consecutively. 
    The movie will loop over the slices, each time projecting the values of \code{colvar} on them.
    If all \code{xs, ys, zs} are \code{NULL}, then \code{xs} will be taken equal to \code{x}.
    } 
  \item{along }{A number 1, 2, 3 denoting the dimension over which the slices are to be moved. 
    If \code{NULL}, then the dimension will be the one corresponding to the longest vector \code{xs, ys, zs}. 
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
  \item{\dots}{additional arguments passed to \link{slice3D} from package 
    \code{plot3D} or to \code{plotrgl} from package \code{plot3Drgl}.
    }
}

\value{
  returns nothing
}
\seealso{
  \link{Sylt3D} for a data set that can be displayed with \code{movieslice3D}
  
  \link{moviepoints3D} for plotting moving points in 3D
}

\author{Karline Soetaert <karline.soetaert@nioz.nl>}

\examples{
 x <- y <- z <- seq(-1, 1, by = 0.1)
 grid   <- mesh(x, y, z)
 colvar <- with(grid, x*exp(-x^2 - y^2 - z^2))

 movieslice3D  (x, y, z, colvar = colvar, ticktype = "detailed")

}
\keyword{ hplot }

