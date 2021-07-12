\name{Chesapeake data set}
\docType{data}
\alias{Chesapeake}
\alias{Ltrans}
\title{
  Particle transport in Chesapeake Bay
}

\description{
  \code{Chesapeake} is a \code{list} with the bathymetry of Chesapeake Bay, Mid-Atlantic Bight
  and the initial position of the particles.
  
  \code{Ltrans} is an \code{array} with output of the Lagrangian Transport model (Ltrans v.2) from
  Chesapeake Bay mouth, at 37 dgN in the Mid-Atlantic Bight (Schlag and North, 2012).
}

\usage{
data(Chesapeake)
data(Ltrans)
}

\format{
  \itemize{
    \item \code{Chesapeake} is a \code{list} with the bathymetry of the area. 
    There are 154 x-values, at 77 y-values. 
  
    It contains:
    \itemize{
      \item \code{lon}, the longitude, (154 x 77), dg East.
      \item \code{lat}, the latitude, (154 x 77), dg North.
      \item \code{depth}, the bathymetry (154 x 77), metres.  
      \item \code{init}, the initial condition of the particles, a (608 x 4) matrix
        with (\code{lon, lat, depth, source}) values.
    }

    \item \code{Ltrans} contains output of the Lagrangian particle transport model, 
    in the Chesapeake mouth area. 608 particles were released in two square regions, 
    and their positions followed over 108 output steps. It is an array of dimension
    (608 x 4 x 108), and which contains for each of the 608 particles, and at each 
    of the 108 output steps the following:
    \itemize{
      \item \code{lon}, the longitude of each particle.
      \item \code{lat}, the latitude of each particle.
      \item \code{depth}, the depth of each particle.
      \item \code{source}, the square region of release, either \code{1} or \code{2}.
    }
  }
}
\author{
  Karline Soetaert <karline.soetaert@nioz.nl>
}

\examples{
# save plotting parameters
 pm <- par("mfrow")
 mar <- par("mar")
   
## =============================================================================
## Show bathymetry and initial distribution of particles
## =============================================================================  

 par(mfrow = c(1, 1))

 lon <- Chesapeake$lon
 lat <- Chesapeake$lat
 depth <- Chesapeake$depth
 init  <- Chesapeake$init
 
 image2D(z = depth, x = lon, y = lat, clab = c("depth", "m"), 
   xlab = "lon", ylab = "lat")

# position of particles 
 with (init, scatter2D(lon, lat, colvar = source, pch = 16, cex = 0.5, 
   col =  c("green", "orange"), add = TRUE, colkey = FALSE))

 par (mar = c(2, 2, 2, 2))
# same, as persp plot
 persp3D(x = lon, y = lat, z = -depth, scale = FALSE, 
   expand = 0.02, main = "initial particle distribution", 
   plot = FALSE)

 points3D(x = init$lon, y = init$lat, z = -init$depth, 
  colvar = init$source, col = c("green", "orange"), 
  pch = 16, cex = 0.5, 
  add = TRUE, colkey = FALSE, plot = FALSE)

\dontrun{
   plotdev(lighting = TRUE, lphi = 45)
} 
 plotrgl(lighting = TRUE, smooth = TRUE)

## =============================================================================
## Tracer output in 3D, traditional device
## =============================================================================  
\dontrun{
 par(mfrow = c(2, 1), mar = c(2, 2, 2, 2)) 
 for (i in c(50, 100))
   tracers3D(Ltrans[, 1, i], Ltrans[, 2, i], Ltrans[, 3, i], 
             colvar = Ltrans[ ,4, i], col = c("green", "orange"),
             pch = 16, cex = 0.5, 
             surf = list(x = lon, y = lat, z = -depth, scale = FALSE, 
               expand = 0.02, colkey = FALSE, shade = 0.3, 
               colvar = depth), colkey = FALSE,
             main = paste("time ", i))
}

## =============================================================================
## Tracer output in 3D, using rgl
## =============================================================================  

 persp3D(x = lon, y = lat, z = -depth, colvar = depth, scale = FALSE, 
   expand = 0.02, main = "particle distribution", plot = FALSE)

 plotrgl(lighting = TRUE, smooth = TRUE) 
 
# you may zoom to the relevant region, or cut a region
# cutrgl()  
 for (i in seq(1, 108, by = 4)) {
   tracers3Drgl(Ltrans[, 1, i], Ltrans[, 2, i], Ltrans[, 3, i], 
             colvar = Ltrans[ ,4, i], col = c("green", "orange"),
             main = paste("time ", i))
# remove # to slow down
#   Sys.sleep(0.1)  
 }  

# using function moviepoints3D
\dontrun{
 persp3Drgl(x = lon, y = lat, z = -depth, colvar = depth, scale = FALSE, 
   expand = 0.02, main = "particle distribution", 
   lighting = TRUE, smooth = TRUE) 

 nt <- dim(Ltrans)[3]  # number of time points
 np <- dim(Ltrans)[1]  # number of particles
 
 times <- rep(1:nt, each = np)
 
 moviepoints3D(x = Ltrans[, 1, ], y = Ltrans[, 2, ], z = Ltrans[, 3, ], 
               t = times, colvar = Ltrans[ ,4, ], col = c("green", "orange"),
               cex = 5, ask = TRUE)

}
## =============================================================================
## Tracer output in 2D, traditional device
## =============================================================================  

 par(mfrow = c(2, 2)) 
 for (i in seq(10, 106, length.out = 4)) 
   tracers2D(Ltrans[, 1, i], Ltrans[, 2, i],  
             colvar = Ltrans[ ,4, i], col = c("green", "orange"),
             pch = 16, cex = 0.5, 
             image = list(x = lon, y = lat, z = depth), colkey = FALSE,
             main = paste("time ", i))

## =============================================================================
## Tracer output in 2D, rgl
## =============================================================================  
 
 image2Drgl (x = lon, y = lat, z = depth)
 for (i in seq(1, 108, by = 3)) {
   tracers2Drgl(Ltrans[, 1, i], Ltrans[, 2, i],  
             colvar = Ltrans[ ,4, i], col = c("green", "orange"))
# remove # to slow down
#   Sys.sleep(0.1)  
 }  

# reset plotting parameters
 par(mar = mar)
 par(mfrow = pm)
}

\references{
Schlag, Z. R., and E. W. North. 2012. Lagrangian TRANSport model (LTRANS v.2) User's Guide. 
  University of Maryland Center for Environmental Science, Horn Point Laboratory. Cambridge, MD. 183 pp.

North, E. W., E. E. Adams, S. Schlag, C. R. Sherwood, R. He, S. Socolofsky. 2011. 
Simulating oil droplet dispersal from the Deepwater Horizon spill with a Lagrangian approach. 
AGU Book Series: Monitoring and Modeling the Deepwater Horizon Oil Spill: A Record Breaking Enterprise.

}

\seealso{
  \link{Sylt3D} for output of a 3-D hydrodynamical model, GETM.

  \link{Oxsat} for a 3-D data set, package \code{plot3D}.
  
  \link{tracers2D} for plotting time series of tracer distributions in 2D

  \link{tracers3D} for plotting time series of tracer distributions in 3D
}

\keyword{datasets}

