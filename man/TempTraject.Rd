\name{Profile data set}
\docType{data}
\alias{TrackProf}
\title{
  Temperature profiles made along a ship track.
}

\description{
  Profiles of temperature made along a ship track, originally made available by US NOAA NODC.
  
  The data were merged from 29 input files named \code{gtspp_103799_xb_111.nc}
  till \code{gtspp_103827_xb_111.nc}.

  These data were acquired from the US NOAA National Oceanographic Data Center 
  (NODC) on 9/06/2012 from \url{http://www.nodc.noaa.gov/gtspp/}. 
}

\usage{
data(TrackProf)
}

\format{
 list with 
  \itemize{
   \item \code{meta}, a \code{data.frame} with the metadata, containing for each of the 
    29 profiles the following:
   \itemize{
    \item \code{station}, the number of the station (part of the original filename).
    \item \code{filename}, the original name of the NetCDF file.
    \item \code{date}, the date of sampling.
    \item \code{time}, the time of sampling, a number relative to 1-1-1900 0 hours.
    \item \code{longitude}, dg E. 
    \item \code{latitutde}, dg N.
   }
   \item \code{temp}, the seawater temperature, at the \code{depth} of the 
     measurement in dg C. A matrix of dimension \code{(29, 93)} for the 
     29 profiles and (at most) 93 depth values; \code{NA} means no measurement.
   \item \code{depth}, the depth of the measurement in \code{temp}, in metres, 
     positive downward. A matrix of dimension \code{(29, 93)} for the 
     29 profiles and (at most) 93 depth values; \code{NA} means no measurement.
  }
}
\author{
  Karline Soetaert <karline.soetaert@nioz.nl>
}

\examples{
# save plotting parameters
 pm <- par(mfrow = c(2, 2))
 mar <- par("mar")
   
## =============================================================================
## show the metadata
## =============================================================================
 print(TrackProf$meta)

## =============================================================================
## display the cruisetrack on the Ocean Bathymetry data  
## =============================================================================  
 # 1. plots the ocean's bathymetry and add sampling positions
 ImageOcean(xlim = c(-50, 50), ylim = c(-50, 50), 
            main = "cruise track")  
 points(TrackProf$meta$longitude, TrackProf$meta$latitude, pch = "+")

# mark starting point
 points(TrackProf$meta$longitude[1], TrackProf$meta$latitude[1], 
        pch = 18, cex = 2, col = "purple")

## =============================================================================    
## image plots of raw data
## =============================================================================    

 image2D(z = TrackProf$depth, main = "raw depth values",  
       xlab = "station nr", ylab = "sample nr", clab = "depth")
 image2D(z = TrackProf$temp, main = "raw temperature values",  
       xlab = "station nr", ylab = "sample nr", clab = "dgC")

## =============================================================================    
## image plots of temperatures at correct depth
## =============================================================================    
  
# water depths to which data set is interpolated
 depth <- 0 : 809    

# map from "sigma" to "depth" coordinates
 Temp_Depth <- mapsigma (TrackProf$temp, sigma = TrackProf$depth, 
   depth = depth)$var

# image with depth increasing downward and increased resolution (resfac)  
 image2D(z = Temp_Depth, main = "Temperature-depth",
       ylim = c(809, 0), y = depth, NAcol ="black", resfac = 2, 
       xlab = "station nr", ylab = "depth, m", clab = "dgC")
   
## =============================================================================    
## scatterplot of surface values on ocean bathymetry
## =============================================================================    

 par(mar = mar + c(0, 0, 0, 2))
 par(mfrow = c(1, 1))
  
# No colors, but add contours
 ImageOcean(xlim = c(-30, 30), ylim = c(-40, 40), 
          main = "cruise track", col = "white", contour = TRUE) 
  
# use data set TrackProf to add measured temperature, with color key
 with (TrackProf, 
   scatter2D(colvar = temp[,1], x = meta[ ,"longitude"],
         y = meta[ ,"latitude"], clab = "temp", 
         add = TRUE, pch = 18, cex = 2))
  
# reset plotting parameters
 par(mar = mar)
 par(mfrow = pm)
}

\references{
  \url{http://www.nodc.noaa.gov/gtspp/}

  U.S. National Oceanographic Data Center: Global Temperature-Salinity Profile Programme.
  June 2006. U.S. Department of Commerce, National Oceanic and Atmosphere Administration,
  National Oceanographic Data Center, Silver Spring, Maryland, 20910. Date of Access: 9/06/2012.
}

\seealso{
  \link{image2D} for plotting images, package \code{plot3D}.

  \link{ImageOcean} for an image of the ocean bathymetry, package \code{plot3D}.

  \link{scatter2D} for making scatterplots, package \code{plot3D}.
  
  \link{Oxsat} for a 3-D data set, package \code{plot3D}.
}

\keyword{datasets}

