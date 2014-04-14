\name{NIOZ Westerschelde monitoring}
\docType{data}
\alias{WSnioz}
\alias{WSnioz.table}
\title{
  NIOZ monitoring data of Westerschelde estuary.
}

\description{
  Part of the long-term monitoring data of the Westerschelde estuary, from 
  1996 till 2004.

  A total of 17 stations were monitored on a monthly basis. 

  The dataset \code{WSnioz} is in long format and contains the following variables: 
  oxygen, temperature, salinity, nitrate, ammonium, nitrite, phosphate, 
  silicate and chlorophyll.

  The dataset \code{WSnioz.table} is in tabular format.
    
  The full dataset can be downloaded from:
  \url{http://www.nioz.nl/monitoring-data-downloads}  
}

\usage{
data(WSnioz)
data(WSnioz.table)
}

\format{
  \code{WSnioz} is a \code{data.frame} with the following columns: 
  \itemize{
    \item \code{SamplingDateTime}, a string with the date and time of sampling.
    \item \code{SamplingDateTimeREAL}, a numeric value with day as per 1900.
    \item \code{Station}, the station number.
    \item \code{Latitude}, \code{Longitude}, the station position.
    \item \code{VariableName}, the variable acronym.
    \item \code{VariableDesc}, description of the variable.
    \item \code{VariableUnits}, units of measurement.
    \item \code{DataValue}, the actual measurement.
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
## Show stations and measured variables
## =============================================================================  
 unique(WSnioz[,c("Station", "Latitude", "Longitude")])
 unique(WSnioz[,c("VariableName", "VariableDesc")])
                                  
## =============================================================================
## An image for Nitrate:
## =============================================================================

# 1. use db2cross to make a cross table of the nitrate data
# assume that samples that were taken within 5 days belong to the same
# monitoring campaign (df.row).

 NO3 <- db2cross(WSnioz, row = "SamplingDateTimeREAL", 
       col = "Station", val = "DataValue", 
       subset = (VariableName == "WNO3"), df.row = 5)

# 2. plot the list using image2D; increase resolution
 image2D(NO3, resfac = 3)   

## =============================================================================
## All timeseries for one station
## =============================================================================

 st1 <- db2cross(WSnioz, row = "SamplingDateTimeREAL", 
       col = "VariableName", val = "DataValue", 
       subset = (WSnioz$Station == 1), df.row = 5)

 Mplot(cbind(st1$x/365+1900,st1$z))

## =============================================================================
## All timeseries for multiple stations
## =============================================================================

 dat <- NULL
 for (st in 1:17) {
   dd <- db2cross(WSnioz, row = "SamplingDateTimeREAL", 
          col = "VariableName", val = "DataValue", 
          subset = (WSnioz$Station == st), df.row = 5)
   dat <- rbind(dat,  cbind(st, time = dd$x/365+1900, dd$z))
 }

# select data for station 1, 17
 dat2 <- Msplit(dat, split = "st", subset = st \%in\% c(1, 17))
 names(dat2)
  
 Mplot(dat2, lty = 1)

## =============================================================================
## tabular format of the same data
## =============================================================================
 head(WSnioz.table)

# plot all data from station 1:
 Mplot(WSnioz.table, select = 3:11, subset = Station == 1, legend = FALSE)

 Mplot(Msplit(WSnioz.table, "Station", subset = Station \%in\% c(1, 13)) , 
   select = c("WNO3", "WNO2", "WNH4", "WO2"), lty = 1, lwd = 2,
   xlab = "Daynr", log = c("y", "y", "y", ""), 
   legend = list(x = "left", title = "Station")) 



# reset plotting parameters
 par(mar = mar)
 par(mfrow = pm)
}

\references{
Soetaert, K., Middelburg, JJ, Heip, C, Meire, P., Van Damme, S., Maris, T., 2006. Long-term change in dissolved inorganic nutrients in the heterotrophic Scheldt estuary (Belgium, the Netherlands). Limnology and Oceanography 51: 409-423.   DOI: 10.4319/lo.2006.51.1_part_2.0409

  \url{http://aslo.org/lo/toc/vol_51/issue_1_part_2/0409.pdf}
}

\seealso{
  \link{image2D} for plotting images, package \code{plot3D}.

  \link{ImageOcean} for an image of the ocean's bathymetry, package \code{plot3D}.

  \link{scatter2D} for making scatterplots, package \code{plot3D}.
  
  \link{Oxsat} for a 3-D data set, package \code{plot3D}.
}

\keyword{datasets}

