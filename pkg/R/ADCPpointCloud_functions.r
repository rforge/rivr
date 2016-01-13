# This code produces a 2D flow layers or a 3D point cloud of flow data from 
# RiverSurveyor exported data in matlab format
#################################################################################

#Copyright (C) 2014 Claude Flener, University of Turku.

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

#################################################################################



#options(max.print=5000) #########
#i=1; j=1

# Required packages:
# require(rgdal) 
# require(R.matlab)
# require(geometry)
# require(sp)
# require(RColorBrewer)
# require(classInt)
# require(maptools)
# require(raster)
# #require(ggplots)


#' riv2FlowLayers
#'
#' This function creates point layers of flow direction, speed, and bathymetry based on RiverSurveyor ADCP data and, optionally, external GNSS data.
#'
#' Three flow layers are created: Surface, Depth-integrated (average), Near-bed flow.
#'
#' In addition, bathymetry is created: depth and bed elevation (and surface elevation for post-processing purposes)
#'
#' @param path the path to the input files (default = working directory.) Automatically handles batch processing if path is a directory. 
#' @note \code{path}: If you provide a full path to a RiverSurveyor *.mat file, that file will be used for processing and only the relevant GNSS points form the external GNSS file (if applicable) will be used. If your measurement is split over several *.mat files, put them all in the same directory and input the path to the directory instead of a file. The function will batch-process all *.mat files in that directory and combine the resulting points into one output file.
#' @param externalGPS \code{GNSS} if external GNSS data is to be used for computing point locations (default = NULL)
#' @param TimeShiftSecs Optional: an integer as the number of seconds to shift GNSS data, if not in UTC (default = 0)
#' @param plane Optional: a dataFrame output by \code{addPlane.z()}. (default = NULL)
#' @note  If \code{plane} is provided, this adds theoretical surface elevations based on plane equation: surface.z and bed.z based on surface.z
#' @param z.out Z coordinate of output file. Takes one of three strings as values: "Elevation", "plane.z", "bed.z" (default = "Elevation")
#' @param out  Output filetype. can take either of these strings as values: "csv", "shape", "all" (default), "none"
#' @note if \code{out == "none"}, no output file is created but the \code{spatialPointsDataFrame} is returned on exit; all other options produce output files but return empty.
#' @param outdir target directory for output files (default = ".")
#' @param outfile Optional: a string as the file name base for output files 
#' @note \code{outfile} defaults to NULL, meaning that the file name of the output file is automatically created form the first ADCP *.mat file used for input. 
#' @param fileAppendix a string appendix to file name
#' @note \code{fileAppendix} is intended to be used together with automatic file names (outfile == NULL) to allow customisation of file names while retaining the convenience of automatically generated file names
#' @param verbose logical, prints the first 100 lines of the result to screen (default =TRUE)
#' @param plot logical, plots the points created. (default = TRUE)
#' @param map logical , creates PDF-maps of the bathymetry and flow data (default = TRUE) 
#' @return A \code{spatialPointsDataFrame}, or, a file containing 3D point data (XYZ) either as CSV, ESRI shape, or both (default). The flow and depth data, as well as a bunch of other variables from the ADCP are included to be used as attributes / scalar fields. 
#' @seealso \code{\link{addPlane.z}}
#' @author Claude Flener \email{claude.flener@@utu.fi}
#' @export
riv2FlowLayers <- function(path=getwd(), externalGPS=NULL, TimeShiftSecs=0, plane=NULL, z.out="Elevation", out="all", outdir=".", outfile=NULL, fileAppendix="", verbose=TRUE, plot=TRUE, map=TRUE){
    rivDat <- data.frame()
    pathnames <- batchFiles("mat", path=path)
    #cat(pathnames)
    for (path in pathnames) { # batch loop
              cat("Reading MAT file: ", basename(path), "\n", sep="")
        filename <- sub(".mat$|$","",basename(path))

    riv <- readRivMat(path)
        #cat("path :", path, "\n")
        #cat("pathnames :", pathnames, "\n")
        #cat("filename :", filename, "\n")
    attach(riv)
    # get basic values to include in output file:
    UTC             <- unlist(GPS$Utc)
    GpsGeoid        <- unlist(GPS$GpsGeoid)
    depth           <- as.numeric( Summary$Depth )
    Top.Q           <- as.numeric( Summary$Top.Q )
    Middle.Q        <- as.numeric( Summary$Middle.Q )
    Bottom.Q        <- as.numeric( Summary$Bottom.Q )
    Left.Q          <- as.numeric( Summary$Left.Q )
    Right.Q         <- as.numeric( Summary$Right.Q )
    Total.Q         <- as.numeric( Summary$Total.Q )
    Cells           <- as.numeric( Summary$Cells )
    Temperature     <- as.numeric( System$Temperature )
    Sample          <- as.numeric( System$Sample )
    sensorDepthSet  <- unlist(Setup$sensorDepth[1])
    VBdepth         <- as.numeric( BottomTrack$VB.Depth )
    BTdepth         <- as.numeric( BottomTrack$BT.Depth )
    first           <- which(System$Step==3)[1] # first measurement in Transect
    last            <- length(which(System$Step==3))
    rows            <- dim(WaterTrack$Velocity)[1]
    columns         <- dim(WaterTrack$Velocity)[2]
    pixels          <- round(round(100*Summary$Cells * System$Cell.Size)/5)
    max.pixels      <- max(pixels)
    CellsNumber     <- length(Cells)
    max.cells       <- max(Cells)
    min.cells       <- min(Cells)
    total.cells     <- sum(Cells)
    # Data format:
    # WaterTrack$Velocity[1, 1:3, 1:10]
    # = point 1, columns 1:3 (ENU), cells 1:10

    ######### Surface layer velocities and directions
    # surface vector using E_N for direction but E_N_|U| for length
    vel.vector.surface  <-  t(WaterTrack$Velocity[1,1:3, ])   # surface =[1,,]
    #WaterTrack$Velocity[1, 1:3, 1:10]
    #       [,1]   [,2]   [,3]   [,4]   [,5]   [,6]   [,7]   [,8]   [,9]  [,10]
            #[1,]  0.460  0.639  0.462  0.480  0.595  0.517  0.604  0.554  0.536  0.656
            #[2,]  0.780  0.637  0.589  0.503  0.775  0.660  0.862  0.772  0.651  0.692
            #[3,] -0.018 -0.039 -0.020 -0.061 -0.096 -0.076 -0.037 -0.018 -0.032 -0.081
    vel.magnitude.surface <- sqrt(vel.vector.surface[ ,1]^2 + vel.vector.surface[ ,2]^2 + abs(vel.vector.surface[ ,3])^2) # speed
    vel.dir.north.surface<- 180/pi * atan2(-vel.vector.surface[ ,1],-vel.vector.surface[ ,2])+180

    ######### Bottom layer velocities and directions
    #The following works to extract data straight from the 3D array, but it is extremely slow
    #vel.vector.bottom  <-  sapply(1:length(Cells),function(x) WaterTrack$Velocity[Cells,,][x,1:2,x])
    #instead, build an empty array with the required dimensions and fill it using a for loop
    vel.vector.bottom <- matrix(nrow=CellsNumber, ncol=3)
    #for(i in 1:length(Cells)) vel.vector.bottom[i,] <- WaterTrack$Velocity[Cells[i],1:2,i ] # works as long as Cells > 0
    #We need to check for Cells[i]==0 to avoid skipping NAs in the table
    for(i in 1:length(Cells)) {
        if(Cells[i] != 0)
            vel.vector.bottom[i,] <- WaterTrack$Velocity[Cells[i],1:3,i ]
        else
            vel.vector.bottom[i,] <- NA
    }
    vel.magnitude.bottom <- sqrt(vel.vector.bottom[ ,1]^2 + vel.vector.bottom[ ,2]^2 + abs(vel.vector.bottom[ ,3])^2) # speed
    vel.dir.north.bottom<- 180/pi * atan2(-vel.vector.bottom[ ,1],-vel.vector.bottom[ ,2])+180

    ######### Depth-averaged velocities and directions

    Mean.Vel.x <-  Summary$Mean.Vel[,1]
    Mean.Vel.y <-  Summary$Mean.Vel[,2]

    Mean.Vel.magnitude <- sqrt(Mean.Vel.x^2 + Mean.Vel.y^2)
    #Mean.vel.dir.polar <- atan(y/x)
    Mean.vel.dir.north <- 180/pi * atan2(-Mean.Vel.x,-Mean.Vel.y)+180


    #set up output dataframe
    dat <- data.frame(
                      UTC               = UTC,
                      depth             = depth,
                      Top.Q             = Top.Q,
                      Middle.Q          = Middle.Q,
                      Bottom.Q          = Bottom.Q,
                      Left.Q            = Left.Q,
                      Right.Q           = Right.Q,
                      Total.Q           = Total.Q,
                      Temperature       = Temperature,
                      Sample            = Sample,
                      sensDpthSet       = sensorDepthSet,
                      VBdepth           = VBdepth,
                      BTdepth           = BTdepth,
                      #snsDpthSet        = sensorDepthSet,
                      VBdepth           = VBdepth,
                      BTdepth           = BTdepth,
                      Sample            = Sample,
                      Temp              = Temperature,
                      TopQ              = Top.Q,
                      MiddleQ           = Middle.Q,
                      BottomQ           = Bottom.Q ,
                      TotalQ            = Total.Q,
                      Cells             = Cells,
                      DA.VelMagn        = Mean.Vel.magnitude,
                      DA.VelDNorth      = Mean.vel.dir.north,
                      Surf.VelMagn      = vel.magnitude.surface,
                      Surf.VelDNorth    = vel.dir.north.surface,
                      Btm.VelMagn       = vel.magnitude.bottom,
                      Btm.VelDNorth     = vel.dir.north.bottom
                      )

    if(!is.null(externalGPS)){
        # merge riv data with external (RTK)GPS data)
        rivMonthSecs <- getRivMonthSecs(path, UTC)
        rivMonthSecs <- rivMonthSecs + TimeShiftSecs
        dat <- cbind(rivMonthSecs, dat)
        #cat(str(externalGPS), "\n")
        #cat(str(dat), "\n")

            #Check for duplicates
            duplicates <- length(which(duplicated(externalGPS$monthSec))) 
            if(duplicates != 0){
                warning(c("Duplicate monthSeconds in GPS data.\n  Automatically removing ", duplicates," duplicates\n"), immediate.=TRUE)
                externalGPS <- externalGPS[ which(!duplicated(externalGPS$monthSec)) , ]
            } 

        dat <- merge(externalGPS, dat, by.x="monthSec", by.y="rivMonthSecs")
        if(dim(dat)[1]==0){
          warning(c("No points remaining after merging RiverSurveyor file ", basename(path),  " with GNSS data.\n"), immediate.=TRUE)
        #}
          #if(match(path, pathnames) < length(pathnames)){
          if(length(pathnames) > 1){
            detach(riv)        
            next          
          }else{
            detach(riv)        
            return
          }
        }

        # add bed elevation based on external GPS data if no theoretical
        # plane is provided
        if(is.null(externalGPS$plane.z)){
            dat$bed.z <- dat$Elevation - dat$depth
        }else{
        #cat(str(dat), "\n")
            dat$bed.z <- dat$plane.z - dat$depth
        }

        #cat(str(dat), "\n")
    }else{
        # Extract RiverSurveyor's internal GPS data
        rivGPSdata <- data.frame(   Easting <-unlist(GPS$UTM[,1])       ,
                                    Northing <-unlist(GPS$UTM[,2])      ,
                                    Altitude <-unlist(GPS$Altitude)     ,
                                    EllGRS80 <- Altitude+GpsGeoid       ,
                                    Satellites <-unlist(GPS$Satellites) ,
                                    HDOP <-unlist(GPS$HDOP)             ,
                                    GPSqual <-unlist(GPS$GPS.Quality)   )
        dat <- cbind(rivGPSdata, dat)
        # add bed elevation based on internal track data if no theoretical
        # plane is provided
        if(is.null(plane)){
            dat$bed.z <- dat$EllGRS80 - dat$depth
        }else{
        dat <- addPlane.z(dat,plane)
        #cat(str(dat), "\n")
            dat$bed.z <- dat$plane.z - dat$depth
        }


    }

        #cat(str(dat), "\n")

    if(plot){ # plot intermediate data in loop
         coord <- SpatialPoints(data.frame(dat$Easting, dat$Northing))
         rivgpsSP <- SpatialPointsDataFrame(data=dat, coords=coord)
         plot(rivgpsSP, pch=20, main= basename(path))
         legend("topright", basename(path), bty="n")         
    }

 rivDat <- rbind(rivDat, dat)
        dat <- NA
detach(riv)
      } # ends batch loop

    dat <- rivDat


        # Write shape file
            # create coordinates
    if(z.out=="Elevation") coord <- SpatialPoints(data.frame(dat$Easting, dat$Northing, dat$Elevation))
    if(z.out=="plane.z") coord <- SpatialPoints(data.frame(dat$Easting, dat$Northing, dat$plane.z))
    if(z.out=="bed.z") coord <- SpatialPoints(data.frame(dat$Easting, dat$Northing, dat$bed.z))

    rivgpsSP <- SpatialPointsDataFrame(data=dat, coords=coord)

    if(is.null(outfile)) outfile <- sub(".mat$|$","",basename(path))

    if(plot) plot(rivgpsSP, pch=20, main=outfile) # plot final data

        if(map){
            # Plot points resulting points:
            colors <- brewer.pal(7, "Blues")
            # colors <- rev( colors ) # reverse color palette
            #set breaks for the 7 colors 
            brks<-classIntervals(rivgpsSP$depth, n=7, style="quantile")
            brks<- brks$brks
            #plot the map
            plot(rivgpsSP, col=colors[findInterval(rivgpsSP$depth, brks,all.inside=TRUE)], axes=F, pch=20)
            #add a legend
            legend("bottomleft", legend=leglabs(round(brks, 1)), fill=colors, bty="n",x.intersp = .5, y.intersp = .8, cex=0.8, title="depth (m)", inset=c(0.02,0.1))
            plotwidth <- max(rivgpsSP@coords[,1]) - min(rivgpsSP@coords[,1])
            plotheight <- max(rivgpsSP@coords[,2]) - min(rivgpsSP@coords[,2])
            maxplotdim <- max(c(plotwidth, plotheight))
            ifelse(maxplotdim %/% 100 > 10 ,scalebarlength <- maxplotdim %/% 1000 *200,scalebarlength <- maxplotdim %/% 100 *20) 
            scalebar(scalebarlength, type='bar', divs=4, below="m", adj=c(0,-1.5) , cex=0.75)
            dev.print(pdf, paste(outdir,"/", outfile, fileAppendix,".depth_map.pdf",sep=""))
            
            # plot the depth-averaged velocities
            colors <- brewer.pal(7, "YlOrRd")
            # colors <- rev( colors ) # reverse color palette
            #set breaks for the 7 colors 
            brks<-classIntervals(rivgpsSP$DA.VelMagn, n=7, style="quantile")
            brks<- brks$brks
            #plot the map
            plot(rivgpsSP, col=colors[findInterval(rivgpsSP$DA.VelMagn, brks,all.inside=TRUE)], axes=F, pch=20)
            #add a legend
            legend("bottomleft", legend=leglabs(round(brks, 1)), fill=colors, bty="n",x.intersp = .5, y.intersp = .8, cex=0.8, title="velocity (m/s)", inset=c(0.02,0.1))
            #add a scale bar
            plotwidth <- max(rivgpsSP@coords[,1]) - min(rivgpsSP@coords[,1])
            plotheight <- max(rivgpsSP@coords[,2]) - min(rivgpsSP@coords[,2])
            maxplotdim <- max(c(plotwidth, plotheight))
            ifelse(maxplotdim %/% 100 > 10 ,scalebarlength <- maxplotdim %/% 1000 *200,scalebarlength <- maxplotdim %/% 100 *20) 
            scalebar(scalebarlength, type='bar', divs=4, below="m", adj=c(0,-1.5) , cex=0.75)
            #print to pdf 
            dev.print(pdf, paste(outdir,"/", outfile, fileAppendix,".velocity_map.pdf",sep=""))
        }

        # Write CSV file
            if (out == "csv" || out== "all"){
            write.table(rivgpsSP@data, paste(outdir, "/", outfile, fileAppendix, ".csv", sep="" ), sep = ",",  row.names=FALSE, col.names=TRUE)
            }
    if (out == "shape" || out== "all"){
                writeOGR(rivgpsSP, outdir , paste(outfile, fileAppendix, sep="")  , driver="ESRI Shapefile", layer_options="POINTZ")
            }
        if(verbose) print(rivgpsSP@data, max=50)
        #ifelse(out=="none", return(rivgpsSP), return())

       


        return(rivgpsSP) 
}



#' riv2FlowPointCloud
#'
#' This function creates 3D point clouds of flow direction, speed, river bed and water surface based on RiverSurveyor ADCP data and, optionally, external GNSS data.
#'
#' @param path the path to the input files (default = working directory.) Automatically handles batch processing if path is a directory. 
#' @note \code{path}: If you provide a full path to a RiverSurveyor *.mat file, that file will be used for processing and only the relevant GNSS points form the external GNSS file (if applicable) will be used. If your measurement is split over several *.mat files, put them all in the same directory and input the path to the directory instead of a file. The function will batch-process all *.mat files in that directory and combine the resulting points into one output file.
#' @param externalGPS \code{GNSS} if external GNSS data is to be used for computing point locations (default = NULL)
#' @param TimeShiftSecs Optional: an integer as the number of seconds to shift GNSS data, if not in UTC (default = 0)
#' @param plane Optional: a dataFrame output by \code{addPlane.z()}. (default = NULL)
#' @note  If \code{plane} is provided, this adds theoretical surface elevations based on plane equation: surface.z and bed.z based on surface.z
#' @param z.out Z coordinate of output file. Takes one of three strings as values: "Elevation", "plane.z", "bed.z" (default = "Elevation")
#' @param out  Output filetype. can take either of these strings as values: "csv", "shape", "all" (default), "none"
#' @note if \code{out == "none"}, no output file is created but the \code{spatialPointsDataFrame} is returned on exit; all other options produce output files but return empty.
#' @param outdir target directory for output files (default = ".")
#' @param outfile Optional: a string as the file name base for output files 
#' @note \code{outfile} defaults to NULL, meaning that the file name of the output file is automatically created form the first ADCP *.mat file used for input. 
#' @param fileAppendix a string appendix to file name
#' @note \code{fileAppendix} is intended to be used together with automatic file names (outfile == NULL) to allow customisation of file names while retaining the convenience of automatically generated file names
#' @param verbose logical, prints the first 100 lines of the result to screen (default =TRUE)
#' @return A \code{spatialPointsDataFrame}, or, a file containing 3D point data (XYZ) either as CSV, ESRI shape, or both (default). The flow and depth data, as well as a bunch of other variables from the ADCP are included to be used as attributes / scalar fields. 
#' @seealso \code{\link{addPlane.z}}
#' @author Claude Flener \email{claude.flener@@utu.fi}
#' @export
riv2FlowPointCloud <- function(path=getwd(), externalGPS=NULL, TimeShiftSecs=0, plane=NULL, z.out="Elevation", out="all", outdir=".", outfile=NULL, fileAppendix="", verbose=TRUE ){
    rivDat <- data.frame()
    #Set up matrix to fill with points
        #pointNames <- c("x", "y", "z", "sample", "cells", "cell.size", "cell.start", "UTC", "GPS.z" ,"surface.z", "bed.z", "depth", "E", "N", "U", "vel", "dir")
        #points <- matrix(nrow=total.cells ,ncol=length(pointNames), dimnames=list(c() , pointNames)) 
    pathnames <- batchFiles("mat", path=path)
    #cat(pathnames)
    for (path in pathnames) {
              cat("Reading MAT file: ", basename(path), "\n", sep="")
        filename <- sub(".mat$|$","",basename(path))

        riv <- readRivMat(path)
        #cat("riv :", str(riv), "\n")
        #cat("path :", path, "\n")
        #cat("pathnames :", pathnames, "\n")
        #cat("filename :", filename, "\n")
        attach(riv)
        # get basic values to include in output file:
        UTC             <- unlist(GPS$Utc)
        depth           <- as.numeric( Summary$Depth )
        Top.Q           <- as.numeric( Summary$Top.Q )
        Middle.Q        <- as.numeric( Summary$Middle.Q )
        Bottom.Q        <- as.numeric( Summary$Bottom.Q )
        Left.Q          <- as.numeric( Summary$Left.Q )
        Right.Q         <- as.numeric( Summary$Right.Q )
        Total.Q         <- as.numeric( Summary$Total.Q )
        Cells           <- as.numeric( Summary$Cells )
        Temperature     <- as.numeric( System$Temperature )
        Sample          <- as.numeric( System$Sample )
        sensorDepthSet  <- unlist(Setup$sensorDepth[1])
        VBdepth         <- as.numeric( BottomTrack$VB.Depth )
        BTdepth         <- as.numeric( BottomTrack$BT.Depth )
        first           <- which(System$Step==3)[1] # first measurement in Transect
        last            <- length(which(System$Step==3))
        rows            <- dim(WaterTrack$Velocity)[1]
        columns         <- dim(WaterTrack$Velocity)[2]
        pixels          <- round(round(100*Summary$Cells * System$Cell.Size)/5)
        max.pixels      <- max(pixels)
        CellsNumber     <- length(Cells)
        max.cells       <- max(Cells)
        min.cells       <- min(Cells)
        total.cells     <- sum(Cells)
        # Data format:
        # WaterTrack$Velocity[1, 1:3, 1:10]
        # = point 1, columns 1:3 (ENU), cells 1:10


        #set up output dataframe
        dat <- data.frame(
                          UTC               = UTC,
                          depth             = depth,
                          Temperature       = Temperature,
                          Sample            = Sample,
                          sensDpthSet       = sensorDepthSet,
                          VBdepth           = VBdepth,
                          BTdepth           = BTdepth,
                          Temp              = Temperature,
                          Cells             = Cells,
                          Cell.Start        = System$Cell.Start,
                          Cell.Size         = System$Cell.Size
                          )

        if(!is.null(externalGPS)){
            # merge riv data with external (RTK)GPS data)
            rivMonthSecs <- getRivMonthSecs(path, UTC)
            rivMonthSecs <- rivMonthSecs + TimeShiftSecs
            dat <- cbind(rivMonthSecs, dat)
            #cat(str(externalGPS), "\n")
            #cat(str(dat), "\n")  
            #Check for duplicates
            duplicates <- length(which(duplicated(externalGPS$monthSec))) 
            if(duplicates != 0){
                warning(c("Duplicate monthSeconds in GPS data.\n  Automatically removing ", duplicates," duplicates\n"), immediate.=TRUE)
                externalGPS <- externalGPS[ which(!duplicated(externalGPS$monthSec)) , ]
            } 

            dat <- merge(externalGPS, dat, by.x="monthSec", by.y="rivMonthSecs")
            # add bed elevation based on external GPS data if no theoretical
            # plane is provided
            if(is.null(externalGPS$plane.z)){
                dat$bed.z <- dat$Elevation - dat$depth
                dat$surface.z <- dat$Elevation
            }else{
            #cat(str(dat), "\n")
                dat$bed.z <- dat$plane.z - dat$depth
                dat$surface.z <- dat$plane.z
            }

        }else{
            # Extract RiverSurveyor's internal GPS data
            rivGPSdata <- data.frame(   Easting <-unlist(GPS$UTM[,1])       ,
                                        Northing <-unlist(GPS$UTM[,2])      ,
                                        Altitude <-unlist(GPS$Altitude)     ,
                                        EllGRS80 <- Altitude+GpsGeoid       ,
                                        Satellites <-unlist(GPS$Satellites) ,
                                        HDOP <-unlist(GPS$HDOP)             ,
                                        GPSqual <-unlist(GPS$GPS.Quality)   )
             dat <- cbind(rivGPSdata, dat)
             dat$surface.z <- dat$EllGRS80
        }
cat("dat: ", str(dat), "\n\n")
        #Extract velocities for each measurement cell into 3D point cloud
        #create simple dataframe to include all data for each "point"
        pointNames <- c("x", "y", "z", "sample", "cells", "cell.size", "cell.start", "monthSec", "GPS.z" ,"surface.z", "bed.z", "depth", "E", "N", "U", "vel", "dir", "zX1.5", "bed.zX1.5", "zX2", "bed.zX2")
        points <- matrix(nrow=sum(dat$Cells) ,ncol=length(pointNames), dimnames=list(c() , pointNames)) 

#cat("points: ", str(points), "\n")

        row <- 1 # initialize row counter outside loops
        for(i in dat$Sample){
            #cat("i = ", i,"\n")
            #cat("Cells[i] = ", dat$Cells[i],"\n")
            #cat("sample = ", Sample,"\n")
           if(!is.na(dat$Cells[i]) && dat$Cells[i]>0){
               for(j in 1:dat$Cells[i]){
                    #cat("j = ", j,"\n")
                    #cat("row = ", row,"\n")
         
                   points[row,1] <-  dat$Easting[i]
                   points[row,2] <-  dat$Northing[i]
                   #points[row,3] <-  surface.z - System$Cell.Start[i] - (abs(1-j)) * ( System$Cell.Size[i] / 2 )
                   points[row,3] <-  dat$surface.z[i] - dat$Cell.Start[i] - j * dat$Cell.Size[i] + ( dat$Cell.Size[i] / 2 )
                   points[row,4] <-  dat$Sample[i]
                   points[row,5] <-  dat$Cells[i]
                   points[row,6] <-  System$Cell.Size[i]
                   points[row,7] <-  System$Cell.Start[i]
                   points[row,8] <-  dat$monthSec[i]
                   points[row,9] <-  dat$Elevation[i]
                   points[row,10] <-  dat$surface.z[i]
                   points[row,11] <-  dat$surface.z[i] - dat$depth[i]
                   points[row,12] <-  dat$depth[i]
                   points[row,13] <-  WaterTrack$Velocity[j,1,i ] # E
                   points[row,14] <-  WaterTrack$Velocity[j,2,i ] # N
                   points[row,15] <-  WaterTrack$Velocity[j,3,i ] # U
                   points[row,16] <-  sqrt(points[row,13]^2 + points[row,14]^2 + abs(points[row,15])^2) # speed
                   points[row,17] <- vel.dir.north.bottom<- 180/pi * atan2(-points[row,13], -points[row,14])+180 # direction in 2D
                   points[row,18] <-  dat$surface.z[i] - dat$Cell.Start[i]*1.5 - j * dat$Cell.Size[i]*1.5 + ( dat$Cell.Size[i]*1.5 / 2 )
                   points[row,19] <-  dat$surface.z[i] - (dat$depth[i])*1.5
                   points[row,20] <-  dat$surface.z[i] - dat$Cell.Start[i]*2 - j * dat$Cell.Size[i]*2 + ( dat$Cell.Size[i]*2 / 2 )
                   points[row,21] <-  dat$surface.z[i] - (dat$depth[i])*2
                   row <- row+1
                }
            }
        }

        rivDat <- rbind(rivDat, points)
               #cat("OK\n")
        dat <- NA
        points <- NA
        detach(riv)
        #detach(dat)
        #cat(str(rivDat))
    }
    dat <- rivDat

    dat$X <- NULL


        #cat(str(dat))
    dat <- dat[ is.finite(rowSums(dat)) , ] 
    
        #cat(str(dat))
    
        #cat(min(dat$x), "\n")
        #cat(max(dat$x), "\n")
        #cat(min(dat$y), "\n")
        #cat(max(dat$y), "\n")
        #cat(min(dat$z), "\n")
        #cat(max(dat$z), "\n")

        # Write shape file
            # create coordinates
    #if(z.out=="Elevation") coord <- SpatialPoints(data.frame(dat$Easting, dat$Northing, dat$Elevation))
    #if(z.out=="plane.z") coord <- SpatialPoints(data.frame(dat$Easting, dat$Northing, dat$plane.z))
    #if(z.out=="bed.z") coord <- SpatialPoints(data.frame(dat$Easting, dat$Northing, dat$bed.z))

    coord <- SpatialPoints(data.frame(dat$x, dat$y, dat$z))
    rivgpsSP <- SpatialPointsDataFrame(data=dat, coords=coord)
    if(is.null(outfile)) outfile <- sub(".mat$|$","",basename(path))
    # Write CSV file
    if (out == "csv" || out== "all"){
        write.table(rivgpsSP@data, paste( outdir,"/", outfile, fileAppendix, ".PointCloud.csv", sep="" ), sep = ",",  row.names=FALSE, col.names=TRUE)
    }
    if (out == "shape" || out== "all"){
        writeOGR(rivgpsSP, outdir, paste(outfile, fileAppendix,".PointCloud", sep="") , driver="ESRI Shapefile", layer_options="POINTZ")
    }
    if(verbose == TRUE) print(rivgpsSP@data, max=100)



    #ifelse(out=="none", return(rivgpsSP), return())
    return(rivgpsSP)
}






#' points2Plane 
#'
#' This function finds the equation for computing a plane based on three 3D points
#'
#' find the equation for fitting a plane: ax +by + cz = d
#'
#' @param points a \code{data.Frame} containing the coordinates of three points in XYZ format.
#' @return a \code{vector} containing the cross-product of three vectors and the offset d
#' @author Claude Flener \email{claude.flener@@utu.fi}
#' @export
#Requires package: geometry
points2Plane <- function(points=NA){
#     require(geometry)
    cat(length(points))
    ifelse( c("x", "y", "z") %in% names(points),points, names(points) <- c("x", "y", "z"))
    attach(points)
    # create 2 vectors from the three points
    v1 <- c(x[3], y[3], z[3]) - c(x[1], y[1], z[1])
    v2 <- c(x[2], y[2], z[2]) - c(x[1], y[1], z[1])
    xprod <- extprod3d(v1,v2) #get cross-product of vectors
    d <- xprod[1]*x[1] + xprod[2]*y[1] + xprod[3]*z[1]
    result <- c(xprod, d)
    names(result) <- c("xp1", "xp2", "xp3","d")
    return( result )
}



#' addPlane.z 
#'
#' This function adds elevations based on fitted plane
#'
#' @param data a \code{data.Frame} containing the coordinates of the points to be fitted to the plane.
#' @note \code{data} should contain at least XY coordinates, but can contain XYZ coordinates. Generally, these are GNSS coordinates from an external GNSS source. The coordinates will be preserved and the \code{plane.z) will be added.
#' @param plane a \code{vector} created by the \code{points2Plane} function
#' @return a \code{data.Frame} containing the original coordinates and one \code{plane.z} column with the plane elevation for each point.
#' @seealso \code{\link{points2Plane}}
#' @author Claude Flener \email{claude.flener@@utu.fi}
#' @export
#'
# Add elevations based on fitted plane
# plane = output of points2Plane
# data: XYZ data; assumes $Easting, $Northing to be present
#       data read by getGPSdata is in the correct format.
addPlane.z <- function(data, plane){
           data$plane.z <- -plane[1]/plane[3]*data$Easting + -plane[2]/plane[3]*data$Northing + plane[4]/plane[3]
        #cat(dat$surface, "\n")
    return(data)

}

