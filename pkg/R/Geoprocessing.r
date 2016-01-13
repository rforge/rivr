# This file contains all kinds of spatial data processing functions

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


# ************   shape.points2csv   ************
### shape.points2csv converts the coordinates and optionally depth information of a shape file into tab separated values. Outputs a table with X, Y, Z, Depth columns. Depth is optional and not included by default (depth=FALSE). If the shape file has 3D coordinates, the XYZ values will be read from those. If the shape file has ony XY coordinates, the function tries to look for an attribute named "Z" or "UTM_Z" (for compatibility with riv2shape() output data). If depth is enabled (p.ex. for sonar data), the function looks for an attribute named "Depth" or "depth" and includes the output in the CSV file.
### The function can handle one shape file with the extension .shp or,  be default will process all .shp files in the working directory, or, if a directory instead of a file is passed to it, all .shp files in that directory (analoguous to riv.shape())
## Warning: Does not play well with non-ASCII characters in path names

shape.points2csv <- function(path=getwd(), z=NA, depth=FALSE, sep=","){
  print(path)
## if path is a file name, start processing
  if(grepl("*[.]shp$",path)){
  #   pathnames <- basename(path)
  #   path <- dirname(path)
  # } else { # if path is a directory, get all mat files and process them in sequence
  #   shp.files <- list.files(path=path, pattern="*[.]shp$")
  #   pattern <- sprintf("*[.]shp$", version)
  #   pathnames <- list.files(pattern=pattern, path=path, full.names=TRUE)
                        pathname <- path  # required for for loop
                        pathnames <- path # required for for loop
                        path <- dirname(path) # required for writing the file later
        }else # if path is a directory, get all mat files and process them in sequence
                {
                        shp.files <- list.files(path=path, pattern="*[.]shp$")
                         pattern <- sprintf("*[.]shp$", version)
                         pathnames <- list.files(pattern=pattern, path=path, full.names=TRUE)

      cat("Loading all shape files in ", path.expand(path), "...\n", sep="")
  }
  ## read shape files
  for (pathname in pathnames) {
    cat("Reading shape file: ", basename(pathname), "\n", sep="")
    filename <- sub(".shp$|$","",basename(pathname))
    pts <- readOGR(path, filename)
    ## check if Z coordinate is named "Z" or "UTM_Z" or use given variable name for Z
    X <- coordinates(pts)[ , 1]
    Y <- coordinates(pts)[ , 2]
    if(class(z)=="character"){
            Z <- pts[[z]]
    } else
    if(length(coordinates(pts))==3){
      Z <- coordinates(pts)[ , 3]
    } else
    if(length(pts$Z) > 0){
        Z <- pts$Z
      } else
      if(length(pts$UTM_Z) > 0){
        Z <- pts$UTM_Z
      }
    if(depth == TRUE){
       ## check if depth variable is "Depth" or "depth"
      if(length(pts$Depth) > 0){
        dat <- cbind(X, Y, Z, pts$Depth)
      } else
      if(length(pts$depth) > 0){
        dat <- cbind(X, Y, Z, pts$depth)
      } else
      dat <- cat(basename(pathname), "does not contain a depth attribute.\n")
      write.table(dat, paste(path.expand(path), "/", filename, ".csv", sep=""), sep = sep,  row.names=FALSE, col.names=c("X", "Y", "Z", "Depth"))
    } else {
      dat <- cbind(X, Y, Z)
      # str(dat)
      write.table(dat, paste(path.expand(path), "/", filename, ".csv", sep=""), sep = sep,  row.names=FALSE, col.names=c("X", "Y", "Z"))
    }
  }
}

### Usage examples:
## shape.points2csv("/Users/claude/Desktop/Paakkane_venesarkka", depth=TRUE)
## shape.points2csv("/Users/claude/Desktop/Venesarkka_bottom_RTKGPS/venesarkka_bottom02092010.shp")
## shape.points2csv("/Users/claude/GIS/Pulmanki/River_Surveyor_raw_data/2010_syksy/2010_08_26_Paakkane_Pulmanki_discharges", depth=TRUE)
##shape.points2csv("/Users/claude/GIS/Pulmanki/River_Surveyor_raw_data/2010_syksy/Paakkane_venesarkka_2010",  depth=TRUE)
# shape.points2csv(path="/Users/claude/GIS/Pulmanki/River_Surveyor_raw_data/2010_syksy/2010_09_02_Lyytikaine__M9_venesarkka_zigzag/20100902114345.shp", z="Z_bed_adj")



# ************   shape.points2csv.RivSurv   ************
### shape.points2csv converts the coordinates and optionally depth information of a shape file into tab separated values. Outputs a table with X, Y, Z, Depth columns. Depth is optional and not included by default (depth=FALSE). If the shape file has 3D coordinates, the XYZ values will be read from those. If the shape file has ony XY coordinates, the function tries to look for an attribute named "Z" or "UTM_Z" (for compatibility with riv2shape() output data). If depth is enabled (p.ex. for sonar data), the function looks for an attribute named "Depth" or "depth" and includes the output in the CSV file.
### The function can handle one shape file with the extension .shp or,  be default will process all .shp files in the working directory, or, if a directory instead of a file is passed to it, all .shp files in that directory (analoguous to riv.shape())
## Warning: Does not play well with non-ASCII characters in path names

shape.points2csv.RivSurv <- function(path=getwd(), depth=FALSE){
  print(path)
## if path is a file name, start processing
  if(grepl("*[.]shp$",path)){
    pathnames <- basename(path)
    path <- dirname(path)
  } else { # if path is a directory, get all mat files and process them in sequence
    shp.files <- list.files(path=path, pattern="*[.]shp$")
    pattern <- sprintf("*[.]shp$", version)
    pathnames <- list.files(pattern=pattern, path=path, full.names=TRUE)
    cat("Loading all shape files in ", path.expand(path), "...\n", sep="")
  }
  ## read shape files
  for (pathname in pathnames) {
    cat("Reading shape file: ", basename(pathname), "\n", sep="")
    filename <- sub(".shp$|$","",basename(pathname))
    pts <- readOGR(path, filename)
    ## check if Z coordinate is named "Z" or "UTM_Z"
    X <- coordinates(pts)[ , 1]
    Y <- coordinates(pts)[ , 2]
    Z_surf <- pts$UTM_Z_surf
    Z_bed  <- pts$UTM_Z_bed

    if(depth == TRUE){
       ## check if depth variable is "Depth" or "depth"
      if(length(pts$Depth) > 0){
        dat <- cbind(X, Y, Z_surf, Z_bed, pts$Depth)
      } else
      if(length(pts$depth) > 0){
        dat <- cbind(X, Y, Z_surf, Z_bed, pts$depth)
      } else
      dat <- cat(basename(pathname), "does not contain a depth attribute.\n")
      write.table(dat, paste(path.expand(path), "/", filename, ".csv", sep=""), sep = "\t",  row.names=FALSE, col.names=c("X", "Y", "Z_surf", "Z_bed", "Depth"))
    } else {
      dat <- cbind(X, Y, Z_surf)
      # str(dat)
      write.table(dat, paste(path.expand(path), "/", filename, ".csv", sep=""), sep = "\t",  row.names=FALSE, col.names=c("X", "Y", "Z"))
    }
  }
}

### Usage examples:
## shape.points2csv("/Users/claude/Desktop/Paakkane_venesarkka", depth=TRUE)
## shape.points2csv("/Users/claude/Desktop/Venesarkka_bottom_RTKGPS/venesarkka_bottom02092010.shp")
## shape.points2csv("/Users/claude/GIS/Pulmanki/River_Surveyor_raw_data/2010_syksy/2010_08_26_Paakkane_Pulmanki_discharges", depth=TRUE)
##shape.points2csv("/Users/claude/GIS/Pulmanki/River_Surveyor_raw_data/2010_syksy/Paakkane_venesarkka_2010",  depth=TRUE)
# shape.points2csv.RivSurv("/Users/claude/GIS/KAT_Oy_RiverSurveyor", depth=TRUE)






### Randomly divide the data into calibration and validation sets
## pts    = vector points of "SpatialPointsDataFrame" class (could work with any data frame but not tested)
## x      = Number of validation points. Positive integer. If x >= 1,  x points are chosen,  if 0 > x < 1,  the given percentage of pts is used.
## plot   = Boolean. Sets flag whether or not to plot the points

split.random.points <- function(pts, x,  plot=TRUE, verbose=TRUE, ...){
  if (x >= 1){
   valid.size <- x                        # set validation points as absolute number
  } else if (x < 1) {
    valid.size <- round(dim(pts[, 1]) * x) # set validation points as percentage
    valid.size <- valid.size[1]
  }else if (x <= 0){
    cat("splitting a data set by 0 or negative points makes no sense")
    return()
  }
  valid.id <- sample(1:dim(pts@data)[1], valid.size) #random sample
  # str(valid.id)
  Vpts <- pts[valid.id, ]
  ## summary(Apts)
  Cpts <- pts[!is.element(1:dim(pts@data)[1], valid.id), ]
  ## summary(Cpts)
  if(plot==TRUE){
    plot(Cpts, pch=20, col="black")
    points(Vpts,  pch=4, col="red")
    legend("topleft", inset=0.02, legend=c("Calibration points","Verification points"), col=c("black","red"), pch=c(20,4), y.intersp=1.0, bty="n", cex=1)
  }
  if (verbose == TRUE)
  {
  cat("Total number of points: ", dim(pts@data)[1], "\n")
  cat("Number of calibration points: ", dim(Cpts@data)[1], "\n")
  cat("Number of validation points: ", dim(Vpts@data)[1], "\n")
  }

  invisible(list(Cpts=Cpts, Vpts=Vpts))
}
### Usage example:
## X <- split.random.points(vecpts, x=300,  plot=FLASE)
## plot(X$Cpts)
## points(X$Vpts,  col="red")

# shorthand for writing SpatialPoints or SpatialPointsDataFrame to an ESRI
# shape file using writeOGR
# obj = the SpatialPoints object to be written. Required
# file = the desired filename without extension as a text string. Defaults
#        to object name if no name is supplied
# path = the path to the target directory as a string. defaults to current working directory
writeSP2shape<- function(obj, file=deparse(substitute(obj)), path=getwd()){
    # write shape file as 3D points (not supported yet by rgdal)
    writeOGR(obj=obj, dsn=path, layer=file, driver="ESRI Shapefile")
}
# Examples
# writeSP2shape(sonar07fltr, "test")
# writeSP2shape(sonar09fltr)


# returns the name of an object as a character string
objName2string <- function(x){
    name <- deparse(substitute(x))
    return(name)
}

# ************   csv2shape  ************
### csv2shape does exactly what it says on the tin
# x = csv file with coordinates
# returns point shape file
csv2shape <- function(dat, sep="\t"){
        x<- read.table(dat, paste(path.expand(path), "/", filename, ".csv", sep=""), sep = "\t",  row.names=FALSE, col.names=c("X", "Y", "Z"))
# TODO: make coordinate structure
#         write shape file using analog of writeSP2shape with , layer_options="POINTZ" option
#         writeSP2shape()
            # write shape file as 3D points (not supported yet by rgdal)
            # writeOGR(xSP, getwd(), "TanaBru_filtered_merged", driver="ESRI Shapefile", layer_options="POINTZ")
}



# generic shape file to CSV conversion
# takes the name of a shape file as input, assuming that it is located in the current wd, (can be set as dir) and outputs a CSV file with the same name in the same directory (overwriting the previous one , if there is one). Coordinates are included by default as XY columns. If coord is set to FALSE, only the attribute table of the shape file is included.
shape2csv  <- function(shape="", dir=".", sep=",", coord = TRUE){
    pts <- readOGR(dir, shape)
    if (coord ==TRUE){
        pts <- cbind(X=pts@coords[,1], Y=pts@coords[,2], pts@data)
    } else {
        pts <- pts@data
    }

    write.table(pts, paste(shape,".csv",sep=""), sep = sep,  row.names=FALSE, col.names=TRUE)
    invisible(pts)
}




# Merges two SpatialPoints objects, regardless of their attribute tables.
# unlike rbind or spRbind, this function uses merge, and therefore does not
# require the same attributes in both shape files. Missing attributes will
# be filled in according to the method described in merge.
# It is assumed that all data is in the same coordinate system. The
# proj4string is extracted from the first data set input, and is applied
# to the merged data
# x = a spatial.points object, or an array of SpatialPoints objects. If x
#     is an array, all objects in the array will be merged.
# y = a SpatialPoints object to be merged with x, if x is a single
#     SpatialPoints object. y is ignored if x is an array.

mergeSP <-  function(x, y=NA){
    if(is.list(x)){
        if(!is.na(y)) cat("Warning: X is a list. Objects in  X were merged. Varible Y was ignored.\n")
        # use proj4string of first object for output object
        proj <- x[[1]]@proj4string
        # set up data and coords of first object before loop
        data <- x[[1]]@data
        coords <- x[[1]]@coords
        for(i in 1:length(x)-1){
            data <- merge(data, x[[i+1]]@data , all=TRUE, sort=FALSE)
            coords <- merge(coords, x[[i+1]]@coords , all=TRUE, sort=FALSE)
        }
        mergedSP <- SpatialPointsDataFrame(coords, data, proj4string=proj)
    } else {
        # if(!is.na(y)){
            data <- merge(x@data, y@data , all=TRUE, sort=FALSE)
            coords <- merge(x@coords, y@coords , all=TRUE, sort=FALSE)
            proj <- x@proj4string
            mergedSP <- SpatialPointsDataFrame(coords, data, proj4string=proj)
        # }
    }
    return(mergedSP)
}
# Examples:
# my <- mergeSP(sonar07, sonar09)
# plot(sonar07)
# plot(sonar09)
# plot(my)
# writeSP2shape(mx)

# dat <- c(sonar07, sonar09, shore)
# MM <- mergeSP(dat)
# plot(MM)
# str(dat[[1]]@data)


# ************   riv2shape  ************
#########################################
#### Deprecated, use riv2spatial instead
#########################################
# riv2shape processes MAT v5 export data from River Surveyor Live software into point data and saves it as shape files, including XYZ coordinates in UTM, GPS quality data (4=RTK, 2=Differential, 1 = Standard, 0 = Invalid), UTC, VB_depth (m), BT_depth (m), total_Q and depth in attribute data.
# the function reads either all the MAT files in the current working directory (default) or any specified directory or a specified file and saves the shape files in the same directory as the original file.
# optional verbose and interactive modes are provided
# plotting the shape files (default) can be disabled if it slows the process down in the case of very large files.
## Warning: Does not play well with non-ASCII characters in path names
#Requires: R.matlab, Rcompression, sp, rgdal
#
##  GPS structure
#               Parameter Name  Description
#       1       Longitude_deg           Longitude for each sample
#       2       Latitude_deg            Latitude for each sample
#       3       Utc                     Universal Time Coordinate (UTC) for each sample
#       4       Satellites              Number of satellites in view for each sample
#       5       HDOP                    Horizontal Dilution of Precision for each sample
#       6       GPS_Age                 GPS Age for each sample
#       7       Altitude_m              Altitude of GPS receiver above proprietary ellipsoid
#                                       needs GeoidHeight parameter to calculate GRS80 altitude
#       8       GPS_Quality             GPS Quality for each sample (4=RTK, 2=Differential, 1 = Standard, 0 = Invalid)
#       9       UTM_m                   Universal Transverse Mercator (UTM) coordinate system- X and Y coordinates for each sample
#
#  Raw11GPSData structure
#               Parameter Name  Description
#       1       VtgTimeStamp            $GPVTG time stamp for each high frequency GPS sample
#       2       VtgTmgTrue              $GPVTG true course made good for each high frequency GPS sam- ple (degrees)
#       3       VtgTmgMag               $GPVTG magnetic course made good for each high frequency GPS sample (degrees)
#       4       VtgSogMPS               $GPVTG speed over ground for each high frequency GPS sample (m/s)
#       5       VtgMode                 $GPVTG active mode for each high frequency GPS sample
#       6       GgaTimeStamp            $GPGGA time stamp for each high frequency GPS sample
#       7       GgaLatitude             $GPGGA latitude for each high frequency GPS sample
#       8       GgaLongitude            $GPGGA longitude for each high frequency GPS sample
#       9       GgaQuality              $GPGGA fix quality for each high frequency GPS sample (4=RTK, 2=Differential, 1 = Standard, 0 = Invalid)
#       10      GgaAltitude             $GPGGA altitude above sea level for each high frequency GPS sam- ple (m)
#       11      GgaUTC                  $GPGGA UTC for each high frequency GPS sample
#
        #   GeoidHeight values determined by Sontek
        # File Name                                     Geoid Height (in m)
        # Joensuu_KAT_20101021111607                    18.60
        # Karasjohka_20090901144148                     23.90
        # Kemijoki_20100519212753                       21.99
        # Kokemaenjoki_20101109112638                   21.48
        # Lapinlahti_20101007130657                     19.99
        # Pulmankijoki_Venesarkka_20100827185952        21.74
        # Teno_Alakongas_20100523123214                 21.74
        # Teno_Nuorgam_20100528152351                   21.70
        # Teno_Seida_up_20100528151644                  21.70
        # Teno_upstream_of_Utsjoki_20100526112610       21.74
        # Utsjoki_20100828153757                                22.63
        #
        # Pulmanki_Siltasarkka_up_20100526195825        21.74
        # Teno_Duskanguoika_20100527152936              22.34
        # Teno_Nuorgam_Alakongas_Down_20100528173717    22.02
        # Teno_Nuorgam_Koulu_20100528141901             21.96
        # Teno_Nuorgam_raja_20100528163254              21.89
        # Teno_Utsjoki_Muinaiskalmisto_20100525114020   22.67
        # Teno_Vetsikko_20100525125841                  22.43
        # Utsjoki_Bridge_20100828140005                 22.62
#########################################
#### Deprecated, use riv2spatial instead
#########################################
riv2shape <- function(path=getwd(), sensorDepth=0.06, geoidHeight=0, ellipsDiff=0, verbose=FALSE, interact=FALSE, plot=TRUE){
    # sensorDepth = depth of sensor below water surface (default is 6 cm in the kase of Paakkane mounted sensor)
    # geoidHeight = difference between GPS ellipsoid and GRS80 ellipsoid at locale (take form GGA string)
    # ellipsDiff  = difference between GRS80 ellipsoid and desired ellipsoid (e.g. N2000) at locale

        # list of serial numbers for RiverSurveyor devices
        # s/n of RS-M9 = 184 and RS-S5 = 1132
        RSM9 <- c(184, 2097, 4414)
        RSS5 <- c(1132)

        print(path) ########
# TODO fix bug that function gives error if path to single file is given that is outside current wd. works for single file if foo.mat located in current wd, but if path/to/foo.mat is passed, function gives error :No sucgh file or directory. Probably to do with for loop in line 305
        # if path is a file name, start processing
        if(grepl("*[.]mat$",path)){
                        # pathnames <- basename(path)
                        pathname <- path  # required for for loop
                        pathnames <- path # required for for loop
                        path <- dirname(path) # required for writing the file later
        }else # if path is a directory, get all mat files and process them in sequence
                {
                        mat.files <- list.files(path=path, pattern="*[.]mat$")
                         pattern <- sprintf("*[.]mat$", version)
                         pathnames <- list.files(pattern=pattern, path=path, full.names=TRUE)
                                #  filenamesfull <- list.files(pattern=pattern)
                                 # extract file names without extensions
                                # filenames <- sub(".mat$|$","",filenamesfull)
                                cat("Loading all River Surveyor MAT files in ", path.expand(path), "...\n", sep="")
                }
        # read MAT files
  for (pathname in pathnames) {
    cat("Reading MAT file: ", basename(pathname), "\n", sep="")
    filename <- sub(".mat$|$","",basename(pathname))
    riv <- readMat(pathname)
    # optional interactive mode to interrupt processing before continuing to next file
    if (interact==TRUE){
        if (interactive()) {
                cat("Press ENTER to view data:")
                readline()
        }
        print(riv)
    }

    # check that the MAT file is a river surveyor file, by checking that the serial number included in the file is one of the known river surveyor devices. (did not find a better way, i.e. a reference to the river surveyor live software used to export the data into MAT)
    RSmodel <- as.numeric(riv$System[1])
    if (RSmodel %in% c(RSM9, RSS5)){
        cat("Creating shape file: ", filename, ".shp\n", sep="")
        #process data into shape file:
# add proper names to sub-dataframes
names(riv$GPS) <- dimnames( riv$GPS)[[1]]
names(riv$Setup) <- dimnames( riv$Setup)[[1]]
names(riv$Summary) <- dimnames( riv$Summary)[[1]]
names(riv$BottomTrack) <- dimnames( riv$BottomTrack)[[1]]
       UTM_X                   <- unlist(riv$GPS$UTM[,1]) # UTM X
       UTM_Y                   <- unlist(riv$GPS$UTM[,2]) # UTM X
       GPS_altitude            <- unlist(riv$GPS$Altitude) # 7 Altitude_m
       GPS_Qual                <- unlist(riv$GPS$GPS.Quality) # 8 GPS_Quality
       UTC                     <- unlist(riv$GPS$Utc) # 3 UTC
       VB_Depth                <- unlist(riv$BottomTrack$VB.Depth) # VB_Depth_m
       BT_Depth                <- unlist(riv$BottomTrack$BT.Depth) # BT_Depth_m
       totalQ                  <- unlist(riv$Summary$Total.Q) # total.Q
       depth                   <- unlist(riv$Summary$Depth) # Depth
       sensorDepthSet          <- unlist(riv$Setup$sensorDepth[1])
       sensorDepthReal         <- sensorDepth

###        # unlist all variables to create numeric data for OGR driver
###        # UTM_X                   <- unlist(riv$GPS[[10]][,1]) # UTM X
###        # UTM_Y                   <- unlist(riv$GPS[[10]][,2]) # UTM X
###        # Changed in RSL version 3.0.3:
###        UTM_X                   <- unlist(riv$GPS[[11]][,1]) # UTM Y
###        UTM_Y                   <- unlist(riv$GPS[[11]][,2]) # UTM Y
###        # lat                     <- unlist(riv$GPS[[9]][,2]) # UTM Y
###        GPS_altitude            <- unlist(riv$GPS[7]) # 7 Altitude_m
###        GPS_Qual                <- unlist(riv$GPS[8]) # 8 GPS_Quality
###        UTC                     <- unlist(riv$GPS[3]) # 3 UTC
###        VB_Depth                <- unlist(riv$BottomTrack[1]) # VB_Depth_m
###        BT_Depth                <- unlist(riv$BottomTrack[2]) # BT_Depth_m
###        totalQ                  <- unlist(riv$Summary[[6]]) # total.Q
###        depth                   <- unlist(riv$Summary[[7]]) # Depth
###        sensorDepthSet          <- unlist(riv$Setup[[9]][1])
###        sensorDepthReal         <- sensorDepth

        # adjust vertical offset
        if (RSmodel %in% RSM9){
                RS.model="M9"
                sensor2GPSlength <- 0.275 + 0.2523 + 0.035
                # M9 length = 25.23 cm, Top of sensor unit to bottom of GPS is 27.5 cm, bottom of GPS receiver to black line where GPS measurements are taken = 3.5 cm
        } else
        if (RSmodel %in% RSS5){
                RS.model="S5"
                sensor2GPSlength <- 0.275 + 0.2536 + 0.035
                # M9 length = 25.23 cm, Top of sensor unit to bottom of GPS is 27.5 cm, bottom of GPS receiver to black line where GPS measurements are taken = 3.5 cm
        }

        ## adjust for erroneous SensorDepth setting
        if(sensorDepthSet != sensorDepthReal){
                WaterDepth <- depth - sensorDepthSet + sensorDepthReal
        }else
                {WaterDepth <- depth}

        ## Calculate water level elevation from GPS data
        UTM_Z_surf <- GPS_altitude + geoidHeight + ellipsDiff - sensor2GPSlength + sensorDepthReal
        ## calculate river bed elevation
        UTM_Z_bed <- UTM_Z_surf - WaterDepth


            rivGPS <- data.frame(UTM_X, UTM_Y, UTM_Z_surf, UTM_Z_bed, GPS_Qual, UTC, VB_Depth, BT_Depth, totalQ, WaterDepth)
            names(rivGPS) <- c("UTM_X", "UTM_Y", "UTM_Z_surf", "UTM_Z_bed", "GPS_qual", "UTC", "VB_Depth_m", "BT_Depth_m", "total_Q", "depth")# NB! make sure the names are max 10 chars, else OGR driver will start bitching
            summary(rivGPS)
            coord <- SpatialPoints(data.frame(UTM_X, UTM_Y, UTM_Z_bed)) # create coordinates
            rivSP <- SpatialPointsDataFrame(data=rivGPS, coords=coord)
            if(verbose==TRUE){
                    print(summary(rivSP))
            }
            if(plot==TRUE){
                    plot(rivSP)
            }
            # write shape file as 3D points (not supported yet by rgdal)
            writeOGR(rivSP, path, filename, driver="ESRI Shapefile", layer_options="POINTZ")
        }else
        cat("The MAT file does not appear to be a RiverSurveyor file\n")
    }
  return(paste("Done!"))
}

### Usage examples:
##
### Process all mat files in current working directory:
##              setwd("/Users/claude/Desktop/riv_mat_test")
##              riv2shape()
##
### Process all mat files in a given directory:
##              riv2shape("/Users/claude/Desktop/riv_mat_test/")
##
### convert one specific file:
##              riv2shape("20100828153055.mat")
##              riv2shape("/Users/claude/Desktop/riv_mat_test/20100828153055.mat")


adjustRivSurv.GPSwaterSurface.vOffset <- function( elevation, depth, RS.model="M9", sensorDepthSet, sensorDepthReal=0.06){
        # adjust vertical offset
# NB! this does not include 3.5 cm from the bottom of GPS receiver to black line where GPS measurements are taken for the Hemisphere antenna of the RiverSurveyor DGPS.
        if (RS.model=="M9"){
                sensor2GPSlength <- 0.275 + 0.2523
                # M9 length = 25.23 cm, Top of sensor unit to bottom of GPS is 27.5 cm
            } else
        if (RSmodel=="S5"){
                sensor2GPSlength <- 0.275 + 0.2536
                # S5 length = 25.36 cm, Top of sensor unit to bottom of GPS is 27.5 cm
        }

        ## adjust for erroneous SensorDepth setting
        if(sensorDepthSet != sensorDepthReal){
                WaterDepth <- depth - sensorDepthSet + sensorDepthReal
        }else
                {WaterDepth <- depth}

        SurfaceElevation <- elevation - sensor2GPSlength + sensorDepthReal

        ## Calculate water level elevation from GPS data
        UTM_Z_surf <- GPS_altitude + geoidHeight + ellipsDiff - sensor2GPSlength + sensorDepthReal
        ## calculate river bed elevation
        UTM_Z_bed <- UTM_Z_surf - WaterDepth

    dat <- data.frame(elevation= SurfaceElevation, depth=WaterDepth)
    return(dat)
}


#RiverSurveyorSerialNumbers:
        RSM9 <- c(184, 2097)
        RSS5 <- c(1132)




# reads in matlab file created by RiverSurveyor Live and extracts spatial
# data and exports as either CSV, ESRI shape, or both (default)
# automatically handles batch processing if path is a directory
# out   : can take either of these values "csv", "shape", "all", "none"
        # if out == "none", no output is created but the spatialPointsDataFrame 
        # is returned on exit; all other options produce output but return empty.
riv2spatial <- function(path=getwd(), out="all", sensorDepth=0.06, geoidHeight=0, ellipsDiff=0, verbose=FALSE, interact=FALSE, plot=TRUE){
    # sensorDepth = depth of sensor below water surface (default is 6 cm in the kase of Paakkane mounted sensor)
    # geoidHeight = difference between GPS ellipsoid and GRS80 ellipsoid at locale (take form GGA string)
    # ellipsDiff  = difference between GRS80 ellipsoid and desired ellipsoid (e.g. N2000) at locale

        # list of serial numbers for RiverSurveyor devices
        # s/n of RS-M9 = 184 and RS-S5 = 1132
        RSM9 <- c(184, 2097, 4414)
        RSS5 <- c(1132)
    data <- batchProcess2List(FUN=readMat, ext="mat", path=path)
    for(i in 1:length(data)){
        riv <- data[[i]]
        filename <- names(data)[i]

        # check that the MAT file is a river surveyor file, by checking that the serial number included in the file is one of the known river surveyor devices. (did not find a better way, i.e. a reference to the river surveyor live software used to export the data into MAT)
        RSmodel <- as.numeric(riv$System[1])
        if (RSmodel %in% c(RSM9, RSS5)){
            cat("Creating spatial file: ", filename, "\n", sep="")
            #process data into shape file:
    # add proper names to sub-dataframes
    names(riv$GPS) <- dimnames( riv$GPS)[[1]]
    names(riv$Setup) <- dimnames( riv$Setup)[[1]]
    names(riv$Summary) <- dimnames( riv$Summary)[[1]]
    names(riv$BottomTrack) <- dimnames( riv$BottomTrack)[[1]]
           UTM_X                   <- unlist(riv$GPS$UTM[,1]) # UTM X
           UTM_Y                   <- unlist(riv$GPS$UTM[,2]) # UTM X
           GPS_altitude            <- unlist(riv$GPS$Altitude) # 7 Altitude_m
           GPS_Qual                <- unlist(riv$GPS$GPS.Quality) # 8 GPS_Quality
           UTC                     <- unlist(riv$GPS$Utc) # 3 UTC
           VB_Depth                <- unlist(riv$BottomTrack$VB.Depth) # VB_Depth_m
           BT_Depth                <- unlist(riv$BottomTrack$BT.Depth) # BT_Depth_m
           totalQ                  <- unlist(riv$Summary$Total.Q) # total.Q
           depth                   <- unlist(riv$Summary$Depth) # Depth
           sensorDepthSet          <- unlist(riv$Setup$sensorDepth[1])
           sensorDepthReal         <- sensorDepth

            # adjust vertical offset
            if (RSmodel %in% RSM9){
                    RS.model="M9"
                    sensor2GPSlength <- 0.275 + 0.2523 + 0.035
                    # M9 length = 25.23 cm, Top of sensor unit to bottom of GPS is 27.5 cm, bottom of GPS receiver to black line where GPS measurements are taken = 3.5 cm
            } else
            if (RSmodel %in% RSS5){
                    RS.model="S5"
                    sensor2GPSlength <- 0.275 + 0.2536 + 0.035
                    # M9 length = 25.36 cm, Top of sensor unit to bottom of GPS is 27.5 cm, bottom of GPS receiver to black line where GPS measurements are taken = 3.5 cm
            }

            ## adjust for erroneous SensorDepth setting
            if(sensorDepthSet != sensorDepthReal){
                    WaterDepth <- depth - sensorDepthSet + sensorDepthReal
            }else
                    {WaterDepth <- depth}

            ## Calculate water level elevation from GPS data
            UTM_Z_surf <- GPS_altitude + geoidHeight + ellipsDiff - sensor2GPSlength + sensorDepthReal
            ## calculate river bed elevation
            UTM_Z_bed <- UTM_Z_surf - WaterDepth


            rivGPS <- data.frame(UTM_X, UTM_Y, UTM_Z_surf, UTM_Z_bed, GPS_Qual, UTC, VB_Depth, BT_Depth, totalQ, WaterDepth)
            names(rivGPS) <- c("UTM_X", "UTM_Y", "UTM_Z_surf", "UTM_Z_bed", "GPS_qual", "UTC", "VB_Depth_m", "BT_Depth_m", "total_Q", "depth")# NB! make sure the names are max 10 chars, else OGR driver will start bitching
            if (out == "csv" || out== "all"){
                write.table(rivGPS, paste( path,"/", filename, ".csv", sep="" ), sep = ",",  row.names=FALSE, col.names=TRUE)
            }
            summary(rivGPS)
            coord <- SpatialPoints(data.frame(UTM_X, UTM_Y, UTM_Z_bed)) # create coordinates
            rivSP <- SpatialPointsDataFrame(data=rivGPS, coords=coord)
            if(verbose==TRUE){
                    print(summary(rivSP))
            }
            if(plot==TRUE){
                    plot(rivSP)
            }
            if (out == "shape" || out== "all"){
                # write shape file as 3D points (not supported yet by rgdal)
                writeOGR(rivSP, path, filename, driver="ESRI Shapefile", layer_options="POINTZ")
            }
        }else
        cat("The MAT file does not appear to be a RiverSurveyor file\n")
    }
  ifelse(out=="none", return(rivSP), return())
}

#x <- riv2spatial(path="/Users/claude/GIS/Pulmanki/River_Surveyor_raw_data/2013_kevat/2013_05_18")







#riverSurveyorCheckVerticalOffsetu <- function(sensorDepth, )


writeSpatialPoints <- function(x, out="all", outdir="." ,outfile=NULL, sep="," ){
            if (out == "shape" || out== "all"){
                writeOGR(x, outdir ,outfile , driver="ESRI Shapefile", layer_options="POINTZ")
            }
        # Write CSV file
            if (out == "csv" || out== "all"){
            write.table(x@data, paste( outdir, "/", outfile, ".csv", sep="" ), sep = sep,  row.names=FALSE, col.names=TRUE)
            }
    return()
}
