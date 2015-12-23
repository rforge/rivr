
    #Copyright (C) 2014 Claude Flener, University of Turku.

    #Licensed under the Apache License, Version 2.0 (the "License");
    #you may not use this file except in compliance with the License.
    #You may obtain a copy of the License at

        #http://www.apache.org/licenses/LICENSE-2.0

    #Unless required by applicable law or agreed to in writing, software
    #distributed under the License is distributed on an "AS IS" BASIS,
    #WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    #See the License for the specific language governing permissions and
    #limitations under the License.

# From Anttoni I got GPS time and UTC time in seconds, which is pretty straightforward and usable:
# GPS time: 552495.00
# UTC time: 552480.00
# The only thing here that I find a bit odd is the fact that GPS time is 15 seconds ahead of UTC and not the other way round.

# From the RiverSurveyor I get the following:
# UTC - Universal Time Coordinated (UTC) for each sample, in HHMMSS.S
# 92823.9
# GgaUTC - $GPGGA UTC for each high frequency GPS sample (HHMMSS.S)
# 92823
# GgaTimeStamp - $GPGGA time stamp for each high frequency GPS sample
# 368974147.815823
# VtgTimeStamp - $GPVTG time stamp for each high frequency GPS sample
# 368974147.825757

# GPS time = the number of seconds since Jan 06, 1980



#######################################################################

# Get the GPS time in seconds since midnight, removing the seconds of the
# previous days of the week.
            GPSdaySec <- function(time){
                daySec <- time %% 86400 # number of seconds in a day
                return(daySec)
            }
# GPSdaySec(552480.00)
# x <- GPSdaySec(552480.00)
# GPSdaySec(gga)

# Convert GPS time to UTC, accounting for leap seconds
# This is a primitive function and it is only valid for measurements after 01.01.2008 until the next leap scond is inserted in the future
GPS2UTC <- function(time, leap=15){
    utc <- time - leap #needs a way to update the number of leap seconds automatically
    return(utc)
}

# turn HHMMSS.SS time into day seconds (seconds from midnight)
# can take decimal seconds
hhmmss2daySec  <- function(hhmmss){
    # hhmmss <- 92823.9

    hh <- trunc(hhmmss/10000)
    mm <- trunc( (hhmmss - hh*10000) /100 )
    ss <- round( hhmmss - hh * 10000 - mm * 100, 2 )

    # hms <- as.POSIXct(paste(hh,":",mm,":",ss, sep=""), format="%H:%M:%S", origin="1970-01-01", tz="GMT")
    # as.numeric(hms)

    daySeconds <- hh * 3600 + mm * 60 + ss
    return(daySeconds)
}
# daysec <- hhmmss2daySec(92823.9)
# hhmmss2daySec(105431)
# h <- (10*60*60)+(54*60)+31
# h <- hhmmss2daySec(112037)
# ms <- 86400+86400+h
# d <- hhmmss2daySec(94307.7)
# d <- hhmmss2daySec(94323.7)

# converting Day seconds to HHMMSS
daySec2hhmmss <- function(secs){
    # secs <- GPSdaySec(gga)
    # secs <- daySeconds
    HH <- trunc(secs/3600)
    MM <- trunc((secs %% 3600) / 60)
    SS <- round(secs - HH * 3600 - MM * 60, 2)

    # HMS <- as.POSIXct(paste(HH,":",MM,":",SS, sep=""), format="%H:%M:%S", origin="1970-01-01", tz="GMT")
    # as.numeric(HMS)
    # hhmmss <- paste(HH,MM,SS, sep="")
    hhmmss <- HH *10000 + MM * 100 + SS
    return(hhmmss)
}
# daySec2hhmmss(y)
# daySec2hhmmss(x)


# Human readable time 	Seconds
# 1 hour        	3600 seconds
# 1 day	                86400 seconds
# 1 weekdays.Date	604800 seconds
# 1 month (30.44 days) 	2629743 seconds
# 1 year (365.24 days) 	31556926 seconds

# Convert YYYYMMDD to GPS seconds (seconds since Jan 6th 1980)
# DOES NOT WORK AS INTENDED !
yyyymmdd2gpsSecs <- function(yyyymmdd){

    yyyy <- trunc(yyyymmdd/10000)
    mm <- trunc( (yyyymmdd- yyyy*10000) /100 )
    dd <- trunc( yyyymmdd- yyyy * 10000 - mm * 100)

    secs <- (yyyy - 1980) * 31556926 + mm * 2629743 + (dd - 6) * 86400
    # ys <- yyyy-1980 *
    # return(paste(yyyy,"-",mm,"-",dd, sep=""))
    return(secs)
}

YMDhmss2date <- function(filename){
    yyyymmdd <- filename / 1000000
    yyyy <- trunc(yyyymmdd/10000)
    mm <- trunc( (yyyymmdd- yyyy*10000) /100 )
    dd <- trunc( yyyymmdd- yyyy * 10000 - mm * 100)
    date <- list(yyyy=yyyy, mm= mm, dd= dd)
    return(date)
}
#date <- YMDhmss2date(20110620231004)
# filename <- 20110620231004

# takes a date as a list (output of YMDhmss2date) and daysecs and returns monthsecs
# this is intended to avoid duplicate timestamps on consecutive days
getMonthSecs <- function(date, daysecs){
    monthSecs <- (date$dd -1) * 86400 # the last day is filled with daysecs
    monthSecs <- monthSecs + daysecs
    return(monthSecs)
}
# getMonthSecs(date,daysec)

########### ------------------------------------------
#' getGPSdata 
#'
#' This function reads GNSS data with a GPS-time stamp from a text file and add month-seconds in UTC format. Depending on the time format used in the raw data, the time format is converted to UTC. Month-seconds are required for combining GNSS data with RiverSurveyor data.
#'
#' @param data a \code{string} containing the path to a text file containing GNSS coordinates and a time stamp.
#' @note \code{data} should contain at least XYZ coordinates and a timestamp. Generally, these are GNSS coordinates from an external GNSS source. The best input data is a *.csv file with post-processed trajectories exported from Trimble Business Center using UTC time stamps in \code{local} format.
#' @param GPStimeFormat a \code{string} indicating the timestamp format. The following strings are accepted: \cr "GPSsecs" if the timestamp is in raw GPS seconds, \cr "UTCsecs" if the time stamp is in UTC seconds, \cr "UTChhmmss" if the time stamp format is in UTC hours-minutes-seconds format (default).
#' @note this function is primarily intended to import data exported form Trimble Business center as processed baseline trajectories with the times stamp in the form of date (DD.MM.YYYY) followed by time in UTC hhmmss.sss format. For instance: \code{19.11.2010,090720.500}
#' @param tab.sep a \code{string} with the tab separator in the text file. Defaults to ",".
#' @param GPS.UTC.leapSeconds Optional: an \code{integer numeral} specifying the number of leap seconds between GPS-time and UTC. This is only required if \code{GPStimeFormat == "GPSsecs"}.
#' @note GPS leap seconds are introduced every now and then in order to align GPS time with UTC. It is not possible to include future leap seconds in the code. Therefore, any leap seconds need to be input by the user for the time period in question.
#' @return a \code{data.Frame} containing the original coordinates and a \code{date} and a \code{UTC} column with the time stamp as month-seconds.
#' @seealso \code{\link{points2Plane}}
#' @author Claude Flener \email{claude.flener@@utu.fi}
#' @export
#'
# Convert GPS data to UTC day seconds:
getGPSdata <- function(data, GPStimeFormat ="UTChhmmss", tab.sep=",", GPS.UTC.leapSeconds =NULL  ){
    # cat(GPStimeFormat)
    data <- read.table(data, sep=tab.sep, header=TRUE)
        # d <- GPS$Da
            names(data)[ names(data) == "easting"]<-"Easting"
            names(data)[ names(data) == "northing"]<-"Northing"
            names(data)[ names(data) == "elevation"]<-"Elevation"
            names(data)[ names(data) == "utc"]<-"UTC"
        d <- data$date
        p <- as.POSIXct(d, format= "%d.%m.%Y" ) # format of Trimble export
        GPSday <- as.numeric( format(p, format="%d") )
    cat("GPSday: ", str(GPSday), "\n")
    cat("GPSdata: ", str(data), "\n")
    if (GPStimeFormat == "GPSsecs"){
        utc <- GPS2UTC(data$StartGPSseconds, GPS.UTC.leapSeconds)
        data$daySec <- GPSdaySec(utc)
        data$monthSec <- (GPSday -1) * 86400 + data$daySec
    }else
    if (GPStimeFormat == "UTCsecs"){
        data$daySec <- GPSdaySec(data$UTCtime)
        data$monthSec <- (GPSday -1) * 86400 + data$daySec
    }else
    if (GPStimeFormat == "UTChhmmss"){
        data$daySec <- hhmmss2daySec(data$UTC)
        data$monthSec <- (GPSday -1) * 86400 + data$daySec
    }else
    cat("No valid GPStimeFormat specified.\n")
    return(data)
}
########### ------------------------------------------
# May be necessary to add an option to truncate the floats to integers for
# the merge operation to work at all. May be better to round to the
# nearest possible decimal of the other data set, if RiverSurveyor has
# decimals.


########### ------------------------------------------
# read all mat files and merge the relevant data into a dataframe
# timestamp takes either "UTC" or "GgaUTC"
# Now includes sensor depth set at measurement time for possible error
# correction
# Includes TrueCourseMadeGood (VtgTmgTrue) for possible data filtering

getRSurvDepthTstamp <- function(path=getwd(), timeStamp="UTC", includeRivGPSxy=FALSE){
    print(path) ########
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
    rivDat <- data.frame(daySec = double(0), depth = double(0))
    # read MAT files
    for (pathname in pathnames) {
    cat("Reading MAT file: ", basename(pathname), "\n", sep="")
    filename <- sub(".mat$|$","",basename(pathname))
    # cat("filename: ", filename, "\n")  #####
    rivMAT <- readMat(pathname)
names(rivMAT$GPS) <- dimnames( rivMAT$GPS)[[1]]
names(rivMAT$RawGPSData) <- dimnames( rivMAT$RawGPSData)[[1]]
names(rivMAT$Setup) <- dimnames( rivMAT$Setup)[[1]]
names(rivMAT$Summary) <- dimnames( rivMAT$Summary)[[1]]
names(rivMAT$BottomTrack) <- dimnames( rivMAT$BottomTrack)[[1]]
# Mat file contains two $System structures
# Rename first one to Sys
rivMAT$Sys <- rivMAT$System
rivMAT$System <- NULL
names(rivMAT$System) <- dimnames( rivMAT$System)[[1]]
    # Select UTC time stamp to use:
    if(timeStamp == "UTC"){
       UTC <- unlist(rivMAT$GPS$Utc) # 3 UTC
        } else
    if(timeStamp == "GgaUTC"){
        UTC <- unlist( rivMAT$RawGPSData$GgaUTC [,1] )           # GgaUTC
    }
 #
    # rivDaySec <- hhmmss2daySec(trunc( UTC) )
    rivDaySec <- hhmmss2daySec( UTC )
    rivDate <- YMDhmss2date(as.numeric( filename ))
    rivMonthSec <- getMonthSecs(rivDate, rivDaySec)
    rivMonthSecRounded <- round(rivMonthSec)
#
    depth <- as.numeric( rivMAT$Summary$Depth )
    Top.Q <- as.numeric( rivMAT$Summary$Top.Q )
    Middle.Q <- as.numeric( rivMAT$Summary$Middle.Q )
    Bottom.Q <- as.numeric( rivMAT$Summary$Bottom.Q )
    Left.Q <- as.numeric( rivMAT$Summary$Left.Q )
    Right.Q <- as.numeric( rivMAT$Summary$Right.Q )
    Total.Q <- as.numeric( rivMAT$Summary$Total.Q )
    Cells <- as.numeric( rivMAT$Summary$Cells )
        #x <- Mean.Vel.1 <- as.numeric( rivMAT$Summary$Mean.Vel[,1] )
        #y <- Mean.Vel.2 <- as.numeric( rivMAT$Summary$Mean.Vel[,2] )
    Boat.Vel.1 <- as.numeric( rivMAT$Summary$Boat.Vel[,1] )
    Boat.Vel.2 <- as.numeric( rivMAT$Summary$Boat.Vel[,2] )
    Boat.Vel.3 <- as.numeric( rivMAT$Summary$Boat.Vel[,3] )
    Boat.Vel.4 <- as.numeric( rivMAT$Summary$Boat.Vel[,4] )
    Track.Reference <- as.numeric( rivMAT$Summary$Track.Reference )

    Temperature <- as.numeric( rivMAT$System$Temperature )
    Sample <- as.numeric( rivMAT$System$Sample )
    SysTime <- as.numeric( rivMAT$System$Time )

    sensorDepthSet          <- unlist(rivMAT$Setup$sensorDepth[1])
    # sum all columns of VtgTmgTrue (first 10 cols contain data, the rest
    # is 0, because sensor samples 10Hz of 50Hz data (cf. RiverSurveyor
    # Manual: Matlab data Appendix)
    VtgTmStamp             <- rowSums(rivMAT$RawGPSData$VtgTimeStamp)
    VtgTmgMPS             <- rowSums(rivMAT$RawGPSData$VtgTmgTrue)
    VtgSogMPS             <- rowSums(rivMAT$RawGPSData$VtgSogMPS)

    VBdepth <- as.numeric( rivMAT$BottomTrack$VB.Depth )
    BTdepth <- as.numeric( rivMAT$BottomTrack$BT.Depth )
        BTvel <- unlist( rivMAT$BottomTrack$BT.Vel)
        BTbeamDepth1 <- unlist( rivMAT$BottomTrack$BT.Beam.Depth[,1] )
        BTbeamDepth2 <- unlist( rivMAT$BottomTrack$BT.Beam.Depth[,2] )
        BTbeamDepth3 <- unlist( rivMAT$BottomTrack$BT.Beam.Depth[,3] )
        BTbeamDepth4 <- unlist( rivMAT$BottomTrack$BT.Beam.Depth[,4] )
        BTbeamDepthMean <- rowMeans(unlist( rivMAT$BottomTrack$BT.Beam.Depth ))
    BTfreq <- as.numeric( rivMAT$BottomTrack$BT.Frequency )

    gpsUTC <- unlist(rivMAT$GPS$Utc)
    rawGgaUTC <- unlist( rivMAT$RawGPSData$GgaUTC [,1] )

    Mean.Vel.x <- as.numeric( riv$Summary$Mean.Vel[,1] )
    Mean.Vel.y <- as.numeric( riv$Summary$Mean.Vel[,2] )

    Mean.Vel.magnitude <- sqrt(Mean.Vel.x^2 + Mean.Vel.y^2)
    Mean.vel.dir.north <- 180/pi * atan2(-Mean.Vel.x,-Mean.Vel.y)+180

        #Mean.Vel.magnitude <- sqrt(x^2 + y^2)
        ##Mean.vel.dir.polar <- atan(y/x)
        #Mean.vel.dir.north <- 180/pi * atan2(-x,-y)+180


##convert Cartesian coordinates (X,Y) to 0 to 360 degrees
##
##NOTE! ATAN2() as used herein is the standard definition ( http://en.wikipedia.org/wiki/Atan2#Definition ). Some software, notably Microsoft Excel, reverses the order of the arguments.
##counterclockwise from +X axis: (180/pi)*ATAN2(-Y,-X) + 180
##counterclockwise from -X axis: (180/pi)*ATAN2( Y, X) + 180
##counterclockwise from +Y axis: (180/pi)*ATAN2( X,-Y) + 180
##counterclockwise from -Y axis: (180/pi)*ATAN2(-X, Y) + 180
##clockwise from +X axis:        (180/pi)*ATAN2( Y,-X) + 180
##clockwise from -X axis:        (180/pi)*ATAN2(-Y, X) + 180
##clockwise from +Y axis:        (180/pi)*ATAN2(-X,-Y) + 180  # 0-360 from North
##clockwise from -Y axis:        (180/pi)*ATAN2( X, Y) + 180

    # dat <- data.frame(rivUTC= UTC, daySec= rivDaySec, monthSec=rivMonthSec, depth=depth, snsDpthSet=sensorDepthSet)
    if(includeRivGPSxy==FALSE){
        dat <- data.frame(rivUTC= UTC, daySec= rivDaySec, monthSec=rivMonthSec, MSrounded=rivMonthSecRounded, depth=depth, snsDpthSet=sensorDepthSet, VtgTmStp= VtgTmStamp, VtgTMG=VtgTmgMPS, VtgSOG= VtgSogMPS,  VBdepth=VBdepth, BTdepth=BTdepth, BTbmDpth1=BTbeamDepth1, BTbmDpth2=BTbeamDepth2, BTbmDpth3=BTbeamDepth3, BTbmDpth4=BTbeamDepth4, BTbmDpthM=BTbeamDepthMean, BTfreq=BTfreq, SysTime=SysTime, Sample=Sample , rivGpsUTC=gpsUTC, rawGgaUTC=rawGgaUTC, Temp=Temperature, TopQ=Top.Q, MiddleQ=Middle.Q, BottomQ=Bottom.Q ,LeftQ=Left.Q, RightQ=Right.Q, TotalQ=Total.Q, Cells=Cells, MeanVel1=Mean.Vel.1, MeanVel2=Mean.Vel.2, MeanVelMagn=Mean.Vel.magnitude , MeanVelDNorth=Mean.vel.dir.north, BoatVel1=Boat.Vel.1, BoatVel2=Boat.Vel.2, BoatVel3=Boat.Vel.3, BoatVel4=Boat.Vel.4, TrackRef=Track.Reference )
    } else{
   # extract RiverSurveyor's own GPS XY and add to exportable data.frame
        Easting <-unlist(rivMAT$GPS$UTM[,1])
        Northing <-unlist(rivMAT$GPS$UTM[,2])
        Satellites <-unlist(rivMAT$GPS$Satellites)
        HDOP <-unlist(rivMAT$GPS$HDOP)
        GPSquality <-unlist(rivMAT$GPS$GPS.Quality)
        Altitude <-unlist(rivMAT$GPS$Altitude)
        GpsGeoid <-unlist(rivMAT$GPS$GpsGeoid)
        Elevation <- Altitude+GpsGeoid

        dat <- data.frame(rivEast = Easting, rivNorth=Northing, rivEllGRS80=Elevation, rivGPSqual=GPSquality, rivGpsHDOP=HDOP, rivSats=Satellites, rivUTC= UTC, daySec= rivDaySec, monthSec=rivMonthSec, MSrounded=rivMonthSecRounded, depth=depth, snsDpthSet=sensorDepthSet, VtgTmStp= VtgTmStamp, VtgTMG=VtgTmgMPS, VtgSOG= VtgSogMPS,  VBdepth=VBdepth, BTdepth=BTdepth, BTbmDpth1=BTbeamDepth1, BTbmDpth2=BTbeamDepth2, BTbmDpth3=BTbeamDepth3, BTbmDpth4=BTbeamDepth4, BTbmDpthM=BTbeamDepthMean, BTfreq=BTfreq, SysTime=SysTime, Sample=Sample , rivGpsUTC=gpsUTC, rawGgaUTC=rawGgaUTC, Temp=Temperature, TopQ=Top.Q, MiddleQ=Middle.Q, BottomQ=Bottom.Q ,LeftQ=Left.Q, RightQ=Right.Q, TotalQ=Total.Q, Cells=Cells, MeanVel1=Mean.Vel.1, MeanVel2=Mean.Vel.2, MeanVelMagn=Mean.Vel.magnitude , MeanVelDNorth=Mean.vel.dir.north, BoatVel1=Boat.Vel.1, BoatVel2=Boat.Vel.2, BoatVel3=Boat.Vel.3, BoatVel4=Boat.Vel.4, TrackRef=Track.Reference )


    }




    rivDat <- rbind(rivDat, dat)
    dat <- NA
  }
# riv <- readMat(RiverSurveyorFile)
# names(riv)
  return(rivDat)
}
########### ------------------------------------------



# rivMAT <- readMat("./depth_GPS_points/20110909122235.mat")
# rivTime <- getRSurvDepthTstamp("./depth_GPS_points", "UTC")







#riv <- RSurvTimestamp(path="/Users/claude/GIS/Pulmanki/River_Surveyor_raw_data/2013_kevat/2013_05_18/")


RSurvTimestamp <- function(path=getwd() ){
    #Set up dataframe to populate by the loop
    rivDat <- data.frame()
    # read MAT files
    pathnames <- batchFiles("mat", path=path)
    for (pathname in pathnames) {
        #cat("pathname :", pathname, "\n")
        #cat("pathnames :", pathnames, "\n")
        #cat("length pathnames :", length( pathnames ), "\n")
        #cat("which pathname in pathnames:", which(pathname %in% pathnames), "\n")
        #cat("match pathname in pathnames:", match(pathname , pathnames), "\n")
        cat("Reading MAT file: ", basename(pathname), "\n", sep="")
        filename <- sub(".mat$|$","",basename(pathname))
        riv <- readRivMat(pathname)

        UTC                 <- unlist(riv$GPS$Utc) # 3 UTC
        #cat(UTC, "\n")
        # rivDaySec <- hhmmss2daySec(trunc( UTC) )
        rivDaySec           <- hhmmss2daySec( UTC )
        #cat(rivDaySec, "\n")
        rivDate             <- YMDhmss2date(as.numeric( filename ))
        #cat(print(rivDate), "\n") 
        rivMonthSec         <- getMonthSecs(rivDate, rivDaySec)
        #cat(rivMonthSec, "\n") 
        rivMonthSecRounded  <- round(rivMonthSec)
        #date <- factor(x=c(rivDate[[1]],rivDate[[2]], rivDate[[3]]), levels=1)
        #date <- factor(paste(date, sep=""))
        file <- factor(filename)
        # Extract RiverSurveyor GPS data
        #depth               <- as.numeric( riv$Summary$Depth )
        Sample              <- as.numeric( riv$System$Sample )

    #set up output dataframe
    dat <- data.frame(
                      UTC               = UTC                    ,
                      rivDaySec         = rivDaySec              ,
                      #date              = date                   ,
                      file              = file                   ,
                      rivMonthSec          = rivMonthSec            ,
                      Sample            = Sample
                      )

        rivDat <- rbind(rivDat, dat)
        dat <- NA
      }
# riv <- readMat(RiverSurveyorFile)
# names(riv)
  return(rivDat)
}


#func <- function(x,y){merge(x, y, by.x=names(x)[1], by.y=names(y)[1])}
#lapply(list.df, func, df)



getRivMonthSecs <- function(pathname=path, UTC=UTC ){
        filename <- sub(".mat$|$","",basename(pathname))
        rivDaySec           <- hhmmss2daySec( UTC )
        #cat(rivDaySec, "\n")
        rivDate             <- YMDhmss2date(as.numeric( filename ))
        #cat(print(rivDate), "\n") 
        rivMonthSec         <- getMonthSecs(rivDate, rivDaySec)
        return(rivMonthSec)
}



# Other stuff that I tried:
#######################################################################
#######################################################################

RSurvTimestampFULL <- function(path=getwd(), timeStamp="UTC", includeRivGPS=FALSE){
    #Set up dataframe to populate by the loop
    rivDat <- list()
    # read MAT files
    pathnames <- batchFiles("mat", path=path)
    for (pathname in pathnames) {
        #cat("pathname :", pathname, "\n")
        #cat("pathnames :", pathnames, "\n")
        #cat("length pathnames :", length( pathnames ), "\n")
        #cat("which pathname in pathnames:", which(pathname %in% pathnames), "\n")
        #cat("match pathname in pathnames:", match(pathname , pathnames), "\n")
        cat("Reading MAT file: ", basename(pathname), "\n", sep="")
        filename <- sub(".mat$|$","",basename(pathname))
        riv <- readRivMat(pathname)

        UTC                 <- unlist(riv$GPS$Utc) # 3 UTC
        # rivDaySec <- hhmmss2daySec(trunc( UTC) )
        rivDaySec           <- hhmmss2daySec( UTC )
        rivDate             <- YMDhmss2date(as.numeric( filename ))
        rivMonthSec         <- getMonthSecs(rivDate, rivDaySec)
        rivMonthSecRounded  <- round(rivMonthSec)
        date <- c(rivDate[[1]],rivDate[[2]], rivDate[[3]])
        # Extract RiverSurveyor GPS data
        Easting             <-unlist(riv$GPS$UTM[,1])
        Northing            <-unlist(riv$GPS$UTM[,2])
        Satellites          <-unlist(riv$GPS$Satellites)
        HDOP                <-unlist(riv$GPS$HDOP)
        GPSquality          <-unlist(riv$GPS$GPS.Quality)
        Altitude            <-unlist(riv$GPS$Altitude)
        GpsGeoid            <-unlist(riv$GPS$GpsGeoid)
        Elevation           <- Altitude+GpsGeoid
        depth               <- as.numeric( riv$Summary$Depth )
        sensorDepthSet      <- unlist(riv$Setup$sensorDepth[1])
        VBdepth             <- as.numeric( riv$BottomTrack$VB.Depth )
        BTdepth             <- as.numeric( riv$BottomTrack$BT.Depth )
        Top.Q               <- as.numeric( riv$Summary$Top.Q )
        Middle.Q            <- as.numeric( riv$Summary$Middle.Q )
        Bottom.Q            <- as.numeric( riv$Summary$Bottom.Q )
        Left.Q              <- as.numeric( riv$Summary$Left.Q )
        Right.Q             <- as.numeric( riv$Summary$Right.Q )
        Total.Q             <- as.numeric( riv$Summary$Total.Q )
        Cells               <- as.numeric( riv$Summary$Cells )
        Temperature         <- as.numeric( riv$System$Temperature )
        Sample              <- as.numeric( riv$System$Sample )
        velocity.x            <- unlist(riv$WaterTrack$Velocity[,1,])
        velocity.y            <- unlist(riv$WaterTrack$Velocity[,2,])
        velocity.u            <- unlist(riv$WaterTrack$Velocity[,3,])
        meanVel.x             <- unlist(riv$Summary$Mean.Vel[,1])
        meanVel.y             <- unlist(riv$Summary$Mean.Vel[,2])
        boatVel.x             <- unlist(riv$Summary$Boat.Vel[,1])
        boatVel.y             <- unlist(riv$Summary$Boat.Vel[,2])
        Track.Reference     <- as.numeric( riv$Summary$Track.Reference )
        SysTime             <- as.numeric( riv$System$Time )
        #BTvel               <- unlist( riv$BottomTrack$BT.Vel)
        #BTfreq              <- as.numeric( riv$BottomTrack$BT.Frequency )



    #set up output dataframe
    dat <- data.frame(
                      Easting           = Easting                ,
                      Northing          = Northing               ,
                      Elevation         = Elevation              ,
                      GPSqual           = GPSquality             ,
                      HDOP              = HDOP                   ,
                      Satellites        = Satellites             ,
                      UTC               = UTC                    ,
                      rivDaySec         = rivDaySec              ,
                      #date           = date                ,
                      monthSec       = rivMonthSec            ,
                      Sample            = Sample                 ,
                      Cells             = Cells                  ,
                      depth             = depth                  ,
                      #sensDpthSet       = sensorDepthSet         ,
                      VBdepth           = VBdepth                ,
                      BTdepth           = BTdepth                ,
                      Top.Q             = Top.Q                  ,
                      Middle.Q          = Middle.Q               ,
                      Bottom.Q          = Bottom.Q               ,
                      Left.Q            = Left.Q                 ,
                      Right.Q           = Right.Q                ,
                      Total.Q           = Total.Q                ,
                      Temp              = Temperature            ,
                      #velocity.x          = velocity.x               ,
                      #velocity.y          = velocity.y               ,
                      #velocity.u          = velocity.u               ,
                      meanVel.x           = meanVel.x                ,
                      meanVel.y           = meanVel.y                ,
                      boatVel.x           = boatVel.x                ,
                      boatVel.y           = boatVel.y                ,
                      trackRef          = Track.Reference
                      #BTvel             = BTvel                  ,
                      #BTfreq            = BTfreq
                      )
        if(includeRivGPS==FALSE){
            dat$Easting <- NULL
            dat$Northing <- NULL
            dat$Elevation <- NULL
            dat$HDOP <- NULL
            dat$Satellites <- NULL
        }






        rivDat <- rbind(rivDat, dat)
            #if(match(pathname , pathnames)==1){
                #rivDat <- dat
            #}else{
                #cat("Merging ", pathname, " to list\n")
                #rivDat <- mapply(c, rivDat, dat, SIMPLIFY=FALSE)
                ##rivDat <- mapply(merge, rivDat, dat) #takes forever
                ##rivDat <- lapply( rivDat, func,  dat) #
            #}
        dat <- NA
      }
# riv <- readMat(RiverSurveyorFile)
# names(riv)
  return(rivDat)
}


#######################################################################

RSurvTimestampList <- function(path=getwd(), timeStamp="UTC", includeRivGPS=FALSE){
    #Set up dataframe to populate by the loop
    rivDat <- list()
    # read MAT files
    pathnames <- batchFiles("mat", path=path)
    for (pathname in pathnames) {
        #cat("pathname :", pathname, "\n")
        #cat("pathnames :", pathnames, "\n")
        #cat("length pathnames :", length( pathnames ), "\n")
        #cat("which pathname in pathnames:", which(pathname %in% pathnames), "\n")
        #cat("match pathname in pathnames:", match(pathname , pathnames), "\n")
        cat("Reading MAT file: ", basename(pathname), "\n", sep="")
        filename <- sub(".mat$|$","",basename(pathname))
        riv <- readRivMat(pathname)

        UTC                 <- unlist(riv$GPS$Utc) # 3 UTC
        # rivDaySec <- hhmmss2daySec(trunc( UTC) )
        rivDaySec           <- hhmmss2daySec( UTC )
        rivDate             <- YMDhmss2date(as.numeric( filename ))
        rivMonthSec         <- getMonthSecs(rivDate, rivDaySec)
        rivMonthSecRounded  <- round(rivMonthSec)
        # Extract RiverSurveyor GPS data
        Easting             <-unlist(riv$GPS$UTM[,1])
        Northing            <-unlist(riv$GPS$UTM[,2])
        Satellites          <-unlist(riv$GPS$Satellites)
        HDOP                <-unlist(riv$GPS$HDOP)
        GPSquality          <-unlist(riv$GPS$GPS.Quality)
        Altitude            <-unlist(riv$GPS$Altitude)
        GpsGeoid            <-unlist(riv$GPS$GpsGeoid)
        Elevation           <- Altitude+GpsGeoid
        depth               <- as.numeric( riv$Summary$Depth )
        sensorDepthSet      <- unlist(riv$Setup$sensorDepth[1])
        VBdepth             <- as.numeric( riv$BottomTrack$VB.Depth )
        BTdepth             <- as.numeric( riv$BottomTrack$BT.Depth )
        Top.Q               <- as.numeric( riv$Summary$Top.Q )
        Middle.Q            <- as.numeric( riv$Summary$Middle.Q )
        Bottom.Q            <- as.numeric( riv$Summary$Bottom.Q )
        Left.Q              <- as.numeric( riv$Summary$Left.Q )
        Right.Q             <- as.numeric( riv$Summary$Right.Q )
        Total.Q             <- as.numeric( riv$Summary$Total.Q )
        Cells               <- as.numeric( riv$Summary$Cells )
        Temperature         <- as.numeric( riv$System$Temperature )
        Sample              <- as.numeric( riv$System$Sample )
        velocity            <- unlist(riv$WaterTrack$Velocity)
        meanVel             <- unlist(riv$Summary$Mean.Vel)
        boatVel             <- unlist(riv$Summary$Boat.Vel)
        Track.Reference     <- as.numeric( riv$Summary$Track.Reference )
        SysTime             <- as.numeric( riv$System$Time )
        BTvel               <- unlist( riv$BottomTrack$BT.Vel)
        BTfreq              <- as.numeric( riv$BottomTrack$BT.Frequency )


    #set up output dataframe
    dat <- list(
                      Easting           = Easting                ,
                      Northing          = Northing               ,
                      Elevation         = Elevation              ,
                      GPSqual           = GPSquality             ,
                      HDOP              = HDOP                   ,
                      Satellites        = Satellites             ,
                      UTC               = UTC                    ,
                      rivDaySec         = rivDaySec              ,
                      rivDate           = rivDate                ,
                      rivMonthSec       = rivMonthSec            ,
                      rivMonthSecRound  = rivMonthSecRounded     ,
                      Sample            = Sample                 ,
                      Cells             = Cells                  ,
                      depth             = depth                  ,
                      sensDpthSet       = sensorDepthSet         ,
                      VBdepth           = VBdepth                ,
                      BTdepth           = BTdepth                ,
                      Top.Q             = Top.Q                  ,
                      Middle.Q          = Middle.Q               ,
                      Bottom.Q          = Bottom.Q               ,
                      Left.Q            = Left.Q                 ,
                      Right.Q           = Right.Q                ,
                      Total.Q           = Total.Q                ,
                      Temp              = Temperature            ,
                      velocity          = velocity               ,
                      meanVel           = meanVel                ,
                      boatVel           = boatVel                ,
                      trackRef          = Track.Reference        ,
                      BTvel             = BTvel                  ,
                      BTfreq            = BTfreq
                      )
        if(includeRivGPS==FALSE){
            dat$Easting <- NULL
            dat$Northing <- NULL
            dat$Elevation <- NULL
            dat$HDOP <- NULL
            dat$Satellites <- NULL
        }






        #rivDat <- rbind(rivDat, dat)
            if(match(pathname , pathnames)==1){
                rivDat <- dat
            }else{
                cat("Merging ", pathname, " to list\n")
                rivDat <- mapply(c, rivDat, dat, SIMPLIFY=FALSE)
                #rivDat <- mapply(merge, rivDat, dat) #takes forever
                #rivDat <- lapply( rivDat, func,  dat) #
            }
        dat <- NA
      }
# riv <- readMat(RiverSurveyorFile)
# names(riv)
  return(rivDat)
}

# the difference between the UNIX epoch and GPS epoch.
#          epochdiff <-  315964819
# d <- 20110620231004
# d/1000000
# yyyymmdd <- d/1000000
# x <- yyyymmdd2gpsSecs(yyyymmdd)
# x %% 604800
# pos <- as.POSIXct(x, origin="1980-06-01", tz="GMT")
# pos <- as.POSIXct(x, origin="2011-01-01", tz="GMT")
# pos <- as.POSIXct(x,  tz="GMT")
# num <- as.numeric(pos)
# as.POSIXlt(num)
# as.POSIXlt(pos)
# as.numeric(as.POSIXct(humanTime, origin="1970-01-01", tz="GMT"))

# edif <- num - epochdiff
# edif %% 604800
# num %% 604800

# w <- 1641
# gw <- w * 604800

# num-gw
# edif-gw

# # http://www.epochconverter.com/
# # How to get the current epoch time in ...
#     Sys.time()
#     as.numeric(Sys.time())
#     epoch <- as.numeric(Sys.time())
# # Convert from epoch to human readable date
#     as.POSIXct(epoch, origin="1970-01-01")
#     humanTime <- as.POSIXct(epoch, origin="1970-01-01")
# # Convert from human readable date to epoch
#     as.numeric(as.POSIXct("MM/dd/yyyy HH:mm:ss", origin="1970-01-01"))
#     as.numeric(as.POSIXct(humanTime, origin="1970-01-01"))


# # Converting HHMMSSS to day seconds:

# hhmmss <- 92823.9

# hh <- trunc(hhmmss/10000)
# mm <- trunc( (hhmmss - hh*10000) /100 )
# ss <- round( hhmmss - hh * 10000 - mm * 100, 2 )

# hms <- as.POSIXct(paste(hh,":",mm,":",ss, sep=""), format="%H:%M:%S", origin="1970-01-01", tz="GMT")

# as.numeric(hms)

# daySeconds <- hh * 3600 + mm * 60 + ss


# epoch <- as.numeric(Sys.time())
# humanTime <- as.POSIXct(epoch, origin="1970-01-01")
# as.numeric(as.POSIXct(humanTime, origin="1970-01-01"))

# epoch <- as.numeric(Sys.time())
# humanTime <- as.POSIXct(epoch, origin="1970-01-01", tz="GMT")
# as.numeric(as.POSIXct(humanTime, origin="1970-01-01", tz="GMT"))

# paakUTChhmmss = 92823.9
# AnttoniUTCsec = 552480.00
# gga           = 368974147.815823
# vtg           = 368974147.825757

# AntoniDaySec <- GPSdaySec(AnttoniUTCsec)
# paakDaySec <- hhmmss2daySec(paakUTChhmmss)
# GGAdaySec <- GPSdaySec(gga)
# GGAhhmmss <- daySec2hhmmss(GGAdaySec)
# GGAhhmmss - 30000 # Somewhere there seems to be a time-zone problem here
# AnttoniHHMMSS <- daySec2hhmmss(AntoniDaySec)
# VTGdaySec <- GPSdaySec(vtg)
# VTGhhmmss <- daySec2hhmmss(VTGdaySec)
# GGAhms <- as.POSIXct(gga, origin="1980-01-06", tz="GMT")
# as.POSIXct(gga, tz="GMT")

# (z <- Sys.time())             # the current datetime, as class "POSIXct"
# unclass(z)                    # a large integer
# floor(unclass(z)/86400)       # the number of days since 1970-01-01 (UTC)
# (z <- as.POSIXlt(Sys.time())) # the current datetime, as class "POSIXlt"
# unlist(unclass(z))            # a list shown as a named vector


# floor(unclass(gga)/86400)       # the number of days since 1970-01-01 (UTC)

# GPSweekSecs <- function(secs){
#     week <- trunc(secs / 604800)
#     weekSecs <- secs %% 604800
#     cat("Week number: ", week, "  seconds: ", weekSecs, "\n", sep="" )
#     return(weekSecs)
# }

# ggaWeekSecs <- GPSweekSecs(gga)
# ggaWeek2DaySecs <- GPSdaySec(ggaWeekSecs)








# epoch=92823.9
# e=92823.9
# epoch=092823
# gga=368974147.815823
# epoch <- as.numeric(Sys.time())
# humanTime <- as.POSIXct(epoch, format="%H%M%S", origin="1970-01-01", tz="GMT")
# as.numeric(as.POSIXct(humanTime, origin="1970-01-01", tz="GMT"))

# x <- c("1jan1960", "2jan1960", "31mar1960", "30jul1960")
# z <- strptime(x, "%d%b%Y")
# z <- strptime(x, "%d%b%Y")
# z <- strptime(epoch, "%H%M%S")

# library(chron)
# x <- chron(times.=epoch, format="%H%M%S", out.format="%H:%M:%S")
# tt1 <- times(format(x, "%H:%M:%S"))

# lt <- as.POSIXlt("2010-10-18 21:46:53")
# tt1 <- times(format(lt, "%H:%M:%S"))




# Sys.Date()
# Sys.time()
# t=92823.9
# as.POSIXct(t, origin=Sys.Date())
# as.POSIXct(t, format="%H:%M:%OS3", origin=Sys.Date())
# tday <- as.POSIXlt(Sys.Date(), tz="GMT")
# now <- as.POSIXlt(Sys.time(), tz="GMT")
# as.POSIXct(tday)
# midnight <- as.numeric(tday)
# as.numeric(now)
# x <- midnight + t
# POSIXorigin <- as.POSIXct(ISOdatetime(1970,1,1,0,0,0))
# POSIXorigin <- as.POSIXlt(ISOdatetime(1970,1,1,0,0,0), tz="GMT")
# POSIXorigin <- as.POSIXlt(ISOdatetime(1970,1,1,0,0,0), tz="EET")
# po <- as.numeric(POSIXorigin )
# as.POSIXct(0, origin = POSIXorigin, tz="GMT")
# as.POSIXct(po, origin = POSIXorigin, tz="GMT")
# as.POSIXct(POSIXorigin , format="%H:%M:%OS3", origin=POSIXorigin, tz="GMT" )
# as.POSIXct(POSIXorigin ,  origin=POSIXorigin, tz="GMT" )
# as.POSIXct(x, origin=POSIXorigin )
# as.POSIXct(t, origin=POSIXorigin )

# as.POSIXct(t, format="%H:%M:%OS3", origin=POSIXorigin)
