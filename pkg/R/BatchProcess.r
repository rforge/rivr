#This code handles batch-processing in R
 
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


#Generic function for batch processing a certain file type. If a single file is given as path, that will be handled using FUN. If a directory is given as path, all files of type ext in that directory will be processed using FUN.
batchProcess <- function(FUN=NA, ext=NA, path=getwd(), verbose=TRUE, interact=FALSE){
    if(class(ext)!="character") cat("Error: ext needs to be of format 'character'")
    extPattern <- paste0("*[.]",ext,"$") # avoid repeating regex
    # if path is a file name, start processing
    if(grepl(extPattern ,path)){
        # pathnames <- basename(path)
        pathname <- path  # required for for loop
        pathnames <- path # required for for loop
        path <- dirname(path) # required for writing the file later
    }else{
        # if path is a directory, get all .ext files and process them in sequence
        ext.files <- list.files(path=path, pattern=extPattern)
        pattern <- sprintf(extPattern, version)
        pathnames <- list.files(pattern=pattern, path=path, full.names=TRUE)
        #  filenamesfull <- list.files(pattern=pattern)
        # extract file names without extensions
        # filenames <- sub(".mat$|$","",filenamesfull)
        if(verbose==TRUE) cat("Loading all .",ext," files in ", path.expand(path), "...\n", sep="")
    }
    # read files
    for (pathname in pathnames) {
        cat("Processing .",ext," file: ", basename(pathname), "\n", sep="")
        #filename <- sub(extPattern,"",basename(pathname))
    x <- FUN(pathname)
    #return( FUN(pathname) )
        #optional interactive mode to interrupt processing before continuing to next file
        #if (interact==TRUE){
            #if (interactive()) {
                #cat("Press ENTER to view data:")
                #readline()
            #}
        #print(dat)
        #}
    }
    return(x)
}

#batchProcess(readMat, "mat", path="/Users/claude/GIS/Pulmanki/River_Surveyor_raw_data/2013_kevat/2013_05_18/20130518190800.mat")
#x <- batchProcess(readMat, "mat", path="/Users/claude/GIS/Pulmanki/River_Surveyor_raw_data/2013_kevat/2013_05_18")



#Generic function for gathering batches of a certain file type. This is primarily intended to be called from inside another function to add automatic batching functionality. Returns a vector of Pathnames to be used in a loop outside this function. If a single file is given as path, that path will be returned as a result. If a directory is given as path, all paths of files of type ext in that directory will be returned.
#Returns a vector "pathnames" of type Character
#Use in conjunction with a loop like this:
    #for (pathname in pathnames) {
        #cat("Processing .",ext," file: ", basename(pathname), "\n", sep="")
        #x <- FUN(pathname)
    #}
batchFiles <- function(ext=NA, path=getwd(), verbose=TRUE, interact=FALSE){
    if(class(ext)!="character") cat("Error: ext needs to be of format 'character'")
    extPattern <- paste0("*[.]",ext,"$") # avoid repeating regex
    # if path is a file name, start processing
    if(grepl(extPattern ,path)){
        # pathnames <- basename(path)
        pathname <- path  # required for for loop
        pathnames <- path # required for for loop
        path <- dirname(path) # required for writing the file later
    }else{
        # if path is a directory, get all .ext files and process them in sequence
        ext.files <- list.files(path=path, pattern=extPattern)
        pattern <- sprintf(extPattern, version)
        pathnames <- list.files(pattern=pattern, path=path, full.names=TRUE)
        #  filenamesfull <- list.files(pattern=pattern)
        # extract file names without extensions
        # filenames <- sub(".mat$|$","",filenamesfull)
        if(verbose==TRUE) cat("Loading all .",ext," files in ", path.expand(path), "...\n", sep="")
    }
    return(pathnames)
}





#Generic function for batch handling a certain file type. This is primarily intended for batch reading files of a specific type to be called from inside another function. If a single file is given as path, that will be handled using FUN and returned as a result. If a directory is given as path, all files of type ext in that directory will be handled  using FUN and the results returned as a list.
batchProcess2List <- function(FUN=NA, ext=NA, path=getwd(), merge=FALSE, verbose=TRUE, interact=FALSE){
    if(class(ext)!="character") cat("Error: ext needs to be of format 'character'")
    extPattern <- paste0("*[.]",ext,"$") # avoid repeating regex
    # if path is a file name, start processing
    if(grepl(extPattern ,path)){
        # pathnames <- basename(path)
        pathname <- path  # required for for loop
        pathnames <- path # required for for loop
        path <- dirname(path) # required for writing the file later
    }else{
        # if path is a directory, get all .ext files and process them in sequence
        ext.files <- list.files(path=path, pattern=extPattern)
        pattern <- sprintf(extPattern, version)
        pathnames <- list.files(pattern=pattern, path=path, full.names=TRUE)
        #  filenamesfull <- list.files(pattern=pattern)
        # extract file names without extensions
        # filenames <- sub(".mat$|$","",filenamesfull)
        if(verbose==TRUE) cat("Loading all .",ext," files in ", path.expand(path), "...\n", sep="")
    }
    # read files
    data <- list()
    if(merge==FALSE){
        for (pathname in pathnames) {
            cat("Processing .",ext," file: ", basename(pathname), "\n", sep="")
            filename <- sub(extPattern,"",basename(pathname))
        #data[basename(pathname)]  <- FUN(pathname)
            x <- FUN(pathname)
            #print(x)
            #data[[basename(pathname)]]  <- x
            data[[filename]]  <- x
        }
    }else{
        cat(pathnames[1], "\n\n")
        print(pathnames[2:length(pathnames)])
        data <- FUN(pathnames[1])
        for (pathname in pathnames[2:length(pathnames)]) {
            x <- FUN(pathname)
            data <- mapply(c, data, x, SIMPLIFY=FALSE)
        }
    }
    #return( FUN(pathname) )
        #optional interactive mode to interrupt processing before continuing to next file
        #if (interact==TRUE){
            #if (interactive()) {
                #cat("Press ENTER to view data:")
                #readline()
            #}
        #print(dat)
        #}
    
    return(data)
}

#x <- batchProcess2List(readMat, "mat", path="/Users/claude/GIS/Pulmanki/River_Surveyor_raw_data/2013_kevat/2013_05_18/20130518190800.mat")
#x <- batchProcess2List(readMat, "mat", path="/Users/claude/GIS/Pulmanki/River_Surveyor_raw_data/2013_kevat/2013_05_18")
#y <- x[[ 1 ]]
#length(x)
#names(x)[1]
#basename(names(x)[1])


readRivMat <- function(x){
    riv <- readMat(x)
    
    #extract structure names from Matlab format to use in R
    names(riv$Setup)       <- dimnames( riv$Setup)[[1]]
    names(riv$Processing)  <- dimnames( riv$Processing)[[1]]
    # Mat file contains two $System structures
    # Rename first one to Specs
    names(riv)[3]          <- "Specs"
    names(riv$Specs)       <- dimnames( riv$Specs)[[1]]
    names(riv$Transformation.Matrices) <- dimnames( riv$Transformation.Matrices)[[1]]
    names(riv$System)      <- dimnames( riv$System)[[1]]
    names(riv$Summary)     <- dimnames( riv$Summary)[[1]]
    names(riv$BottomTrack) <- dimnames( riv$BottomTrack)[[1]]
    names(riv$GPS)         <- dimnames( riv$GPS)[[1]]
    names(riv$WaterTrack)  <- dimnames( riv$WaterTrack)[[1]]
    names(riv$Compas)      <- dimnames( riv$Compas)[[1]]
    names(riv$RawGPSData)  <- dimnames( riv$RawGPSData)[[1]]
    return(riv)

}
