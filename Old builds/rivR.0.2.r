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

    # This code is was written by Claude Flener, 2011


# compute Lyzenga's X and regression between X and depth
# returns the regression (intended to get the regression coefficient out via summary)
LyzengaX <- function(DN, depth, deepWater){
	deepWater = deepWater
	data <- data.frame(DN=DN,depth=depth)
	colnames(data) <- c("DN","depth")
	data$Lyz_X <- log(data$DN - deepWater)
	colnames(data) <- c("DN","depth","Lyz_X")
	na.exclude(data)
        # catch possible infinites produced by log (na.exclude
        # doesn't catch INF
        data <- data[which(is.finite(data$Lyz_X)) , ]
#       cat("length of data after filtering NAs and INFs", length(data$DN),"\n")
#	print(summary(data$LyzX))
	LyzFit <- try(lm(formula = data$depth ~ data$Lyz_X, na.action=na.exclude))
        
        # print(summary(LyzFit))
	return(LyzFit)
}


# Calculate the substitute for Lyzenga's deep water reflectance by finding the reflectance that linearises the depth-reflectance relationship for one band
# taking residual spread into account
        # verbose outputs the whole calibration loop, otherwise only the result is printed to screen. The silent option turns off all printing.

calibrateDeepWaterRef <- function(DN, depth, seed = 1, margin=0.01, verbose=FALSE, silent = FALSE,  anime=FALSE,  legendpos="topright", addline=FALSE){
  # DN <- DN +1000  #in order to avoid negative log values. Slows things down considerably,  but avoids errors.
     # cat("############## margin: ",margin , "\n")  ############### Debugging                       

# Check these initial coefficient settings if the calibration gets stuck early
	coeff = 9 # set initial value for coefficient
	coeffrounded = 9 # set initial value for coefficient
	oldSeed = 999
	oldoldSeed = 888
        left = -1 - margin
        right = -1 + margin
        Xi <- 1000
        if(anime==TRUE) ani.record(reset = TRUE)   # clear history before recording

        if(force==TRUE)cat("WARNING! Forcing calibration to continue into negative seed values may result in an infinite loop!\n")
		# When coefficient is near -1, relationship is linear
		# since the reflectance values are integers, a margin near -1 needs to be sufficiently precise.
        if(mean(DN) > 1){ #if mean(DN) < 1 , DNs are probably band ratio
                          # next if handles band ratios

                            if (verbose==TRUE) print(paste("oldoldSeed: ", oldoldSeed),quote=F)
                            if (verbose==TRUE) print(paste("oldSeed: ", oldSeed),quote=F)
                            if (verbose==TRUE) print(paste("Now starting...", oldSeed),quote=F)

            while(( coeffrounded  <= left | coeffrounded  >= right ) && min(Xi)>0){
                if(coeff == 0){ # 0 occurs when too many NAs are produced and no data is left
                    cat("An error occurred. The data could not be linearised to within a margin of", margin,"of -1. The last deep water reflectance value before the error was: ",seed ,"\n")
                    return(seed)
                }
                if (verbose==TRUE) cat("correlation coefficient: ", coeffrounded, "\n")
                    # check if target search has stabilised
                    if (seed != oldSeed && seed != oldoldSeed){    
                        oldoldSeed <- oldSeed
                        oldSeed  <-  seed
                            if (verbose==TRUE) print(paste("oldoldSeed: ", oldoldSeed),quote=F)
                            if (verbose==TRUE) print(paste("oldSeed: ", oldSeed),quote=F)
                        if(coeff < -1){
                            seed <-  seed - 1
                        } else
                        if(coeff > -1){
                            seed <-  seed + 1
                        }
                            if (verbose==TRUE) print(paste("seed: ", seed),quote=F)
                        X <- Lyzenga.basic(DN, depth, deepWater = seed)
                        coeffOld <- coeff
                        coeff <- cor(X$model$"data$depth", X$model$"data$Lyz_X")
                        coeffrounded <- round(coeff, digits=2)
                        Xi <- log(DN-seed)
                        if (verbose==TRUE) cat("min Xi: ", min(Xi), "\n")

                        if(anime==TRUE){
                            plot(Xi,depth, xlab="Xi (calibrated Lsi)", ylab="Depth", xlim=c(0,max(Xi)))
                            legend(legendpos, inset=0.02, bty="n", legend=c("L_si: ",seed,"coeff.: ", round( coeff, 2 ), "min Xi: ", round( min(Xi), 2 )))
                            if(addline==TRUE) abline(mean(Xi), -1, col="red")

                            ani.record()   # record the current frame
                            }

                    }
                       if ( seed == oldoldSeed) break
                       # make sure the coefficient does not move in the
                       # wrong direction. This can happen even though the
                       # seeds are still moving in the right direction.
                       if ( coeff > coeffOld){
                           if (verbose==TRUE) cat(" \n Breaking loop due to coefficient decreasing\n")
                           seed <- oldSeed
                           break
                       }
                }
                 
        }
        oldoldSeed <- seed
        
                
   DeepWaterRef <- seed 
  # DeepWaterRef <- round(seed -1000) #/10 to revert the *10 effect in the first line
		if (verbose==TRUE && silent ==FALSE){
			print(paste("Deep water reflectance: ", DeepWaterRef),quote=F) 
			print(paste("Coefficient: ", coeff),quote=F)
			}
		return(DeepWaterRef)
        }






# Batch calculate deep water values for several bands
# Input needs to be a data frame containing DN_1 DN_2 ... Depth
# returns an array of the deep water values for each band with the same index as the input data frame (minus the depth column)
getDeepWaterValues <- function(data, verbose=TRUE, silent=FALSE, ...){
	deepWaterValues <- c()
	for(i in 1:(length(data)-1)){
		if (verbose==TRUE){
                    print(paste("Band: ", i),quote=F)
                    S <- calibrateDeepWaterRef(DN= data[i], depth= data[length(data)], seed= 1, verbose=TRUE)
		} else
                    S <- calibrateDeepWaterRef(DN= data[i], depth= data[length(data)], seed= 1, verbose=FALSE)
                    deepWaterValues[i] <- S	
		}
	if(silent==FALSE) print(deepWaterValues)
	return(deepWaterValues)
}





