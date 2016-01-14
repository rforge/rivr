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
# verbose outputs the whole calibration loop, otherwise only the result is printed to screen. The silent option turns off all printing.
calibrateDeepWaterRef <- function(DN, depth, seed = 1, margin=0.01, verbose=FALSE, silent = FALSE){
	coeff = -9 # set initial value for coefficient
	coeffrounded = -9 # set initial value for coefficient
	oldSeed = 999
	oldoldSeed = 888
        left = -1 - margin
        right = -1 + margin
        Xi <- 1000
		# When coefficient is near -1, relationship is linear
		# since the reflectance values are integers, a margin near -1 needs to be sufficiently precise.
        # cat("left= ", left, "     right =", right,"\n")
            while(( coeffrounded  <= left | coeffrounded  >= right ) && min(Xi)>0 ){
                if(coeff == 0){ # 0 occurs when too many NAs are produced and no data is left
                    cat("An error ocurred. The data could not be linearised to within a margin of", margin,"of -1. The last deep water reflectance value before the error was: ",seed ,"\n")
                    return(seed)
                }
                if (verbose==TRUE) cat("coefficient: ", coeff,"\n")
                    # check if target search has stabilised
                    if (seed != oldSeed && seed != oldoldSeed){    
                        oldoldSeed <- oldSeed
                        oldSeed  <-  seed
                            if (verbose==TRUE) print(paste("oldoldSeed: ", oldoldSeed),quote=F)
                            if (verbose==TRUE) print(paste("oldSeed: ", oldSeed),quote=F)
                        if(coeff < -1){
                            seed <-  seed + 1
                        } else
                        if(coeff > -1){
                            seed <-  seed - 1
                        }
                            if (verbose==TRUE) print(paste("seed: ", seed),quote=F)
                        X <- Lyzenga.basic(DN, depth, deepWater = seed)
                        coeff <- summary(X)$coefficients[2]
                        coeffrounded <- round(coeff, digits=2)
                        Xi <- log(DN-seed)
                        if (verbose==TRUE) cat("min Xi: ", min(Xi), "\n")
                            # print(summary(X))
                            # print(paste("Deep water reflectance: ", seed),quote=F)
                            # print(paste("Coefficient: ", coeff),quote=F)
                    }
                       if ( seed == oldoldSeed) break
                }
                       if ( seed == oldoldSeed){
                            while(coeffrounded  <= left | coeffrounded  >= right){
                                if (verbose==TRUE) cat("coefficient: ", coeff,"\n")
                                if (seed != oldSeed){ 
                                    oldoldSeed <- oldSeed
                                    oldSeed  <-  seed
                            if (verbose==TRUE) print(paste("oldoldSeed: ", oldoldSeed),quote=F)
                            if (verbose==TRUE) print(paste("oldSeed: ", oldSeed),quote=F)
                                if(coeff < -1){
                                    seed <-  seed + 0.01
                                } else
                                if(coeff > -1){
                                    seed <-  seed - 0.01
                                }
                                # seed <- round(seed, digits=2)
                                    if (verbose==TRUE) print(paste("seed: ", seed),quote=F)
                                X <- Lyzenga.basic(DN, depth, deepWater = seed)
                                coeff <- summary(X)$coefficients[2]
                                coeffrounded <- round(coeff, digits=2)
                                Xi <- log(DN-seed)
                                if (verbose==TRUE) cat("min Xi: ", min(Xi), "\n")
                            }
                        }
                    }
                
   DeepWaterRef <- seed 
		if (verbose==TRUE && silent ==FALSE){
			print(paste("Deep water reflectance: ", DeepWaterRef),quote=F) 
			print(paste("Coefficient: ", coeff),quote=F)
			}
		return(DeepWaterRef)
        }


# Calculate the substitute for Lyzenga's deep water reflectance by finding the reflectance that linearises the depth-reflectance relationship for one band
        # This illusrated version plots the data at each iteration.
calibrateDeepWaterRef_illustrated <- function(DN, depth, seed = 1, verbose=TRUE, record=TRUE){
	coeff = -9 # set initial value for coefficient
	oldSeed = 999
        minXi <- 10000
        
		# When coefficient is near -1, relationship is linear
		# since the reflectance values are integers, a margin near -1 needs to be sufficiently precise.
            if(record==TRUE) ani.record(reset = TRUE)   # clear history before recording

            while( (coeff < -1.05 | coeff > -0.95) && minXi>0){
                if(coeff == 0){ # 0 occurs when too many NAs are produced and no data is left
                    cat("An error ocurred. The data could not be linearised to witin a margin of 0.05 of -1. The last deep water reflectance value before the error was: ",seed ,"\n")
                    return(seed)
                }
                if(coeff < -1){
                    if (seed != oldSeed){    # check if target search has stabilised
                                        oldSeed  <-  seed
                                        seed <-  seed + 1
                        X <- Lyzenga.basic(DN, depth, deepWater = seed)
                         coeff <- summary(X)$coefficients[2]
            print(paste("Deep water reflectance: ", seed),quote=F)
            print(paste("Coefficient: ", coeff),quote=F)
            Xi <- log(DN-seed)
            minXi <- min(Xi)
            cat("min Xi: ", minXi, "\n")
            cat(str(Xi), "\n")
            plot(Xi,depth, xlab="Xi (calibrated Lsi)", ylab="Depth")
            legend("topright", inset=0.02, bty="n", legend=c("coeff.: ", round( coeff, 2 ), "min Xi: ", round( min(Xi), 2 )))
            if(record==TRUE) ani.record()   # record the current frame
                        }
                }else
                if(coeff > -1){
                    if (seed != oldSeed){
                            oldSeed  <-  seed
                    seed <-  seed - 1
                        X <- Lyzenga.basic(DN, depth, deepWater = seed)
                         coeff <- summary(X)$coefficients[2]
            print(paste("Deep water reflectance: ", seed),quote=F)
            print(paste("Coefficient: ", coeff),quote=F)
            Xi <- log(DN-seed)
            minXi <- min(Xi)
            cat("min Xi: ", minXi, "\n")
            cat(str(Xi), "\n")
            plot(Xi,depth)            
            legend("topright", inset=0.02, bty="n", legend=c("coeff.: ", coeff, "min Xi: ", min(Xi)))
            if(record==TRUE)  ani.record()   # record the current frame
                        }
                }
        }
  DeepWaterRef <- seed 
		if (verbose==TRUE){
			print(paste("Deep water reflectance: ", DeepWaterRef),quote=F) 
			print(paste("Coefficient: ", coeff),quote=F)
			}
		return(DeepWaterRef)
        }

# usage example:
# library(animation)
# dwtest <- calibrateDeepWaterRef_illustrated(dat$band1, dat$Depth, verbose=TRUE)
# ani.options(interval = 0.3)
# ani.replay()





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





