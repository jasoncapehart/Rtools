
#-------------------------------------------
#Discretize continuous variable by frequency
#-------------------------------------------

#MUST BE FIXED!! Does not work with num.bins = 5, 15 or anything else other than multiples of 10!

#Input: Continuous vector, number of desired observations per bin
#Output: A list with two elements
# $summary which contains a list of each bin, it's min, max, and median value
# $binned which contains the original vector and it's corresponding bucket's median value
  
disc.freq <- function(vec, num.bins) {
  vec <- vec[order(vec)]
  len <- length(vec)
  cut.size <- len / num.bins
  cut.vec <- seq(from = 1, to = len+1, by = cut.size)
  cut.vec[length(cut.vec)] <- cut.vec[length(cut.vec)] - 1 #Adjust the cap of the cut.vec for the increment starting at 1
  i <- 1
  freq.df <- NULL
  output.df <- NULL
  max.iter <- length(cut.vec)-1
  for (i in 1:max.iter) {
    #Set min and max row values
    min.row <-cut.vec[i]
    max.row <- ifelse(i != max.iter, cut.vec[i+1]-1, cut.vec[i+1]) #Adjust so the last record isn't cut
    #Get the defining values of the bin
    low <- vec[min.row]
    high <- vec[max.row]
    median.val <- median(vec[min.row:max.row])
    #Store the defining values
    freq.descrip <- data.frame("bin" = i, "low" = low, "high" = high, "median" = median.val)
    freq.df <- rbind(freq.df, freq.descrip)
    #Combine the median value along with the original vector
    rep.times <- length(vec[min.row:max.row])
    rep.median <- rep(median.val, times = rep.times)
    output <- data.frame("Original" = vec[min.row:max.row], "BinMedian" = rep.median)
    output.df <- rbind(output.df, output)
  }
  freq.list <- list("summary" = freq.df, "binned" = output.df)
  return(freq.list)
}



#-------------------------------------------
#Discretize continuous variable by # of bins
#-------------------------------------------
  
#Input: Continuous vector, number of desired bins
#Output: Discretized vector, with elements corresponding to bin cutoff position
#Required: foreach() package
#Notes: Can be run in parallel by switching "%do%" for "%dopar%" and adding the SNOW package
  
disc.width <- function(vector, num.bins) {
 range <- max(vector) - min(vector)
 cut <- range / num.bins
 bin.vector <- foreach(i=1:length(vector), .combine="c") %do% {round(vector[i]/cut, digits=0)*cut}
 return(bin.vector)
}
#----------------------------------------------


#---------------------------------------------
#Proportion of Events by Discrete Bin
#---------------------------------------------
#Input: Binary outcome variable, vector of discretized continuous variable
#Output: The proportion of the "1" class out of all trials for each bin

prop.by.bin <- function(binary.output, disc.vector) {
 #Levels of discretized variable
 bin.levels <- as.numeric(levels(as.factor(disc.vector)))
 #Total trials in each discrete bin
 trials <- as.vector(table(disc.vector))
 #Total events in each discrete bin
 events <- tapply(X=binary.output, INDEX=disc.vector, FUN=sum)
 #Proportion of events/trials
 prop <- events / trials
 #Data frame
 bin.prop.df <- data.frame("Bin" = bin.levels, "Proportion" = prop)
 
 return(bin.prop.df)
}



#------------------------------------------------
#Scale function
#-------------------------------------------------
#Input: Any numeric vector
#Output: Vector scaled between 0 and 1

scale.01 <- function(vector) {
 scaled.vector <- (vector-min(vector))/((max(vector)-min(vector)))
 return(scaled.vector)
}

#------------------------------------------------
#Scale Min-Max function
#-------------------------------------------------
#Input: Any numeric vector
#Output: Vector scaled between -1 and 1

scale.mm <- function(vector, mini = 0, maxi = 1) {
 scaled.vector <- (vector-min(vector))/((max(vector)-min(vector)))*(maxi-mini) + mini
 return(scaled.vector)
}

#----------------------------------------------
# RMSE function
#-------------------------------------

#Input: Any numeric vector of predictions and any numeric vector of the true output
#Output: The average absolute error

rmse <- function(pred.vec, output.vec) {
  error.vec <- (pred.vec - output.vec)^2
  avg.error <- sqrt(mean(error.vec))
  return(avg.error)
}

#----------------------
# Factor to Numeric Function
#----------------------
#Input: A factor with 

fact2.numeric <- function(vec) {
 numeric.vec <- as.numeric(levels(vec))[vec]
 return(numeric.vec)
}

#-----------------------
# Split Function
#-----------------------
#Input: Data frame, # of folds

split <- function(df, k.folds) {
  
  #Do a random sample based on # of folds
  rows <- dim(df)[1]
  sample.size <- rows - floor(rows/k.folds)
  
  df.train <- df[sample(nrow(df), sample.size, replace=FALSE), ]
  df.test <- df[!rownames(df) %in% rownames(df.train), ]
  
  split.df <- list(train=df.train, test=df.test)
  return(split.df)
}

#-----------------------------
#Ratio Interpolation
#---------------------------

#Input: numer = A continuous numerator vector,
# denom = a continuous denominator vector,
# n = number of points from left and right of 0 to sample from
#Ouptut: A ratio where the discontinuity at 0 has been replaced by an average
# of the ratio of the points to the left and right of the discontinuity
#Issues: The approximation is better for asymmetric than symmetric functions
# doing a linear or parabolic fit would give the best simple estimate for the
# ratio at 0

ratio.interp <- function(numer, denom, n=1) {
  
  df <- data.frame("Numer" = numer, "Denom" = denom)
  df.ratio <- NULL
  
  #If there are no zeros return the vector unaltered
  if (dim(df[df[, "Denom"] == 0, ])[1] == 0) df.ratio <-df$Numer/df$Denom
    
  #Otherwise replace the points where the Denominator is zero with an average
  else {
    #Order the Data Frame and Set a Rank
    df.order <- df[order(df[, "Denom"]), ]
    df.order <- data.frame(df.order, "Rank" = c(1:dim(df.order)[1]))
    
    #Determine the Rank of all Zeros
    rnk <- df.order[df.order$Denom == 0, "Rank"] 
    
    #Grab the "n" points above and below the lowest and highest zero
    bottom.rnk <- min(rnk) - c(1:n)
    top.rnk <- max(rnk) + c(1:n)
    
    #Find the bottom and top ratios then average them
    bot.n <- df.order[sapply(df.order$Rank, FUN=function(x) x %in% bottom.rnk) == TRUE, ]
    top.n <- df.order[sapply(df.order$Rank, FUN=function(x) x %in% top.rnk) == TRUE, ]
    avg.n <- rbind(bot.n, top.n)
    avg.ratio <- mean(avg.n$Numer/avg.n$Denom)
    
    #Replace the NaNs with the average ratio
    df.ratio <- df$Numer /df$Denom
    df.ratio[is.nan(df.ratio) == TRUE] <- avg.ratio
  }
  return(df.ratio)
}


#------------------------
# Group RMSE
#------------------------

#Input: A vector of true output that is a factor, and a vector of predicted values
#Output: The RMSE for each level of the factor
#Notes: The true output vector should be raw and not defined as a factor. If the
# data type of the output vector is a factor the output will be given according to the
# factor level rather than the value the factor level represents

group.rmse <- function(true.output, pred) {
    df <- data.frame("output" = true.output, "pred" = pred, "squared.error" = (true.output-pred)^2)
    grouped <- aggregate(df, by=list(df$output), FUN=function(x) sqrt(mean(x)))
    rmse.by.group <- data.frame("Group" = grouped$Group.1, "rmse" = grouped$squared.error)
    return(rmse.by.group)
}

#------------------------
# Champion Test Function
#------------------------

 champion.test <- function(x) {
    test <- NULL
    abs.rf <- abs( as.numeric(x["quality"]) - as.numeric(x["rf"]) )
    abs.knn <- abs( as.numeric(x["quality"]) - as.numeric(x["knn"]) ) 
    if (abs.rf < abs.knn) test <- "rf"
    else test <- "knn"
    return(test)
  }

#-------------------
# kNN Rule Function
#------------------

knn.rule <- function(x) {
  winner <- NULL
  dist <- abs( as.numeric(x["pred.knn"]) - round(as.numeric(x["pred.knn"]), 0) )
  if ( dist < 0.05 ) winner <- round(as.numeric(x["pred.knn"], 0))
  else winner <- as.numeric(x["pred.rf"])
  return(winner)
}

#-----------------------
# Loop timing snippet
#------------------------

curr.time <- Sys.time() - start
update <- paste("On loop", i, "with", round(curr.time, 2), attributes(curr.time)$units, "elapsed")
print(update)

#-------------------------
# Balanced Sample Function
#--------------------------
#Combines some undersampling of big classes and oversampling of small classes

#Input: data frame, factor variable to sample, # of samples
#Output: Balanced data frame


balanced.sample <- function(df, factor.col, sample.size) {
  #Levels of the factor variable
  level.list <- levels(as.factor(df[,factor.col]))
  #Number of levels for the factor variable
  num.factors <- length(level.list)
  
  #Initialize objects used in for loop
  df.balanced <- NULL
  i <- 1
 
  for (i in 1:num.factors) {
    #Get a data frame of only rows for the given level of the factor
    df.level.i <- subset(df, get(factor.col) == level.list[i])
    #Sample that level until the specified "sample.size" is reached
    level.i.sample <- df.level.i[sample(nrow(df.level.i), sample.size, replace=TRUE),]
    #Tack on the sample to the balanced data frame
    df.balanced <- rbind(df.balanced, level.i.sample)
  }
  return(df.balanced)
}


#-------------------------
# Over/Under Sample Function
#--------------------------
#Undersamples big classes and oversamples small classes by a set %

#Input: data frame, factor variable to sample, % of sampling (written as a number not a percent. For example: 200% = 2)
#Output: Resampled Data Frame


resample <- function(df, factor.col, sample.pct) {
  #Levels of the factor variable
  level.list <- levels(as.factor(df[,factor.col]))
  #Number of levels for the factor variable
  num.factors <- length(level.list)
  #Number of observations in the biggest class
  max.class.obs <- max(table(df[, factor.col]))
  #Number of observations to sample in the biggest class
  max.class.sample <- floor((1/sample.pct)*max.class.obs)
  
  #Initialize objects used in for loop
  df.balanced <- NULL
  i <- 1
 
  for (i in 1:num.factors) {
    #Get a data frame of only rows for the given level of the factor
    df.level.i <- subset(df, get(factor.col) == level.list[i])
    #Store the number of observations for factor "i"
    i.obs <- dim(df.level.i)[1]
    #If the number of observations times the sample.pct is greater than the max.class.sample
    # Then only sample the same number of records as the max.class.sample
    if (i.obs*sample.pct >= max.class.sample ) {
      #Sample the number of records in max.class.sample
      level.i.sample <- df.level.i[sample(nrow(df.level.i), max.class.sample, replace=TRUE),]
    }
    #Otherwise the number of records to sample is the number of records that exists times the sample.pct
    else {
      level.i.sample <- df.level.i[sample(nrow(df.level.i), floor(i.obs*sample.pct), replace=TRUE),]
    }
    #Tack on the sample to the balanced data frame
    df.balanced <- rbind(df.balanced, level.i.sample)
  }
  return(df.balanced)
}


#----------------------
# Global Heatmap Function
#-----------------------
#Input: df - A data frame
# x.axis - A factor with name "x.axis" in df
# y.axis - A factor with name "y.axis" in df
# heat - A numeric variable with name "heat" in df
#Output: A heatmap where the value of the "heat" variable is scaled across all combinations of x and y
#Requires: ggplot2

gg.heatmap <- function(df, x.axis, y.axis, heat) {
  p <- ggplot(data=df, aes_string(x=x.axis, y=y.axis)) +
    geom_tile(data=df, aes_string(fill=heat)) +
    scale_fill_gradient(low="red", high="white")
print(p)
}


#-----------------------
# Density Plot Function
#------------------------
#Input: A dataframe and a column number
#Output: a density plot of that column
#Requires: ggplot2

df.density <- function(df, col.num) {
   column.name <- colnames(df)[col.num]
   p <- ggplot(df, aes_string(x=paste(column.name))) + stat_density() +
     scale_x_continuous(name=paste(column.name)) + opts(title=paste(column.name))
   print(p)
}

#-----------------------
# Density Plot by Facet Function
#-----------------------
#Input: A dataframe, a column number, and the name of the 
# column that creates the facet. The faceted column must be
# a factor. The name of the facet must be in "quotes"
#Output: A faceted density plot
#Requires: ggplot2

df.facet <- function(df, col.num, facet.name) {
  column.name <- colnames(df)[col.num]
  facet.formula <- paste(facet.name, "~.")
  p <- ggplot(df, aes_string(x=paste(column.name))) + 
    geom_density(aes_string(fill="quality")) +
    facet_grid(paste(facet.formula), scales="free") +
    opts(title=paste(column.name))
  print(p)
}


#---------------------------
# Stacked Density Plot Example
#----------------------------
p <- ggplot(red, aes(x=fixed.acidity)) + geom_density(aes(fill=factor(quality)), position="stack")
print(p)


#----------------------------
# Submission Function Version June 23
#---------------------------


#Input: Test data frame
#Output: Predictions for each type
#Required: Preferred algorithm for "white" and "red" types, scale.01() function

submission <- function(wine.test, best.white, best.red) {
  
  #Whites
  white.test <- subset(wine.test, type == "white")
  white.pred <- predict(best.white, newdata=white.test)
  white.output <- data.frame(white.test, "Prediction" = white.pred)
  
  #Reds
  red.test <- subset(wine.test, type == "red")
  red.pred <- predict(best.red, newdata=red.test)
  red.output <- data.frame(red.test, "Prediction" = red.pred)
 
  #Combine
  wine.pred <-NULL
  wine.pred <- rbind(white.output, red.output)
  wine.pred <- wine.pred[order(wine.pred$WineID),]
  return(wine.pred)
}