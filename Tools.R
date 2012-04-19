
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
