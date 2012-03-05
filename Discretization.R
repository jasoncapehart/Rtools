# Discretization
# Equal Width

# TODO: A real discretization function
#          - Meaningful bucket names and options for bucket names (Range, Mean, Median)
#           - Equal width option 1st, equal freq option 2nd
#           - Count of observations in each bucket
#       An binary aggregation function
#          - Input: Take a binary variable and a factor list
#          - Output: Count of factors in each bucket


#----------------------
# Discretize Data Frame
# Input: df - data frame of numeric data to discretize by column
#        disc.fun - a discretization function
#        num.bin.seq - the number of bins to use for each column
# Output: discretized.df - the same data, in the same order in discrete buckets
# Calls: the specified disc.func
# TODO: Set up disc.df() to handle NAs
#----------------------

disc.df <- function(df, disc.func = eq.width.disc, num.bin.seq = 10) {
 
  # Find the number of columns
  cols <- dim(df)[2]
  rows <- dim(df)[1]
  # Preallocate
  mat.discretized <- matrix(data=NA, nrow=rows, ncol=cols)
  # If a number of bins isn't specified for each column use the 1st element for every col
  if (length(num.bin.seq) < cols) { num.bin.seq <- rep(num.bin.seq[1], times = cols) }
 
  for (i in 1:cols) {
    col.i.disc <- disc.func(df[, i], num.bins = num.bin.seq[i])
    mat.discretized[, i] <- col.i.disc$agg.fun
  }
  df.discretized <- data.frame(mat.discretized)
  colnames(df.discretized) <- paste(colnames(df), ".disc", sep='')
  return(df.discretized)
}


#------------------
# Equal Width Discretize Function
#    For the moment, this only does equal width discretization
#      and does not handle NAs
# Input: input - the numeric data to discretize
#        num.bins - the number of desired buckets
#        agg.fun - mean by default, or a function to give each bin a numeric name
# Output: disc.df - a data frame that includes:
#             input - the original data in the original order
#             agg.fun - the numeric value for the bucket
#             bucket.name - the range of the bin
# Required: foreach
# Calls: disc.interval(), integer.test()
#------------------------------

eq.width.disc <- function(input, num.bins, agg.fun = mean) {
  require(foreach)

  # Range
  input.range <- max(input) - min(input)
  # Bucket Size
  bucket.size <- input.range / num.bins
  # Find the quotient of the input for the bucket size 
  quotient <- input / bucket.size
  # Find the bucket interval
  bound.df <- as.data.frame(disc.interval(quotient, bucket.size))
  # Create the disc.df
  disc.df <- data.frame("input" = input
                        , "bucket.name" = paste("(", bound.df$lower.bound, ", ", bound.df$upper.bound, "]", sep='')
                        )
  
  # Find a numeric value for each bucket as specified by the aggregate function
  bucket.agg <- aggregate(formula = input~bucket.name, data = disc.df, FUN = agg.fun)
  colnames(bucket.agg)[2] <- "agg.fun"
  # Merge the agg.fun value to the disc.df
  disc.df <- merge(x=disc.df, y = bucket.agg, by.x="bucket.name", by.y="bucket.name", all.x = TRUE, sort = FALSE)
  # Reorder columns
  disc.df <- data.frame("input" = disc.df$input, "agg.fun" = disc.df$agg.fun, "bucket.name" = disc.df$bucket.name)
  return(disc.df)
}


#--------------
# Discretization Interval
# Input: quotient - the quotient to test
#        bucket.size - the length of each discretized interval
# Output: bound.mat - the lower and upper bounds on the interval
# Calls: integer.test()
#-------------------

disc.interval <- function(quotient, bucket.size) {
  require(foreach)
  
  # Preallocate
  upper.quotient <- rep(NA, times = length(quotient))
  lower.quotient <- rep(NA, times = length(quotient))
  upper.bound <- rep(NA, times = length(quotient))
  lower.bound <- rep(NA, times = length(quotient))
  
  
  bound.mat <- foreach(i = 1:length(quotient), .combine = rbind) %dopar% {
    # Check to see if the quotient is an integer
    if (integer.test(quotient[i]) == TRUE) {
      upper.quotient[i] <- quotient[i] # This sets the upper bound as a closed interval
      lower.quotient[i] <- quotient[i] - 1
    } else { 
      upper.quotient[i] <- ceiling(quotient[i])
      lower.quotient[i] <- floor(quotient[i])
      }
    upper.bound[i] <- upper.quotient[i] * bucket.size
    lower.bound[i] <- lower.quotient[i] * bucket.size
    return(c(lower.bound[i], upper.bound[i]))
  }
  colnames(bound.mat) <- c("lower.bound", "upper.bound")
  
  return(bound.mat)
}


#------------------
# Integer Test
#   Not to be confused with is.integer()!
#     this tests if "x" is equivalent to an integer, not if the
#     data type is integer
# Input: x - the number to test
#       tol - the tolerance, defaults to floating point precision
# Output: whether or not the number is an integer within tolerance
#---------------------

integer.test <- function(x, tol = .Machine$double.eps) {
 sapply(x, FUN = function(x) {
   if ( abs(x - round(x)) < tol | abs(round(x) - x) < tol) { return(TRUE)
    } else { return(FALSE) } 
   }
  )
}






