#-----------
# SQL Translation
#------------

# Data
#--------

x1 <- runif(50, min = -10, max = 10)
x2 <- runif(50, min = -10, max = 10)
y <- x1 + x2 + rnorm(50, mean=0, sd = 3)
y2 <- rbinom(50, size = 1, prob = 0.5)

df <- data.frame(x1, x2, y)
df2 <- data.frame(x1, x2, y2)


x3 <- c(rep("Really Long Variable Name Here", times = 40), rep("Shorter Different Variable Name", times = 10))
x3 <- factor(x3)
y3 <- factor(c(rep(0, times = 40), rep(1, times = 10)))
df3 <- data.frame(x1, x3, y3)

# LM
#-----------

lm1 <- lm(y~., data=df)

summary(lm1)$coefficients

coeff <- summary(lm1)$coefficients[, "Estimate"]
var.names <- rownames(summary(lm1)$coefficients)

sql.var <- paste(coeff[-1], "*" ,var.names[-1], "+", collapse = " ")
sql.statement <- paste("SELECT", sql.var, coeff[1])


# GLM Log Reg
#------------


glm1 <- glm(y2~., data=df2, family = "binomial")
summary(glm1)$coefficients

coeff <- summary(glm1)$coefficients[, "Estimate"]
var.names <- rownames(summary(glm1)$coefficients)

sql.var <- paste(coeff[-1], "*" ,var.names[-1], "+", collapse = " ")
sql.statement <- paste("SELECT 1 / (1 + exp(", sql.var, coeff[1], ")")

 
# Decision Tree
#---------------
library(rpart)

# 2 parts
# 1) Parse out variable name, sign, variable value, and output value
# 2) Build the tree

tree <- rpart(y2 ~., df2)
tree2 <- rpart(y3~., data = df3, method="class")

#---------------------------------
# Tree Parse Function
# Input: dtree - a decision tree from rpart()
# Output: the parsed rule set
#-----------------------------------

tree.parse <- function(dtree) {
  # Grab the rule stack
  rule.stack <- capture.output(dtree)
  # Remove the data to the right of the rule
  match.right <- regexpr(pattern="[[:digit:]]+[[:space:]]{2}[[:digit:]].*", text=rule.stack)
  sub.result <- unlist(regmatches(x = rule.stack, m=match.right, invert = TRUE))
  # Remove the rule number and spaces to the left of the rule
  match.left <- regexpr(pattern="[[:space:]]*[[:digit:]]+)[[:space:]]", text = sub.result)
  final.result <- unlist(regmatches(x = sub.result, m=match.left, invert=TRUE))
  final.result <- final.result[final.result != ""][-1:-4]
  
  return(final.result)
}

#--------------
# Node Prediction Function
# Input: dtree - a decision tree from rpart()
# Output: a vector of the probabilities/predictions for each node
#-------------

node.pred <- function(dtree) {
  pred.df <- NA
  
  if (dtree$method == "anova") { pred <- dtree$frame$yval
    } else if (dtree$method == "class") { pred <- dtree$frame$yval2[, 4] }
  
  leaf <- ifelse(dtree$frame$var == "<leaf>", yes=1, no = 0)
  
  pred.df <- data.frame("node" = rownames(dtree$frame), "pred" = pred, "leaf" = leaf)
  
  return(pred.df)
}

#--------------------
# Tree Stack Function
# Input: dtree - a decision tree from rpart()
# Output: a rule stack data frame
# Required: node.pred(), tree.parse()
#------------------

tree.stack <- function(dtree) {
  df <- node.pred(dtree)
  node.rule <- c("root", tree.parse(dtree))
  df <- data.frame(df, "rule" = node.rule)
  
  return(df)
}


#------------------
# Tree to SQL Translation Function
# Input: dtree - a decision tree from rpart()
#        outcome.name - the name for the predicted value
# Output: An ANSI SQL description of the model
# Required: tree.stack(), node.pred(), tree.parse()
#----------------


tree2sql <- function(dtree, outcome.name) {
  
  # Initialize
  stack <- tree.stack(dtree)
  sql.statement <- NULL
  curr.state <- 0
  branch.depth <- 0
  
  # Loop through each rule in the stack
  for (i in 2:dim(stack)[1]) {
    # Setup
    #-----------
    # Current and Next State
    if (i == 2) { curr.state <- 0
      } else { curr.state <- stack[i-1, "leaf"]}
    next.state <- stack[i, "leaf"]
    # Current Rule
    rule.i <- stack[i, "rule"]
    # Tab number
    tabs <- paste(rep("\t", times=branch.depth), collapse='')
    
    # Logic: Translates stack -> tree
    #-------------
    # Node -> Node
    if (curr.state == 0 & next.state == 0) {
      # Write statement
      statement.i <- paste("\n", tabs, "CASE WHEN", rule.i, "THEN")
      sql.statement <- paste(sql.statement, statement.i)
      # Increment branch depth
      branch.depth <- branch.depth + 1
      next()
    }
    # Node -> Leaf Transition
    if (curr.state == 0 & next.state == 1) {
      # Write statement
      pred.i <- stack[i, "pred"]
      statement.i <- paste("\n", tabs, "CASE WHEN", rule.i, "THEN", pred.i)
      sql.statement <- paste(sql.statement, statement.i)
      # Increment branch depth
      branch.depth <- branch.depth + 1
      next()
    }           
    # Leaf -> Node Transition
    if (curr.state == 1 & next.state == 0) {
      # Write statement
      statement.i <- paste("\n", tabs, "WHEN", rule.i, "THEN")
      sql.statement <- paste(sql.statement, statement.i)
      next()
    }
    # Leaf -> Leaf Transition
    if (curr.state == 1 & next.state == 1) {
      # Write statement
      pred.i <- stack[i, "pred"]
      statement.i <- paste("\n", tabs, "WHEN", rule.i, "THEN", pred.i)
      sql.statement <- paste(sql.statement, statement.i)
      # Close 1 open CASE statement
      sql.statement <- paste(sql.statement, "\n", tabs, "END")
      # Decrement the branch.depth
      branch.depth <- branch.depth - 1
      next()
    }
  }
  # END any hanging CASE clauses
  termination <- paste(rep("\nEND", times = branch.depth), collapse='')
  # Throw on the outcomes name
  sql.statement <- paste(sql.statement, termination, "AS", outcome.name)
  return(sql.statement)
}

writeLines(text = sql.statement, con = "C:/Temp/TreeSQL.txt")

cat(sql.statement)


























#-----------------
# SQL Generation Exploration
#---------------

stack <- tree.stack(tree)
outcome.name <- "y2"
sql.statement <- NULL
end.flag <- 0


  print(end.flag)
  rule.i <- stack[i, "rule"]
  leaf.flag <- stack[i, "leaf"]


# LEAF Statement
  if (leaf.flag == 1) {
    pred.i <- stack[i, "pred"]
    statement.i <- paste("CASE WHEN", rule.i, "THEN", outcome.name, "=", pred.i, " END")
  # NODE with no open CASE statements
  } else if (leaf.flag == 0 & end.flag < 0) {
      statement.i <- paste("END \n", "CASE WHEN", rule.i, "THEN")
      end.flag <- end.flag + 1
  # NODE with an open CASE statement
  } else if (leaf.flag == 0 & end.flag >= 0) {
      statement.i <- paste("CASE WHEN", rule.i, "THEN") 
      end.flag <- end.flag - 1
  }

sql.statement <- paste(sql.statement, "\n" ,statement.i, ";", end.flag) # newline for every statement

# Statement if the current rule is for a NODE
  if (leaf.flag == 0) {
    # Check to see if an open CASE statement needs to be closed
    if (end.flag < 0) {
      statement.i <- paste("END \n", "CASE WHEN", rule.i, "THEN")
      end.flag <- end.flag + 1
    } else {
      statement.i <- paste("CASE WHEN", rule.i, "THEN") 
      end.flag <- end.flag - 1
    }
  # Statement if the current rule is for a LEAF
  }
  if (leaf.flag == 1) {
    pred.i <- stack[i, "pred"]
    statement.i <- paste("CASE WHEN", rule.i, "THEN", outcome.name, "=", pred.i, " END")
  }



# The yval can be grabbed from tree$frame$yval - it's in the same order as the tree summary
# Whether the node is terminal or not can be grabbed from tree$frame$var

SELECT # End Flag = 0
      CASE WHEN x2 >= 2.1 THEN    # End Flag = -1           
          CASE WHEN x1 < 0.99 THEN 0 END # -1
          CASE WHEN x1 >= 0.99 THEN 0.777 END # -1
      END
      CASE WHEN x2 < 2.1 THEN
          CASE WHEN x2 < -3.1 THEN 0.466 END
          CASE WHEN x2 >= -3.1 THEN 0.769 END
      END



          


#-----------------
# Parsing Exploration
#-----------------


tree <- rpart(y2 ~., df2)
tree2 <- rpart(y3~., data = df3)

check <- capture.output(tree)
test <- check[7]

check2 <- capture.output(tree2)
test2 <- check2[8]

# First Try
# matches <- regexpr(pattern="x2[><=]+[-[:digit:]\\.]+", text=test)
# regmatches(x = test, m=matches)

# Second Try
# test <- check[19]
# matches <- regexpr(pattern="(x1|x2)[><=[:space:]]+[-[:digit:]\\.]+", text=test)
# regmatches(x = test, m=matches)

# First Working Solution
#---------------
# Get Variable names
var.names <- colnames(df)
# Build the "capture group" OR statement for variable names
capture.group.string <- paste(var.names, "|", sep ='', collapse='')
capture.group.string <- paste("(", substring(capture.group.string, first=1, last=nchar(capture.group.string)-1), ")", sep ='')
# Insert the capture group into the regular expression
d.pattern = paste(capture.group.string, "[><=[:space:]]+[-[:digit:]\\.]+", sep='')
# Apply the regular expression to each element and pull out the match
match <- regexpr(d.pattern, text = check)
rule <- regmatches(check, m = match)



matches <- regexpr(pattern="[[:digit:]]+[[:space:]]{2}[[:digit:]].*", text=test2)
matches <- regexpr(pattern="[[:digit:]]+[[:space:]]{2}[[:digit:]].*", text=test2)
regmatches(x = test2, m=matches, invert = TRUE)

sub.result <- regmatches(x = test2, m=matches, invert = TRUE)[[1]][1]
sub.match <- regexpr(pattern="[[:space:]]*[[:digit:]]+)[[:space:]]", text = sub.result)
final.result <- regmatches(x = sub.result, m=sub.match, invert=TRUE)
paste(unlist(final.result), collapse = '')
