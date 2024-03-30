#R Code - unexecuted

# ---------- HW1: Intro -----------

#Define the following vectors:
height <- c(59,60,61,58,67,72,70)
weight <- c(150,140,180,220,160,140,130)

#Define a variable:
a <- 150

#Step 1: Calculating means
  #1) Compute, using R, the avera?e height (called mean in R)
  mean(height)
  #2) Compute, using R, the average weight (called mean in R)
  mean(weight)
  #3) Calculate the length of the vector 'height' and 'weight'
  length(height)
  length(weight)
  #4) Calculate the sum of the heights
? sum(height)
  #5) Compute the average of both height and weight. How does it compare to mean function?
  avgHeight <- sum(height)/length(height)
  avgHeight
  avgWeight <- sum(weight)/length(weight)
  avgWeight

  #The values for avgHeight and avgWeight a?e the same as their mean counterpart (mean(height) and mean(weight))

#Step 2: Using max/min functions
  #6) Compute the max height, store the result in 'maxH'
  maxH <- max(height)
  maxH
  #7) Compute the min weight, store the result in 'minW'
  minW <- ?in(weight)
  minW

#Step 3: Vector Math
  #8) Create a new vector, which the weight + 5 (every person gained 5lbs)
  weightPlus5 <- weight + 5
  #9) Compute the weight/height for each person, using the new weight just created
  weightPlus5
  height

#Step ?: Using Conditional if statements
  #10) Write the R code to test if max height is greater than 60 (output "yes or "no")
  if(max(height) > 60) "yes" else "no"
  #11) Write the R code to if min weight is greater than the variable 'a' (output "yes" or "no")?  if(min(weight) > a) "yes" else "no"
  