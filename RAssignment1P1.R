# Assignment 1 - Part 1

# Read in the design matrix from CSV file (as a dataframe)
PuromycinDesignDF <- read.csv("model.matrix.csv")
print(PuromycinDesignDF)
# convert data frame to matrix
PuromycinDesignMatrix <- as.matrix(PuromycinDesignDF)

data(Puromycin)
force(Puromycin)



class(PuromycinDesignMatrix) # verify it's a matrix
str(PuromycinDesignMatrix) # check structure
head(PuromycinDesignMatrix) # view first few rows

# Calculate XtX to get a square matrix
XtX <- t(PuromycinDesignMatrix) %*% PuromycinDesignMatrix

# Compute eigenvalues and eigenvectors of XtX
eigen_result <- eigen(XtX)

# Extract eigenvalues and eigenvectors
eigenvalues <- eigen_result$values
eigenvectors <- eigen_result$vectors
# Print eigenvalues and eigenvectors
class(eigenvalues)
str(eigenvalues)
print(eigenvalues)
class(eigenvectors)
str(eigenvectors)
print(eigenvectors)
# Verify one of the eigenvectors
# Take the first eigenvector and corresponding eigenvalue
test_eigenvector <- eigenvectors[, 1]
test_eigenvalue <- eigenvalues[1]

# Normalize the eigenvector
normalized_eigenvector <- test_eigenvector / sqrt(sum(test_eigenvector^2))

# Check if XtX times the normalized eigenvector equals the eigenvalue times the normalized eigenvector
check_result <- XtX %*% normalized_eigenvector
expected_result <- test_eigenvalue * normalized_eigenvector
is_eigenvector <- all.equal(as.numeric(check_result), as.numeric(expected_result))
# Print verification result
print(is_eigenvector)

# Demonstrate the eigenvector property
print(PuromycinDesignMatrix %*% eigenvalues)[, 1]
print(eigenvalues[1] * eigenvectors)[, 1]


# Calculate the condition number - indicating multi colinearity
condition_number <- sqrt(max(eigenvalues) / min(eigenvalues))
print(condition_number)
# This results in NaN - indicating that one of the eigenvalues is zero (or very close to zero), which suggests perfect multicollinearity in the design matrix.

# Print the minimum and maximum eigenvalues
print(min(eigenvalues))
print(max(eigenvalues))
# An eigenvalue of e-15 (0.000000000000001) is essentially zero due to numerical precision limits in computers. This indicates perfect or near-perfect multicollinearity in your design matrix.

# Proportion of variance explained by each component
prop_variance <- eigenvalues / sum(eigenvalues)
cumsum(prop_variance)  # Cumulative proportion








f1 = lm(Volume~Girth+Height,data=trees)
summary(f1)
model.matrix(f1)
X <- model.matrix(f1)

# the quickest way to transpose a matrix in R: use the t() function
#The t() function is part of base R and provides an immediate way to transpose a matrix.
xtx <- t(X)%*%X
xtx

#generally, solve () allows us to find the values of unknown variables 
#that satisfy multiple equations simultaneously.
#By solving systems of equations, we can determine the relationships between variables and make informed decisions based on the solutions obtained.
#here we will use the solve() function in R to solve linear algebraic equations of the form “A*x = b”, where “A” is the coefficient matrix, “x” is the vector or matrix of unknown variables, and “b” is the vector or matrix of constants.

solve(xtx)

round(xtx%*%solve(xtx))	## Identity matrix - why?

ftd1 <- f1$fitted.values

str(f1$model$Volume)

ftd2 <- X%*%solve(xtx)%*%t(X)%*%f1$model$Volume

str(ftd2)

str(ftd1)


sum(ftd1-ftd2) ## zero – almost!

#And the "hat" matrix ...

H <- X%*%solve(xtx)%*%t(X)
sum(H-t(H))  ## symmetry 
##----
sum(H%*%H-H)  ## property name?

##----
range(H%*%X-X) #3 !
### Residuals ...

resid1 <- f1$residuals

dim(X)[1]


resid2 <- (diag(rep(1,dim(X)[1])) - H ) %*% f1$model$Volume
sum(resid1-resid2)

##----
sum(resid1)

##----
t(f1$residuals)%*%f1$fitted.values

##----
round(t(f1$residuals)%*%X)


### The Estimated Coefficients

coef(f1) # Intercept

solve(xtx)%*%t(X)%*%f1$model$Volume  ## beta_hat!

#

#eigenvalues and eigenvectors
# Create square matrix:
X <- matrix( c(2, 4, 3, 2), nrow=2, ncol=2, byrow = TRUE)


eigen_results <- eigen(X)
eigenvalues <- eigen_results$values
eigenvectors <- eigen_results$vectors

print(eigenvalues)
print(eigenvectors)

#How to verify one of the eigenvectors is an eigenvector of the data matrix

#For matrix X, taking the first eigenvector and the corresponding eigenvalue
test_eigenvector <- eigenvectors[, 1] 
test_eigenvalue <- eigenvalues[1]  

#Normalize the eigenvector
normalized_eigenvector <- test_eigenvector / sqrt(sum(test_eigenvector^2))

#Checking if X times the normalized eigenvector equals the eigenvalue times the normalized eigenvector - formula: Xv = λv
check_result <- X %*% normalized_eigenvector
expected_result <- test_eigenvalue * normalized_eigenvector

#Use all.equal for comparison (which accounts for floating-point arithmetic inaccuracies)
is2_eigenvector <- all.equal(as.numeric(check_result), as.numeric(expected_result))

#Print the verification result
print(is2_eigenvector)

