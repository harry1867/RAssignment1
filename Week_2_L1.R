# BINF 6970
# Week 2 L1 - Properties of Vectors and Matrices


# we willl be using Trees dataset:a data frame with 31 observations on 3 variables.
help(trees)
data(trees)
plot(trees)
f1 = lm(Volume~Girth+Height,data=trees)
summary(f1)
model.matrix(f1)
X <- model.matrix(f1)

# the quickest way to transpose a matrix in R: use the t() function
#The t() function is part of base R and provides an immediate way to transpose a matrix.
xtx <- t(X)%*%X
xtx

xtxalt <- X%*%t(X)

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

