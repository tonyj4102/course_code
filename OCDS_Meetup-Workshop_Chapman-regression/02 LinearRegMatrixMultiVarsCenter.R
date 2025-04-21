#
# Multi Regression
# Center the data
# Solve the regression using Matrix
#
x1 <- c(1,3,4,7,9,9)
x2 <- c(9,9,6,3,1,2)
y <- c(3,5,6,8,7,10)

# Compute the Center numbers
(Cx1 = x1 - mean(x1))
(Cx2 = x2 - mean(x2))
(Cy = y - mean(y))

# Convert the data into matrix
(X <- cbind(Cx1,Cx2))
(Y <- matrix(Cy,nrow=6,ncol=1))

(XtX <- t(X) %*% X)
(xtX.inv <- solve(XtX))

(xtY <- t(X) %*% Y)

(ans = xtX.inv %*% xtY)

summary(lm(Cy~ Cx1 + Cx2))
