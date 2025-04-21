x <- c(0,1,2,3,4,5)
y <- c(0,1,60,40,41,47)

(result1 <- lm(y~x))
(result2 <- lm(y~x + I(x^2)))
(result3 <- lm(y~x + I(x^2) + I(x^3)))
(result4 <- lm(y~x + I(x^2) + I(x^3) + I(x^4)))
(result5 <- lm(y~x + I(x^2) + I(x^3) + I(x^4) + I(x^5) ))

y1 <- result1[[1]][2]*x   + result1[[1]][1]
y2 <- result2[[1]][3]*x^2 + result2[[1]][2]*x   + result2[[1]][1]
y3 <- result3[[1]][4]*x^3 + result3[[1]][3]*x^2 + result3[[1]][2]*x   + result3[[1]][1]
y4 <- result4[[1]][5]*x^4 + result4[[1]][4]*x^3 + result4[[1]][3]*x^2 + result4[[1]][2]*x   + result4[[1]][1]
y5 <- result5[[1]][6]*x^5 + result5[[1]][5]*x^4 + result5[[1]][4]*x^3 + result5[[1]][3]*x^2 + result5[[1]][2]*x + result4[[1]][1]

(J1 <- sum( (y1-y)^2))
(J2 <- sum( (y2-y)^2))
(J3 <- sum( (y3-y)^2))
(J4 <- sum( (y4-y)^2))
(J5 <- sum( (y5-y)^2))

x <- seq(0,5,0.01)
y1 <- result1[[1]][2]*x   + result1[[1]][1]
y2 <- result2[[1]][3]*x^2 + result2[[1]][2]*x   + result2[[1]][1]
y3 <- result3[[1]][4]*x^3 + result3[[1]][3]*x^2 + result3[[1]][2]*x   + result3[[1]][1]
y4 <- result4[[1]][5]*x^4 + result4[[1]][4]*x^3 + result4[[1]][3]*x^2 + result4[[1]][2]*x   + result4[[1]][1]
y5 <- result5[[1]][6]*x^5 + result5[[1]][5]*x^4 + result5[[1]][4]*x^3 + result5[[1]][3]*x^2 + result5[[1]][2]*x + result4[[1]][1]



x0 <- c(0,1,2,3,4,5)
y0 <- c(0,1,60,40,41,47)

par(mfrow=c(2,2))

plot(x0,y0,xlab="x",ylab="y",main="First Degree")
points(x,y1,type="l")

plot(x0,y0,xlab="x",ylab="y",main="Second Degree")
points(x,y2,type="l")

plot(x0,y0,xlab="x",ylab="y",main="Third Degree")
points(x,y3,type="l")

plot(x0,y0,xlab="x",ylab="y",main="Fourth Degree")
points(x,y4,type="l")

par(mfrow=c(1,1))
plot(x0,y0,xlab="x",ylab="y",main="Fifth Degree",ylim=c(-40,80) )
points(x,y5,type="l")

JVector = c (J1, J2, J3, J4, J5 )
ModelDegreePoly = c (1,2,3,4,5)
plot(ModelDegreePoly,JVector,type='l')

