price = c(49, 69, 89, 99, 109)
demand = c(124, 95, 71, 45, 18)
plot(price,demand)

#################################################
Oneprice = c(1,1,1,1,1,price)
Xprice = matrix(Oneprice,nrow=5)
Xprice

##############################################
demand = c(124, 95, 71, 45, 18)
Ydemand = matrix(demand,nrow=5)
Ydemand

###############################################
z1 = t(Xprice)%*%Xprice
z1
# comput the inverse of z
det(z1)
invz1 = solve(z1)

############################################
z2 = t(Xprice)%*%Ydemand
z2
##########################################
ans = invz1 %*% z2
ans

#######################################
summary(lm(demand~price))
