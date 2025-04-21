#x = seq(0,6.5,0.5)
#xPredict = seq(0,6.5,0.1)

#(n = length(xPredict))
#set.seed(98103)
#epsilon <- rnorm(n,0,1)

#y = sin(4*xPredict)
#plot(xPredict,y,type='l')
#y1 = y + epsilon*0.2
#plot(xPredict,y1,ylim=c(-2,2))

####################################################

x = seq(0,6.5,0.5)
xPredict = seq(0,6.5,0.1)

(n = length(x))
set.seed(98103)
epsilon <- rnorm(n,0,1)

y = sin(4*x)
plot(x,y,type='l')
y1 = y + epsilon*0.2
plot(x,y1,ylim=c(-2,2))


#########################################

#result = lm(y1~x)
result = lm(y1 ~ poly(x,1,raw=TRUE))
summary(result)
p = predict(result,list(x=xPredict))
lines(xPredict,p,col='black',lwd=3)

########################################

#result = lm(y1 ~ x + I(x^2))
result = lm(y1 ~ poly(x,2,raw=TRUE))
summary(result)
p = predict(result,list(x=xPredict))
lines(xPredict,p,col='red',lwd=3)

#########################################

result = lm(y1 ~ poly(x,4,raw=TRUE))
summary(result)
p = predict(result,list(x=xPredict))
lines(xPredict,p,col='green',lwd=3)

##########################################

result = lm(y1 ~ poly(x,8,raw=TRUE))
summary(result)
p = predict(result,list(x=xPredict))
lines(xPredict,p,col='navy',lwd=3)

########################################

result = lm(y1 ~ poly(x,10,raw=TRUE))
summary(result)
p = predict(result,list(x=xPredict))
lines(xPredict,p,col='firebrick1',lwd=3)

#########################################

result = lm(y1 ~ poly(x,16,raw=TRUE))
summary(result)
p = predict(result,list(x=xPredict))
lines(xPredict,p,col='brown4',lwd=3)


