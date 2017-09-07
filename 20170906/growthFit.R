#fit a ricker function and quadratic equation to the growth data

growth <- read.csv("data/Growth.csv")
plot(growth$Diameter, growth$Growth)

#ricker function
modRicker <- nls(Growth~Diameter*exp(r*(1-(Diameter/k))), 
                 data=growth, 
                 start=list(r=1, k=50))

summary(modRicker)

plot(growth$Diameter, growth$Growth)
lines(x=0:50, y=predict(modRicker, data.frame(Diameter=0:50)))

#quadratic function
modQuad <- nls(Growth~a*Diameter^2+b*Diameter+c,
               data=growth,
               start=list(a=-1,b=0.5,c=1))
summary(modQuad)

plot(growth$Diameter, growth$Growth)
lines(x=0:50, y=predict(modQuad, data.frame(Diameter=0:50)))

