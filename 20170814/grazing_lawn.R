gl <- read.csv("grazing_lawn.csv")
par(mfrow=c(2,2)) #make sure panel is big enough

plot(gl$In_out, gl$N, xlab="Location", ylab="% N")
plot(gl$In_out, gl$P, xlab="Location", ylab="% P")
plot(gl$In_out, gl$LS, xlab="Location", ylab="L:S Ratio")
plot(gl$In_out, gl$G, xlab="Location", ylab="Grazing Intensity")