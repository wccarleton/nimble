par(mfrow=c(5,4),mar=c(1,3,3,1),oma=c(1,1,3,1),family="serif",bg="white")
numcols <- ncol(samples)
samples_set <- samples[1000:nrow(samples),c(1:10)]
jname <- colnames(samples_set)
for(j in 1:ncol(samples_set)){
plot(samples_set[,j],
   type="l",
   xlab="Iteration",
   ylab=jname[j],
   main=paste(jname[j],"Trace",sep=" "))
#
plot(density(samples_set[,j]),
   xlab="Intercept",
   ylab="Density",
   main=paste(jname[j],"Density", sep=" "))
#
tempregions <- HPDregion(samples_set[,j],c(0.95,0.68))$regions
#
polygon(tempregions[[1]],col="lightgrey",border="NA")
polygon(tempregions[[2]],col="grey",border="NA")
}
mtext("MCMC Posterior Densities: \n Polynomial",side=3,line=0,outer=T)
