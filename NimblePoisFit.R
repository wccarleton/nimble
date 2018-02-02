PoisFitCode <- nimbleCode({
   a ~ dunif(0.0001,100)
   b ~ dunif(0.0001,100)
   lambda <- a/b
   for(k in 1:K){
      lambda_hat[k] ~ dgamma(a,b)
      for (j in 1:J[k]){
         x[j,k] ~ dpois(lambda_hat[k])
      }
   }
})

x <- sapply(1:10,function(m){rpois(n=rpois(1,1000),lambda=2)})
x <- do.call(cbind.na,x)
J <- apply(x,2,function(m){length(m[which(!is.na(m))])})


PoisFitConsts <- list(K=dim(x)[2],J=J)

PoisFitInits <- list(a=5,
                     b=5)

PoisFitData <- list(x=x)

PoisFitModel <- nimbleModel(code=PoisFitCode,
                        data=PoisFitData,
                        inits=PoisFitInits,
                        constants=PoisFitConsts)

#compile nimble model to C++ code—much faster runtime
C_PoisFitModel <- compileNimble(PoisFitModel, showCompilerOutput = FALSE)

#configure the MCMC
PoisFitModel_conf <- configureMCMC(PoisFitModel,print=F)

PoisFitModel_conf$addMonitors(c("lambda","lambda_hat"))

PoisFitModelMCMC <- buildMCMC(PoisFitModel_conf,thin=10)

C_PoisFitModelMCMC <- compileNimble(PoisFitModelMCMC,project=PoisFitModel)

niter=10000

set.seed(1)

C_PoisFitModelMCMC$run(niter)

samples <- as.matrix(C_PoisFitModelMCMC$mvSamples)
