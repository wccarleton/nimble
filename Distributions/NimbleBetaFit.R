BetafitCode <- nimbleCode({
   a ~ dunif(0.0001,100)
   b ~ dunif(0.0001,100)
   aa ~ dunif(0.0001,100)
   bb ~ dunif(0.0001,100)
   E_a <- a/b
   E_b <- aa/bb
   E <- E_a / (E_a + E_b)
   for(k in 1:K){
      a_hat[k] ~ dgamma(a,b)
      b_hat[k] ~ dgamma(aa,bb)
      E_hat[k] <- a_hat[k] / (a_hat[k] + b_hat[k])
      for (j in 1:J[k]){
         x[j,k] ~ dbeta(a_hat[k],b_hat[k])
      }
   }
})

#x <- sapply(1:10,function(m){rbeta(n=rpois(1,100),shape1=0.5,shape2=0.5)})
#x <- do.call(cbind.na,x)
J <- apply(x,2,function(m){length(m[which(!is.na(m))])})


BetafitConsts <- list(K=dim(x)[2],J=J)

BetafitInits <- list(a=8,
                     b=2,
                     aa=8,
                     bb=2)

BetafitData <- list(x=x)

BetafitModel <- nimbleModel(code=BetafitCode,
                        data=BetafitData,
                        inits=BetafitInits,
                        constants=BetafitConsts)

#compile nimble model to C++ code—much faster runtime
C_BetafitModel <- compileNimble(BetafitModel, showCompilerOutput = FALSE)

#configure the MCMC
BetafitModel_conf <- configureMCMC(BetafitModel,print=F)

BetafitModel_conf$addMonitors(c("E","E_hat"))

BetafitModelMCMC <- buildMCMC(BetafitModel_conf,thin=1)

C_BetafitModelMCMC <- compileNimble(BetafitModelMCMC,project=BetafitModel)

niter=2000

set.seed(1)

C_BetafitModelMCMC$run(niter)

samples <- as.matrix(C_BetafitModelMCMC$mvSamples)
