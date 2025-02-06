# library(Rcpp)
# sourceCpp("em_pred.cpp")
em4hmm=function (hmm,nstates=2,nrep=3,tolerance=2) {
  means=sapply(X = hmm$means,colMeans)
  covar=array(data = unlist(lapply(hmm$covariances,"[",1:length(hmm$predictors),1:length(hmm$predictors))),
              dim = c(length(hmm$predictors),
                      length(hmm$predictors),
                      nstates))  
  tpm=hmm$tpm[hmm$tpm[,"Seq"]==1,-ncol(hmm$tpm)]
  if(length(unique(hmm$sequences))==1){
  deltas=hmm$delta
  } else {
  deltas=colMeans(hmm$delta)
  }
  output={}
  run=1
  crit=tolerance+1
  npar=length(deltas)+nstates*length(hmm$predictors)+nstates*(nstates-1)+# upraveny k pro nezavisly parametry (v ramci tpm jsou nektery hodnoty dopocitat, neobot se v radku doplnuji do jedne)
      nstates*((((length(hmm$predictors)**2)-length(hmm$predictors))/2)+length(hmm$predictors))
  if(any(means==0)){stop("One of means converge to 0!")}
  while(crit>tolerance && run<=nrep){
    dens_mat=hmm_dens(hmm=hmm,means=means,covar=covar,nstates=nstates)
    ### dens_mat, ktery se rovnaj 0 nastavit na nejakou minimalni hodnotu!
    #dens_mat[dens_mat==0] = min(dens_mat[dens_mat>0])
    dens_mat[dens_mat< 1e-307] = 1e-307
    #pred=em_predict(hmm = hmm,means = means,covar = covar,nstates = nstates,deltas = deltas,tpm=tpm)
    pred=em_pred(dens_mat=dens_mat,deltas = deltas,tpm=tpm)
    pred[["scales"]]=1/pred[["scales"]]
    xis=array(data = unlist(pred$xis),dim=c(nstates,nstates,length(pred$xis)))
    rowSums(xis,dims = 1)
    gammas2=t(sapply(pred[["xis"]],colSums))
    results=c(logLik=-sum(log(pred[["scales"]])),AIC=-2*(-sum(log(pred[["scales"]])))+2*npar)
      deltas = gammas2[1,]
      tpm=rowSums(xis,dims = 2)/rowSums(xis,dims = 1)
      means=t(t(t(hmm$data[-nrow(hmm$data),as.character(hmm$predictors)])%*%gammas2)/
        colSums(gammas2))
      
      covar=array(data = matrix(nrow=length(hmm$predictors),ncol=length(hmm$predictors)),dim=c(length(hmm$predictors),
                      length(hmm$predictors),nstates))
      for(i in 1:nstates){
        dev=(t(t(hmm$data[-nrow(hmm$data),as.character(hmm$predictors)])-means[,i]))
         for(j in 1:length(hmm$predictors)){
          for(k in 1:length(hmm$predictors)){
            covar[k,j,i]=sum(gammas2[, i] * (dev[,j]*dev[,k]))/sum(gammas2[,i])
          }
        }
      }
      output[[run]]=list(results=results,deltas=deltas,tpm=tpm,means=means,vcov=covar)
      if(run>1){
      crit=abs(results[2]-output[[run-1]][["results"]][2])
      }
      run=run+1
  }
  #output[["summary"]]=matrix(unlist(lapply(output,"[",1)),ncol=2,byrow=T)
  class(output)="emmodel"    
  return(output)
}

summary.emmodel=function(object,...){
  cat("Nazdar smrade!\n")
  cat("\nConvergence:\n")
  print(matrix(unlist(lapply(object,"[",1)),ncol=2,byrow=T))
}
