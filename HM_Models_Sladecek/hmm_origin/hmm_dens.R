hmm_dens=function(hmm,means,covar,nstates=2){
  if (!require("mvnfast",character.only = TRUE)) {
    install.packages("mvnfast",dep=TRUE)
  }
  if(length(hmm$predictors)==1){
    dens_mat=matrix(nrow=nrow(hmm$data),ncol=nstates)
    for (state in 1:nstates){
      dens_mat[,state] = dnorm(x = hmm$data[,hmm$predictors[1]], mean = means[state],
                               sd = sqrt(covar[[state]]))
    }
  } else {
    dens_mat=matrix(nrow=nrow(hmm$data),ncol=nstates)
    for (state in 1:nstates) {
      dens_mat[,state]=mvnfast::dmvn(X=as.matrix(hmm$data[,hmm$predictors]),
                                     mu = means[,state],
                                     sigma = covar[,,state])
    }
  }
  return(dens_mat)
}
