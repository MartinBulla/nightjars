# a) odhad parametru - supervised
# 1) loot (loo=T)
# 2) celej dataset (parametry nejsou zadany,formula obsahuje response)
# 3) poskytnuty parametry (parametry jsou zadany)
# b) odhad parametru - unsupervised
# em (em=T a jsou k dispozici initialni parametry)
# c) fit - viterbi (jsou k dispozici parametry)


# function to proceed full HMM, including diagnostics
hmmf=function(formula,data,loo=T,em=T,sequences,nrep=30,full.output=T,independent.seq=NULL,tolerance=2,
              delta_init=NULL,tpm_init=NULL,means_init=NULL,covar_init=NULL){
  ### pro pripad, ze se pojede model na vice nezavislych datasetech (acm), 
  ###, pripadne em po jednotlivych datasetech,...
  ### bude nutny doprogramovat prislusny dalsi moznosti,
    # pridat check jestli formula obsahuje response
    hmm=hmm_prep(formula = formula,data = data,loo = loo,sequences=sequences,
                 delta_init=delta_init,tpm_init=tpm_init,means_init=means_init,covar_init=covar_init)
    if(loo==T){
    means=sapply(X = hmm$means,colMeans)
    covar=array(data = unlist(lapply(hmm$covariances,"[",1:length(hmm$predictors),
                                     1:length(hmm$predictors))),
                dim = c(length(hmm$predictors),
                        length(hmm$predictors),
                        length(hmm$states)))  
    tpm=hmm$tpm[hmm$tpm[,"Seq"]==1,-ncol(hmm$tpm)]
    # obecne, jak resit delty? Dava prumer smysl i pokud jsou sekvence nezavisly?
    if(length(unique(hmm$sequences))==1){
      deltas=hmm$delta
    } else {
      deltas=colMeans(hmm$delta)
    }
    }
    
  
  if (em==T){
    em1=em4hmm(hmm,nstates=length(hmm$states),nrep = nrep)
    
    ### Nastrel konvergencnich hlasek
    n_conv=length(em1)
    if(n_conv==nrep){
      conv_diag=matrix(unlist(lapply(em1,"[",1)),ncol=2,byrow=T)
      if(!all(diff(conv_diag[,2])<0)){
        # prvni moznost, ze loglik monotonne neklesa, ale kolisa.
        warning("The jojo effect detected, improve stationarity of your data!")# netusim jak tomu jinak rikat, neco musime najit
      } else if (abs(diff(conv_diag[,2])[n_conv-1])>2){
        warning(paste0("The convergence was not found with max grad: "),# nevim jak to popsat...
                diff(conv_diag[,2])[n_conv-1],". Try increase the nrep argument.")
      }
    }
  }
    if(em==T){
    predstate=viterbi4hmmf(x = hmm, n_states = length(hmm$states), tpm = em1[[n_conv]]$tpm,
                           means = em1[[n_conv]]$means,covar = em1[[n_conv]]$vcov,
                           independent.seq = independent.seq)
    } else if(loo==T) {
      predstate=viterbi4hmmf(x = hmm, n_states = length(hmm$states), tpm = tpm,
                             means = means,covar = covar,independent.seq = independent.seq)  
    } else {
      predstate=viterbi4hmmf(x = hmm, n_states = length(hmm$states), tpm = hmm$tpm[,-ncol(hmm$tpm)],
                             means = as.numeric(hmm$means),covar = as.numeric(hmm$covar),independent.seq = independent.seq)  
    }
    
  if(full.output==T){
    if(em==F){
      output=list(model=hmm,
                  predstate=predstate)# zatim oboji class hmmf - v summary jsem musel zobecnit pozici predstate
    } else {
      output=list(model=hmm,
                  optim=em1,
                  n_conv=n_conv,
                  predstate=predstate)
    }
    class(output)="hmmf"
    return(output)
  } else {
    # only predstate
    output=list(predstate=predstate)
    class(output)="hmm_predstate"
    return(output)
  }
}

# summary zatim nedava smysl pro pripad, ze nemame testovaci data - asi vymyslet jinou tridu atp...
summary.hmmf=function(output,save=F){
  response=output[[1]]$data[,all.vars(output[[1]]$formula)[1]]# samotna inkubace (tedy primo sezeni na vejcich)
  fitted=output[[length(output)]]-1### co s tou zkurvenou minus 1??? je to univerzalni?
  #residuals=(response-fitted)### ...???
  confmat=xtabs(formula = ~fitted+response)
  if(save==F){
    if(length(m$model$formula)==3){
    cat(paste(m$model$formula[2],m$model$formula[1],m$model$formula[3]))
    } else {
      cat(paste(m$model$formula[1],m$model$formula[2]))
    }
    cat(paste("\n","Relative confusion matrix:",sep=" ","\n"))
    print(round((confmat/sum(confmat)*100),2))
    cat(paste("\n","Overall % explained:",round(sum(diag(confmat/sum(confmat)))*100,2),"%",sep=" ","\n"))
    cat(paste("\n","% detected:",sep=" ","\n"))
    print(round((diag(confmat)/colSums(confmat))*100,2))# kolik inkubace je rozpoznano jako inkubace
    cat(paste("\n","% correctly assigned:",sep=" ","\n"))
    print(round((diag(confmat)/rowSums(confmat))*100,2))# kolik toho, co model tvrdi ze je inkubace je skutecne inkubace
    # 1-false positive ("credibility"/"truthfullness"?)
  } else {
    x=round(c(round(sum(diag(confmat/sum(confmat)))*100,2),
              t(confmat/sum(confmat)),
              round((diag(confmat)/colSums(confmat))*100,2),
              round((diag(confmat)/rowSums(confmat))*100,2)),2)
    names(x)=c("overall %",paste(rep(output[[1]]$states,each=length(output[[1]]$states)),
                                 rep(output[[1]]$states,times=length(output[[1]]$states)),sep="_"),
               paste(output[[1]]$states,"detected",sep="_"),
               paste(output[[1]]$states,"assigned",sep="_"))
    return(x)       
  }
  
}
#summary(output)

summary.hmm_predstate=function(output){
  print(xtabs(~output$predstate))
}
