viterbi4hmm=function (x,n_states=2,tpm,means,covar,delta=NULL,...){
  if(is.null(delta)){
    delta = solve(t(diag(n_states)-tpm + 1),rep(1,n_states))
  }
  n_obs=dim(x$data)[1]
  all_dens=hmm_dens(x,means,covar,nstates=n_states)# tady nedavat hmm, ale "x" obecneji...
  
  ### all_dens, ktery se rovnaj 0 nastavit na nejakou minimalni hodnotu!
  all_dens[all_dens==0|is.na(all_dens)] = min(all_dens[!is.na(all_dens) & all_dens>0])
  
  rel_probs = matrix(0,n_obs,n_states)
  start_dens = delta*all_dens[1,] # hustoty pro zacatek retezce
  rel_probs[1,]=start_dens/sum(start_dens)# relativni pravdepodobnost jednotlivych hustot
  for (i in 2:n_obs) {
    dens=apply(rel_probs[i-1,]*tpm, 2, max)*all_dens[i,]# 2, ze to jde po sloupcich
    if(sum(dens)==0){
      rel_probs[i,]=1/n_states
    } else {
      rel_probs[i,]=dens/sum(dens)
    }
  }
  
  pred_state=numeric(n_obs)
  pred_state[n_obs]=which.max(rel_probs[n_obs,])
  
  for (i in (n_obs-1):1) {
    pred_state[i] = which.max(tpm[,pred_state[i+1]]*rel_probs[i,])
  }
  return(pred_state)
}


viterbi4hmmf=function(x,n_states=2,tpm,means,covar,delta=NULL,independent.seq=NULL,...){
  if(is.null(independent.seq)){
    predstate=viterbi4hmm(x = x,
                          n_states = n_states,
                          tpm = tpm,
                          means = means,
                          covar = covar,
                          delta = delta)
  } else {
  predstate={}
  for (sekvence in unique(independent.seq)) {
    # jak resit deltu?
  
   #delta=apply(m$model$delta,2,mean)
    
    hmm2=x
    hmm2$data=hmm2$data[independent.seq==sekvence,]
    predstate_sekv=viterbi4hmm(x = hmm2, 
                          n_states = n_states, 
                          tpm = tpm, 
                          means = means, 
                          covar = covar, 
                          delta = delta)
    
    predstate=c(predstate,predstate_sekv)
  }
  }
  return(predstate)
  }