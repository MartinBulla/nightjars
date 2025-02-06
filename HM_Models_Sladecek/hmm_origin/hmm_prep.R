# TO DO umoznit jiny (manualni) zadani parametru - pokud nebude k dispozici trenovaci dataset.
# mohlo by jit mabualne/prevodem z jinyho modelu (fce by vzala jinej model a vykuchala by z nej 
# parametry v pozadovanem formatu)

hmm_prep = function(formula, data, loo = F,...) {
  # convert formula
  if (missing(formula)) {
    stop("You have to provide a formula!")
  } else if (class(formula) != "formula") {
    formula = as.formula(formula)
  }
  
  # get variables from formula
  vars = all.vars(formula)
  if (missing(data)) {
    data = get(vars[1])
    response = data[, vars[2]]
    predictors = vars[3:length(vars)]
  } else {
    response = data[, vars[1]]
    predictors = vars[2:length(vars)]
  }
  if (any(is.na(response))){
    warning("Response variable include NA's")
  }
  no_NA=which(!is.na(response))
  response=response[no_NA]
  
  # check existence of variables
  for (i in 1:(length(predictors))) {
    if (!exists(x = predictors[i], where = data)) {
      stop(paste("Variable", predictors[i], "does not exist!"))
    }
  }
  # prompt for sequences, if needed
  datt=data[no_NA,]
  if (loo==T){
    # sequences are in the form of ellipsis, because otherwise it would have to be set even if loo==F
    if(!missing(...)){
      argsExtra <- list(...)
      if(any(names(argsExtra)=="sequences")){
      sequences=as.character(substitute(argsExtra))[names(argsExtra)=="sequences"]
      } else {
        sequences=readline(prompt = "Which variable contains sequences?")
      }
    } else {
    sequences=readline(prompt = "Which variable contains sequences?")
    }
    if (!exists(x = sequences, where = data)) {
      stop(paste("Variable", sequences, "is not contained in your dataset!"))
    }
    sequences=as.numeric(as.factor(get(x = sequences, pos = data[no_NA,])))
    ## estimates for loot ##
    # support variables
    states=unique(response)[order(unique(response))]
    deltas={}
    means=array(data = 0,dim = c(length(unique(sequences)),length(predictors),length(states)),
                dimnames = list(sequences=unique(sequences),predictors=predictors,states=states))
    covs=array(data = 0,dim = c(length(predictors)*length(unique(sequences)),
                                length(predictors)+1,
                                length(states)),
               dimnames = list(sequences=rep(unique(sequences),each=length(predictors)),
                               predictors=c(predictors,"Seq"),
                               states=states))
    sums=matrix(data = 0, ncol=length(states),nrow=length(states))
    tpm=matrix(data = 0, ncol=length(states),nrow=length(unique(sequences))*length(states))
    
    
    # training
    
    for (Seq in unique(sequences)) {
      dat=datt[sequences != Seq,]
      response_=response[sequences != Seq]
      
      # delta
      deltas=rbind(deltas,table(response[sequences != Seq])/
                             (length(sequences[sequences != Seq])))
      # means and covs
      for (state in states) {
        dat2=dat[response_ == state,]
        if(length(predictors)==1){
        means[unique(sequences)==Seq,,states==state]=mean(dat2[,predictors])
        covs[rep(unique(sequences),each=length(predictors))==Seq,,states==state]=cbind(var(dat2[,predictors]),rep(Seq,length(predictors)))

        } else{
        means[unique(sequences)==Seq,,states==state]=colMeans(dat2[,predictors])
        covs[rep(unique(sequences),each=length(predictors))==Seq,,states==state]=cbind(cov(dat2[,predictors]),rep(Seq,length(predictors)))
        }
      }
      # tpm
      sums_=matrix(data = 0, ncol=length(states),nrow=length(states))
      for (seq in unique(sequences[sequences!=Seq])){
        response__=response[sequences == seq]
        trans=c(-999,as.character(response__[1:length(response__)-1]))
        for (i in 1:length(states)) {
          for(j in 1:length(states)) {
          sums[i,j]=length(response__[response__==states[i] & trans==states[j]])
          }
        }
        sums_=sums_+sums
      }
        tpm[rep(unique(sequences),each=length(states))==Seq,]=sums_/rowSums(sums_)
      }
  } else {
    # estimation without LOO
    sequences=1
    states=unique(response)[order(unique(response))]
    # deltas
    deltas=table(response)/length(response)
    # support vars
    means=array(data = 0,dim = c(1,length(predictors),length(states)),
                dimnames = list(sequences=1,predictors=predictors,states=states))
    covs=array(data = 0,dim = c(length(predictors),
                                length(predictors)+1,
                                length(states)),
               dimnames = list(sequences=rep(1,each=length(predictors)),
                               predictors=c(predictors,"Seq"),
                               states=states))
    sums=matrix(data = 0, ncol=length(states),nrow=length(states))
    tpm=matrix(data = 0, ncol=length(states),nrow=length(states))
    # means and covs
    for (state in states) {
      dat2=datt[response == state,]
      if(length(predictors)==1){
        means[,,states==state]=mean(dat2[,predictors])
        covs[,,states==state]=cbind(var(dat2[,predictors]),rep(1,length(predictors)))
        
      } else{
        means[,,states==state]=colMeans(dat2[,predictors])
        covs[,,states==state]=cbind(cov(dat2[,predictors]),rep(1,length(predictors)))
      }
    }
    # tpm
    trans=c(-999,as.character(response[1:length(response)-1]))
      for (i in 1:length(states)) {
        for(j in 1:length(states)) {
          sums[i,j]=length(response[response==states[i] & trans==states[j]])
        }
      }
    tpm=sums/rowSums(sums)
  }
  # Output
  means=setNames(lapply(split(means, arrayInd(seq_along(means), dim(means))[, 3]),
                  array, dim = dim(means)[-3], dimnames(means)[-3]),
           dimnames(means)[[3]])
  
  covs=setNames(lapply(split(covs, arrayInd(seq_along(covs), dim(covs))[, 3]),
                  array, dim = dim(covs)[-3], dimnames(covs)[-3]),
           dimnames(covs)[[3]])
  tpm=cbind(tpm,Seq=rep(unique(sequences),each=length(states)))
  output=list(delta=deltas, means=means,covariances=covs,tpm=tpm,
              formula=formula,data=data,predictors=predictors,sequences=sequences,
              states=states)
  class(output)="hmmodel"
  return(output)
}    
summary.hmmodel=function(object,...){
  cat("Nazdar smrade!\n")
  cat("\nTransition probability matrix:")
  print(object$tpm)
}
