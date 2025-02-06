### pridat moznost zadani z em/hmm!
### nebo to spis mozna rovnou zakomponovat do tech fci!
### definovat spis jako methods, aby bylo univerzalni zadani fce plotdist i pro hmmf i pro zakladni data
### sjednoceni skal tak, aby se vzdy celej graf vesel...

# pokus o vizualizaci "modelovanych" rozdeleni funkci
plotdist=function(predictor,response,m=NULL,breaks=50,...){
  if(!is.null(m)){
    if(is.null(m$optim)){
      means=sapply(X = m[["model"]]$means,colMeans)
      sds=sapply(m[["model"]]$covariances,"[",1:length(m[["model"]]$predictors),
                 1:length(m[["model"]]$predictors))
    } else {
      means=m[["optim"]][m$n_conv][[1]]$means
      sds=m[["optim"]][m$n_conv][[1]]$vcov[,,]# jak tohle udelat pro vicerozmerny modely??
    }
   
  } else {
    means=tapply(predictor, response, mean)
    sds=tapply(predictor, response, sd)
  }
  
   # find boundaries for the plot
    lower=min(means-3*sds)
    upper=max(means+3*sds)
    peak=max(dnorm(x=means,mean=means,sd=sds))
    
  plot(NA,xlim=c(lower,upper),ylim=c(0,peak),bty="n",
       ylab="density",xlab="value",...)
  # vysku sloupcu dat adjustovat podle peaku!
  cols=sample(c("black","red","green","blue","orange","deeppink3","brown"),
              size=length(unique(response)))
  for (i in 1:length(means)) {
    x=seq(means[i]-3*sds[i],means[i]+3*sds[i],by=sds[i]/30)
    y=dnorm(seq(means[i]-3*sds[i],means[i]+3*sds[i],
                by=sds[i]/30),mean=means[i],sd=sds[i])
    lines(x=x,y=y,col=cols[i])
    d=predictor[response==unique(response)[i]]
    ints=seq(min(d),max(d),length.out = breaks)
    freqs=as.numeric(xtabs(~cut(d,breaks = ints)))
    freqs=(freqs/max(freqs))*peak
    # tady je otazkou, zda skutecne takto relativizovat - pro kazdy stav je maximalni 
    # cetnost stejna!
    for(int in 1:(length(ints)-1)) {
      lines(x=rep(mean(ints[int],ints[int+1]),2),
            y=c(0,freqs[int]),col=cols[i])
    }
    text(x=means[i],y=max(y),labels = unique(response)[i])
  }
  
}

