# moznosti stacionarizace signalniho procesu: odecteni prumeru,
# ma (moving average) radu 3-5, fft (fast discreete fourier transformation)
ma = function (x,order=1){
  xt=NULL
  for (i in 1:(length(x)-order)){
    xt[i]=mean(x[i:(i+order-1)])
  }
  for (j in (length(x)-order+1):length(x)){
    xt[j]=mean(x[j:length(x)])
  }
return(xt)
}

#x=1:10
#ma(x,order=2)
