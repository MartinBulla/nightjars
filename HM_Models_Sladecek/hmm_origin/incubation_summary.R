
incub_summary=function(data,save=F){
data[is.na(data)]=5
data=c(data,5)
x=rep(NA,length(data))
x=cumsum(c(0,abs(diff(data)))) 
x1=cumsum(rep(1,times=length(data))) - 
    rep(c(0,which(!!abs(diff(data)))[-length(which(!!abs(diff(data))))]),
        times=c(which(!!abs(diff(data)))[1],diff(which(!!abs(diff(data))))))
gaps=tapply(x1[data==0],x[data==0],max)

bouts=tapply(x1[data==1],x[data==1],max)
if(save==F){
cat(paste("\n","Overall incubation attendance:",sep=" ","\n"))
print(sum(bouts)/(sum(gaps)+sum(bouts)))
cat(paste("\n","N bouts:",sep=" ","\n"))
print(length(bouts))
cat(paste("\n","N gaps:",sep=" ","\n"))
print(length(gaps))
cat(paste("\n","Length of incubation gaps:",sep=" ","\n"))
print(summary(gaps)) 
cat(paste("\n","Length of incubation bouts:",sep=" ","\n"))
print(summary(bouts))  
} else {
  x=c(sum(bouts)/(sum(gaps)+sum(bouts)),
            length(bouts),length(gaps),
            summary(bouts),summary(gaps))
  return(x)       
} 
}
