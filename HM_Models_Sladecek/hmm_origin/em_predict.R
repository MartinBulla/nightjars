em_predict=function(hmm,means,covar,deltas,tpm,nstates=2){
dens_mat=hmm_dens(hmm=hmm,means=means,covar=covar,nstates=nstates)

### dens_mat, ktery se rovnaj 0 nastavit na nejakou minimalni hodnotu!
#dens_mat[dens_mat==0] = min(dens_mat[dens_mat>0])
dens_mat[dens_mat< 1e-307] = 1e-307

alphas = matrix(NA, nrow = nrow(hmm$data), ncol = nstates)
alphas[1,]=deltas*dens_mat[1,]
scales=1/sum(alphas[1,])
alphas[1,]=alphas[1,]*scales
## rekurzivni indukce forward procedury
# pro kazde t a kazdy stav nasobim  a) soucet vsechny moznosti, jak se dobrat daneho stavu z libovolneho
# stavu v t-1 (kazda je soucinem P stavu v t-1 a P prechodu na nas stav v t ) a b) densitu rozdeleni pozorovanych
# promennych  pro pozorovani v case t za predpokladu daneho stavu.
for (i in 2:nrow(hmm$data)) {
  alphas[i,]=alphas[(i-1),]%*%tpm*dens_mat[i,]
  scales[i]=1/sum(alphas[i,])
  alphas[i,]=alphas[i,]*scales[i]
}
# backward procedure: Podminena prst pozorovani Yt+1,...YT, za predpokladu ze v case t je stav S
# pravdepodobnost ukonceni konkretni sekvence (1 pro konec) za predpokladu
# ze stav i bude zacinat v case t.
betas = matrix(NA, nrow = nrow(hmm$data), ncol = nstates)
betas[nrow(betas),]=1
for (j in ((nrow(betas)) - 1):1) {
  betas[j,]=colSums(diag((betas[(j+1),]*dens_mat[j+1,]))%*%t(tpm))
  #betas[(j+1),1]*dens_mat[j+1,1]*tpm[1,1]+betas[j+1,2]*dens_mat[j+1,2]*tpm[1,2]
  #betas[(j+1),1]*dens_mat[j+1,1]*tpm[2,1]+betas[j+1,2]*dens_mat[j+1,2]*tpm[2,2]
  #colSums(diag(c(1,2))%*%t(matrix(2:5,nrow=2,ncol=2,byrow=T)))
  betas[j,]=betas[j,]/sum(betas[j,])
}

ptrans= {}
for (i in 1:(nrow(hmm$data)-1)){
  ptrans[[i]]=(alphas[i,]*diag(nstates))%*%tpm*matrix(dens_mat[i+1,]*betas[i+1,],nrow=nstates,ncol=nstates,byrow=T)
}
## Prst jednotlivych pripadu v ramci transition probability matrix
# pro a i,j se jedna o alpha(i)*ai,j*p(o t+1|j)*beta(j)
## v patem sloupci radkove sumy (konstantni pro vsechny radky!)

xis=lapply(ptrans,function (x) x/sum(x))
## P jednotlivych stavu v case t (soucet jednotlivych pripadu davajicich stejny stav)
return(list(xis=xis,scales=scales))
}
