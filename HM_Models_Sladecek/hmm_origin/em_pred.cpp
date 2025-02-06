#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
Rcpp::List em_pred(Rcpp::NumericMatrix dens_mat,
                   Rcpp::NumericVector deltas,
                   Rcpp::NumericMatrix tpm) {
  int nstates = dens_mat.ncol();
  int N = dens_mat.nrow();
  Rcpp::NumericVector scales(N);
  Rcpp::NumericVector scales_beta(N);
  Rcpp::NumericVector scales_xis(N-1);
  Rcpp::NumericMatrix alphas(N,nstates);
  Rcpp::NumericMatrix betas(N,nstates);
  scales[0] = 0;
  scales_beta[N-1] = 2;
  scales_xis[0] = 0;
  
  // pocatecni hodnoty
  for (int state = 0; state < nstates; state++){
    alphas(0,state) = deltas[state] * dens_mat(0,state);
    betas(N-1,state) = 1;
    scales[0] = scales[0] + alphas(0,state);
  }
  
  for (int state = 0; state < nstates; state++){
    alphas(0,state) = alphas(0,state) / scales[0];
  }
  
  // forward procedura, zaciname v druhem radku (t = 1)
  for (int t = 1; t < N; t++){
    for (int j = 0; j < nstates; j++){
      for (int i = 0; i < nstates; i++){
        alphas(t,j) = alphas(t,j) + alphas(t-1,i) * tpm(i,j) * dens_mat(t,j);
      }
      scales[t] = scales[t] + alphas(t,j);
    }
    for (int state = 0; state < nstates; state++){
      alphas(t,state) = alphas(t,state) / scales[t];
    }
  }
  
  // backward procedura
  for (int t = (N-2); t >= 0; t--){
    for (int j = 0; j < nstates; j++){
      for (int i = 0; i < nstates; i++){
        betas(t,j) = betas(t,j) + betas((t+1),i) * dens_mat((t+1),i) * tpm(j,i);
      }
      scales_beta[t] = scales_beta[t] + betas(t,j);
    }
    for (int state = 0; state < nstates; state++){
      betas(t,state) = betas(t,state) / scales_beta[t];
    }
  }
  
  // vystup (tpm pro kazdy krok)
  Rcpp::NumericMatrix ptrans(nstates,nstates);
  Rcpp::List xis(N-1);
  for (int t = 0; t < (N-1); t++){
    for (int j = 0; j < nstates; j++){
      for (int i = 0; i < nstates; i++){
        ptrans(j,i) = alphas(t,j) * tpm(j,i) * dens_mat(t+1,i) * betas(t+1,i);
        scales_xis[t] = scales_xis[t] + ptrans(j,i);
      }
    }
    for (int j = 0; j < nstates; j++){
      for (int i = 0; i < nstates; i++){
        ptrans(j,i) = ptrans(j,i) / scales_xis[t];
      }
    }
  xis[t] = clone(ptrans);
  }    
  Rcpp::List output;
  output["xis"] = xis;
  output["scales"] = scales;
  return(output);  
}

