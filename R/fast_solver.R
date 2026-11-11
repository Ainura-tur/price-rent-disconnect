# Fast solver for the model of 04_model_matching.R, used by 04d_purchase_volumes.R.
# From Ratbek Dzhumashev's Section 6.3 code (November 2026). 04d checks it against
# 04's solve_path() before using it.
#
# Exact convolution implementation of the original lower-triangular filters.
# Avoids a dense matrix product; the valuation linear system is unchanged.
solve_path_fast <- function(phi, eps, kappa, eta, chi, J, theta=1,
                            omega=OMEGA,varpi=VARPI,delta_k=DELTA_K) {
  n <- length(phi); j <- 0:(n-1)
  a <- c(0,kappa*eta*(1-kappa)^j[-n])
  b <- ifelse(j>=J,chi*(1-delta_k)^(j-J),0)
  dker <- as.numeric(stats::filter(a-b,1-omega,method='recursive'))*omega
  rtker <- as.numeric(stats::filter(dker,1-theta,method='recursive'))*theta
  idx <- row(matrix(0,n,n))-col(matrix(0,n,n)); L <- idx>=0
  RT <- matrix(0,n,n); RT[L] <- rtker[idx[L]+1]
  B <- varpi*diag(n)+(1-varpi)*RT
  A <- diag(n); A[1:(n-1),] <- A[1:(n-1),]-B[2:n,]
  # A is lower Hessenberg: only one superdiagonal. Elimination therefore
  # updates a single remaining column, rather than a dense trailing matrix.
  diagonal <- diag(A); upper <- A[cbind(1:(n-1),2:n)]; rhs <- eps*phi
  safe <- TRUE
  for (z in 1:(n-1)) {
    if(abs(diagonal[z])<1e-10) {safe <- FALSE; break}
    ix <- (z+1):n
    mult <- A[ix,z]/diagonal[z]
    rhs[ix] <- rhs[ix]-mult*rhs[z]
    A[ix,z+1] <- A[ix,z+1]-mult*upper[z]
    diagonal[z+1] <- A[z+1,z+1]
  }
  if(safe && abs(diagonal[n])>=1e-10) {
    v <- numeric(n); v[n] <- rhs[n]/diagonal[n]
    for(z in (n-1):1) v[z] <- (rhs[z]-upper[z]*v[z+1])/diagonal[z]
  } else {
    A <- diag(n); A[1:(n-1),] <- A[1:(n-1),]-B[2:n,]
    v <- solve(A,eps*phi)
  }
  p <- as.numeric(stats::filter(v,1-theta,method='recursive'))*theta
  s <- as.numeric(stats::filter(c(0,p[-n]),1-kappa,method='recursive'))*kappa*eta
  start <- if(J<n) c(rep(0,J),p[seq_len(n-J)]) else rep(0,n)
  k <- as.numeric(stats::filter(start,1-delta_k,method='recursive'))*chi
  r <- as.numeric(stats::filter(s-k,1-omega,method='recursive'))*omega
  list(p=p,r=r,v=as.numeric(v))
}
