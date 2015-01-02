// This is with a hierarchical covariate z.

data {
    int N; int M; int M1; int M2; int M3;
    vector[N] lprice; 
    int<lower=1> count[N];
    real yr[N]; real z[N];
    int pnro[N]; int l1[M]; int l2[M1]; int l3[M2];
}
transformed data {
    matrix[N, 6] X;
    for (i in 1:N) { X[i, 1] <- 1; X[i, 2] <- yr[i]; X[i, 6] <- yr[i]*yr[i]; 
                     X[i, 4] <- z[i]; X[i, 5] <- yr[i]*z[i]; X[i, 6] <- yr[i]*yr[i]*z[i]; }
}
parameters {
    vector[6] mean_beta;
    cholesky_factor_corr[6] LOmega; vector<lower=0>[6] tau;  
    cholesky_factor_corr[6] LOmega1; vector<lower=0>[6] tau1;
    cholesky_factor_corr[6] LOmega2; vector<lower=0>[6] tau2;
    cholesky_factor_corr[6] LOmega3; vector<lower=0>[6] tau3;
    matrix[M, 6] beta; // pnro level
    matrix[M1, 6] beta1; // l1 level
    matrix[M2, 6] beta2; // l2 level
    matrix[M3, 6] beta3; // l3 level
    real<lower=0> sigma;
}
model {
    // FIXME: should use multi-t instead of multinormals?
    // FIXME: should assume log-normal for prices of original transactions (that we don't have)?
    // (Impossible exactly, but see mean and std of lognormal)
    vector[N] imean;
    vector[N] isigma;
    matrix[6, 6] LSigma_beta;
    matrix[6, 6] LSigma_beta1;
    matrix[6, 6] LSigma_beta2;
    matrix[6, 6] LSigma_beta3;
    LSigma_beta <- diag_pre_multiply(tau, LOmega);
    LSigma_beta1 <- diag_pre_multiply(tau, LOmega1);
    LSigma_beta2 <- diag_pre_multiply(tau, LOmega2);
    LSigma_beta3 <- diag_pre_multiply(tau, LOmega3);
    LOmega ~ lkj_corr_cholesky(2); tau ~ cauchy(0,2.5);
    LOmega1 ~ lkj_corr_cholesky(2); tau1 ~ cauchy(0,2.5);    
    LOmega2 ~ lkj_corr_cholesky(2); tau2 ~ cauchy(0,2.5);
    LOmega3 ~ lkj_corr_cholesky(2); tau3 ~ cauchy(0,2.5);
    mean_beta ~ normal(0, 5);
    for (i in 1:M) beta[i] ~ multi_normal_cholesky(beta1[l1[i]], LSigma_beta); 
    for (i in 1:M1) beta1[i] ~ multi_normal_cholesky(beta2[l2[i]], LSigma_beta1);
    for (i in 1:M2) beta2[i] ~ multi_normal_cholesky(beta3[l3[i]], LSigma_beta2);
    for (i in 1:M3) beta3[i] ~ multi_normal_cholesky(mean_beta, LSigma_beta3);
    for (i in 1:N) { imean[i] <- X[i] * beta[pnro[i]]'; isigma[i] <- sigma/sqrt(count[i]); }
    sigma ~ cauchy(0, 2);
    lprice ~ normal(imean, isigma);
}