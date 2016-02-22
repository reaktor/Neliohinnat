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
    vector[6] zero_beta;
    for (i in 1:N) { X[i, 1] <- 1; X[i, 2] <- yr[i]; X[i, 3] <- yr[i]*yr[i]; 
                     X[i, 4] <- z[i]; X[i, 5] <- yr[i]*z[i]; X[i, 6] <- yr[i]*yr[i]*z[i]; }
    for (i in 1:6) zero_beta[i] <- 0;
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
    real<lower=0> ysigma;
    real<lower=0> df; 
}
model {
    // FIXME:
    // - This model has replicates of covariates z at the unit level. Does not hurt, but 
    //   they are not estimable. Covariates X[, (4, 5, 6)] should exist only at the l1 level.
    vector[N] imean;
    vector[N] isigma;
    matrix[6, 6] LSigma_beta;
    matrix[6, 6] LSigma_beta1;
    matrix[6, 6] LSigma_beta2;
    matrix[6, 6] LSigma_beta3;
    LSigma_beta <- diag_pre_multiply(tau, LOmega);
    LSigma_beta1 <- diag_pre_multiply(tau1, LOmega1);
    LSigma_beta2 <- diag_pre_multiply(tau2, LOmega2);
    LSigma_beta3 <- diag_pre_multiply(tau3, LOmega3);
    LOmega ~ lkj_corr_cholesky(2);  tau ~ lognormal(-1., 3.);
    LOmega1 ~ lkj_corr_cholesky(2); tau1 ~ lognormal(-1., 3.);    
    LOmega2 ~ lkj_corr_cholesky(2); tau2 ~ lognormal(-1., 3.);
    LOmega3 ~ lkj_corr_cholesky(2); tau3 ~ lognormal(-1., 3.);
    mean_beta ~ normal(0, 5);
    for (i in 1:M) beta[i] ~ multi_normal_cholesky(zero_beta, LSigma_beta); 
    for (i in 1:M1) beta1[i] ~ multi_normal_cholesky(zero_beta, LSigma_beta1);
    for (i in 1:M2) beta2[i] ~ multi_normal_cholesky(zero_beta, LSigma_beta2);
    for (i in 1:M3) beta3[i] ~ multi_normal_cholesky(zero_beta, LSigma_beta3);
    // FIXME: 
    // - Remove bottom-level covars 4..6, for there are no repeats?
    // - pnro-level sigmas?
    //   The log-normal prices are in the data averaged at the euro level, so the 
    //   distribution of the averages is a bit skewed and long-tailed. 
    // - Should or could the log-normal origin be taken into account analytically?
    //   (Impossible exactly, but see mean and std of lognormal)
    for (i in 1:N) { 
           imean[i] <- X[i] * (mean_beta + 
                               (beta3[l3[l2[l1[pnro[i]]]]] + beta2[l2[l1[pnro[i]]]] + beta1[l1[pnro[i]]] + 
                                beta[pnro[i]])'); 
           isigma[i] <- sqrt(ysigma^2 + sigma^2/count[i]); }
    sigma ~ normal(0, 2);
    ysigma ~ normal(0, 2);
    df ~ normal(0, 20);
    lprice ~ student_t(df+1, imean, isigma);
}

