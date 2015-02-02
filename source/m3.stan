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
}
model {
    // FIXME:
    // - This model has replicates of covariates z at the unit level. Does not hurt, but 
    //   they are not estimable. Covariates X[, (4, 5, 6)] should exist only at the l1 level.
    // - Convergence speed would probably be much faster, if the hierarchy was centralized.
    //   That is, all levels would be at mean zero, and then at the level of pnro's
    //   there would be sum of betas of all levels. 
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
    for (i in 1:M) beta[i] ~ multi_normal_cholesky(beta1[l1[i]], LSigma_beta); 
    for (i in 1:M1) beta1[i] ~ multi_normal_cholesky(beta2[l2[i]], LSigma_beta1);
    for (i in 1:M2) beta2[i] ~ multi_normal_cholesky(beta3[l3[i]], LSigma_beta2);
    for (i in 1:M3) beta3[i] ~ multi_normal_cholesky(zero_beta, LSigma_beta3);
    // FIXME: 
    // - Centralize. 
    // - Remove bottom-level covars 4..6, for there are no repeats.
    // - Should allow extra variance?
    // - Should use multi-t instead of multinormals?
    //   The log-normal prices are in the data averaged at the euro level, so the 
    //   distribution of the averages is a bit skewed and long-tailed. 
    // - Should or could the log-normal origin be taken into account analytically?
    //   (Impossible exactly, but see mean and std of lognormal)
    for (i in 1:N) { imean[i] <- X[i] * (mean_beta + beta[pnro[i]]'); isigma[i] <- sigma/sqrt(count[i]); }
    sigma ~ normal(0, 2);
    lprice ~ normal(imean, isigma);
}