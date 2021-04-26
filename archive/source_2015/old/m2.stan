data {
    int N; int M; int M1; int M2; int M3;
    vector[N] lprice; 
    int<lower=1> count[N];
    real yr[N];
    int pnro[N]; int l1[M]; int l2[M1]; int l3[M2];
}
transformed data {
    matrix[N, 3] X;
    for (i in 1:N) { X[i, 1] <- 1; X[i, 2] <- yr[i]; X[i, 3] <- yr[i]*yr[i]; }
}
parameters {
    vector[3] mean_beta;
    corr_matrix[3] Omega; vector<lower=0>[3] tau;  // FIXME: could use Cholesky
    corr_matrix[3] Omega1; vector<lower=0>[3] tau1;
    corr_matrix[3] Omega2; vector<lower=0>[3] tau2;
    corr_matrix[3] Omega3; vector<lower=0>[3] tau3;
    matrix[M, 3] beta; // pnro level
    matrix[M1, 3] beta1; // l1 level
    matrix[M2, 3] beta2; // l2 level
    matrix[M3, 3] beta3; // l3 level
    real<lower=0> sigma;
}
model {
    // FIXME: should use multi-t instead of multinormals?
    // FIXME: should assume log-normal for prices of original transactions (that we don't have)?
    // (Impossible exactly, but see mean and std of lognormal)
    vector[N] imean;
    vector[N] isigma;
    matrix[3, 3] Sigma_beta;
    matrix[3, 3] Sigma_beta1;
    matrix[3, 3] Sigma_beta2;
    matrix[3, 3] Sigma_beta3;
    Sigma_beta <- quad_form_diag(Omega, tau);
    Sigma_beta1 <- quad_form_diag(Omega1, tau1);
    Sigma_beta2 <- quad_form_diag(Omega2, tau2);
    Sigma_beta3 <- quad_form_diag(Omega3, tau3);
    Omega ~ lkj_corr(3); tau ~ cauchy(0,2.5);
    Omega1 ~ lkj_corr(3); tau1 ~ cauchy(0,2.5);    
    Omega2 ~ lkj_corr(3); tau2 ~ cauchy(0,2.5);
    Omega3 ~ lkj_corr(3); tau3 ~ cauchy(0,2.5);
    mean_beta ~ normal(0, 5);
    for (i in 1:M) beta[i] ~ multi_normal(beta1[l1[i]], Sigma_beta); // FIXME: could be vectorized
    for (i in 1:M1) beta1[i] ~ multi_normal(beta2[l2[i]], Sigma_beta1);
    for (i in 1:M2) beta2[i] ~ multi_normal(beta3[l3[i]], Sigma_beta2);
    for (i in 1:M3) beta3[i] ~ multi_normal(mean_beta, Sigma_beta3);
    for (i in 1:N) { imean[i] <- X[i] * beta[pnro[i]]'; isigma[i] <- sigma/sqrt(count[i]); }
    sigma ~ cauchy(0, 2);
    lprice ~ normal(imean, isigma);
}