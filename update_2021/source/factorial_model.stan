// This is with a hierarchical covariate z, t(df) residuals, two sigmas.
// FIXME: tau[,3] does not mix well; beta[,3] could be erased.
data {
    int N; int M; int M1; int M2; int ncovs;
    matrix[N, ncovs] covs;
    vector[N] lprice; 
    int<lower=1> count[N];
    real yr[N]; 
    int pnro[N]; int l1[N]; int l2[N]; 
}
transformed data {
    matrix[N, 3] X_y;
    vector[6] zero_beta; 
    for (i in 1:N) { X_y[i, 1] = 1; X_y[i, 2] = yr[i]; X_y[i, 3] = yr[i]*yr[i]; }
    for (i in 1:6) zero_beta[i] = 0;
}
parameters {
    cholesky_factor_corr[3] LOmega; vector<lower=0>[3] tau;  
    cholesky_factor_corr[6] LOmega1; vector<lower=0>[6] tau1;
    cholesky_factor_corr[6] LOmega2; vector<lower=0>[6] tau2;
    matrix[M, 3] beta; // pnro level
    matrix[M1, 6] beta1; // l1 level
    matrix[M2, 6] beta2; // l2 level
    row_vector[6] mean_beta;
    real<lower=0> sigma;
    real<lower=0> ysigma;
    real<lower=0> df;
    //real<lower=0> sigma_b_cov;
    vector[ncovs] beta_cov;
    vector[ncovs] beta_yr_cov;
}
transformed parameters {
    vector[N] z;
    vector [N] z_yr;
    matrix [N, 3] X_z;
    matrix [N, 6] X;
    z = covs * beta_cov;
    z_yr = covs * beta_yr_cov;
    for (i in 1:N) { X_z[i, 1] = z[i]; X_z[i, 2] = yr[i]*z_yr[i]; X_z[i, 3] = yr[i]*yr[i]*z_yr[i]; }
    X = append_col(X_y, X_z);
}
model {
    vector[N] obs_mean;
    vector[N] obs_sigma;
    row_vector[6] x;
    matrix[3, 3] LSigma_beta;
    matrix[6, 6] LSigma_beta1;
    matrix[6, 6] LSigma_beta2;
    LSigma_beta = diag_pre_multiply(tau, LOmega);
    LSigma_beta1 = diag_pre_multiply(tau1, LOmega1);
    LSigma_beta2 = diag_pre_multiply(tau2, LOmega2);
    LOmega ~ lkj_corr_cholesky(2);  tau ~ lognormal(-2., 1.);
    LOmega1 ~ lkj_corr_cholesky(2); tau1 ~ lognormal(-2., 1.);    
    LOmega2 ~ lkj_corr_cholesky(2); tau2 ~ lognormal(-2., 1.);
    mean_beta ~ normal(0, 5);
    //sigma_b_cov ~ cauchy(0, 1);
    beta_cov ~ normal(0, 0.25);
    for (i in 1:M) beta[i] ~ multi_normal_cholesky(head(zero_beta, 3), LSigma_beta); 
    for (i in 1:M1) beta1[i] ~ multi_normal_cholesky(zero_beta, LSigma_beta1);
    for (i in 1:M2) beta2[i] ~ multi_normal_cholesky(zero_beta, LSigma_beta2);
    // append_col() and append_row(); head() and tail(); segment()
    for (i in 1:N) {
           x = X[i];
           obs_mean[i] = x * (mean_beta + beta2[l2[i]] + beta1[l1[i]])' + head(x, 3) * beta[pnro[i]]'; 
           obs_sigma[i] = sqrt(ysigma^2 + sigma^2/count[i]); }
    sigma ~ normal(0, 2);
    ysigma ~ normal(0, 2);
    df ~ normal(0, 20);
    lprice ~ student_t(df+1, obs_mean, obs_sigma); // Reparameterize as a scale mixture?
    }
