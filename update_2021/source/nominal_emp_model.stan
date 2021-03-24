// This is with yearly covariates, t(df) residuals, two sigmas.
// FIXME: tau[,3] does not mix well;
data {
    int N; int N_pred;
    int M; int M1; int M2;
    int n_covs;
    int n_years;
    row_vector[n_covs] covs[N];
    row_vector[n_covs] covs_pred[N_pred];
    vector[N] lprice; vector[N_pred] lprice_pred;
    int<lower=1> count[N]; int<lower=0> count_pred[N_pred];
    real yr[N]; real yr_pred[N_pred];
    int<lower=1,upper=n_years> year[N];
    int<lower=1,upper=n_years> year_pred[N_pred];
    int pnro[N]; int pnro_pred[N_pred];
    int l1[N]; int l2[N];
    int l1_pred[N_pred]; int l2_pred[N_pred];
}
transformed data {
    row_vector[2] X[N];
    row_vector[2] X_pred[N_pred];
    vector[2] zero_beta; 
    for (i in 1:N) { X[i, 1] = 1; X[i, 2] = yr[i];} // X[i, 3] = yr[i]*yr[i]; }
    for (i in 1:N_pred) { X_pred[i, 1] = 1; X_pred[i, 2] = yr_pred[i]; } //X_pred[i, 3] = yr_pred[i]*yr_pred[i]; }
    for (i in 1:2) zero_beta[i] = 0;
}
parameters {
    cholesky_factor_corr[2] LOmega; vector<lower=0>[2] tau;  
    cholesky_factor_corr[2] LOmega1; vector<lower=0>[2] tau1;
    cholesky_factor_corr[2] LOmega2; vector<lower=0>[2] tau2;
    row_vector[2] beta[M]; // pnro level
    row_vector[2] beta1[M1]; // l1 level
    row_vector[2] beta2[M2]; // l2 level
    real<lower=0> sigma;
    real<lower=0> ysigma;
    real<lower=0> df;
    row_vector[n_covs] beta_year[n_years];
}
model {
    vector[N] obs_mean;
    vector[N] obs_sigma;
    row_vector[2] x;
    matrix[2, 2] LSigma_beta;
    matrix[2, 2] LSigma_beta1;
    matrix[2, 2] LSigma_beta2;
    LSigma_beta = diag_pre_multiply(tau, LOmega);
    LSigma_beta1 = diag_pre_multiply(tau1, LOmega1);
    LSigma_beta2 = diag_pre_multiply(tau2, LOmega2);
    LOmega ~ lkj_corr_cholesky(2);  tau ~ lognormal(-2., 1.);
    LOmega1 ~ lkj_corr_cholesky(2); tau1 ~ lognormal(-2., 1.);    
    LOmega2 ~ lkj_corr_cholesky(2); tau2 ~ lognormal(-2., 1.);
    for (i in 1:M) beta[i] ~ multi_normal_cholesky(zero_beta, LSigma_beta); 
    for (i in 1:M1) beta1[i] ~ multi_normal_cholesky(zero_beta, LSigma_beta1);
    for (i in 1:M2) beta2[i] ~ multi_normal_cholesky(zero_beta, LSigma_beta2);
    // append_col() and append_row(); head() and tail(); segment()
    for (i in 1:N) {
           x = X[i];
           obs_mean[i] = dot_product(covs[i], beta_year[year[i]]) + 
                            dot_product(x, (beta[pnro[i]] + beta1[l1[i]] + beta2[l2[i]]));
           obs_sigma[i] = sqrt(ysigma^2 + sigma^2/count[i]); }
    sigma ~ normal(0, 2);
    ysigma ~ normal(0, 2);
    df ~ normal(0, 20);
    lprice ~ student_t(df+1, obs_mean, obs_sigma); // Reparameterize as a scale mixture?
    }
generated quantities {
   vector[N_pred] pred;
   for (i in 1:N_pred) {
        real expected_model = dot_product(covs_pred[i], beta_year[year_pred[i]]) + 
                              dot_product(X_pred[i], (beta[pnro_pred[i]] + 
                              beta1[l1_pred[i]] + beta2[l2_pred[i]]) );
        real expected_obs = lprice_pred[i];
        real prec_model = 1 / ysigma^2;
        real prec_obs = count_pred[i] / sigma^2; // may be zero because possibly count=0.
        real prec = prec_model + prec_obs; 
        pred[i] = normal_rng((prec_model * expected_model + prec_obs * expected_obs) / prec,
                             sqrt(1 / prec));
   }
}

