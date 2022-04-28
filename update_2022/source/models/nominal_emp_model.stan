// This is with yearly covariates, t(df) residuals, two sigmas.
data {
    int N; int N_pred;
    int M; int M1; int M2;
    int n_covs;
    int n_years;
    matrix[N_pred,n_covs] covs_pred;
    vector[N] lprice;
    vector[N] count;
    real yr_pred[N_pred];
    int<lower=1,upper=n_years> year_pred[N_pred];
    int pnro_pred[N_pred];
    int l1_pred[N_pred]; int l2_pred[N_pred];
    int pnro_yr_ix[N];
}
transformed data {
    matrix[N_pred,2] X_pred;
    vector[2] zero_beta; 
    for (i in 1:N_pred) { X_pred[i, 1] = 1; X_pred[i, 2] = yr_pred[i]; }
    for (i in 1:2) zero_beta[i] = 0;
}
parameters {
    cholesky_factor_corr[2] LOmega; vector<lower=0>[2] tau;  
    cholesky_factor_corr[2] LOmega1; vector<lower=0>[2] tau1;
    cholesky_factor_corr[2] LOmega2; vector<lower=0>[2] tau2;
    matrix[M,2] beta; // pnro level
    matrix[M1,2] beta1; // l1 level
    matrix[M2,2] beta2; // l2 level
    real<lower=0> sigma;
    vector<lower=0>[n_years] ysigma;
    real<lower=0> df;
    matrix[n_years, n_covs] beta_year;
    vector[N_pred] u_pred_raw;
}
transformed parameters {
    vector[N_pred] pred_mean;
    vector[N_pred] expected_model;
    expected_model = rows_dot_product(covs_pred, beta_year[year_pred]) + 
                            rows_dot_product(X_pred, (beta[pnro_pred] + 
                            beta1[l1_pred] + beta2[l2_pred]) );
    pred_mean = expected_model + ysigma[year_pred] .* u_pred_raw;
}
model {
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
    sigma ~ lognormal(-1.5, 2);
    ysigma ~ lognormal(-4.2, 2);
    df ~ normal(0, 20);
    u_pred_raw ~ normal(0,1);
    lprice ~ student_t(df+1, pred_mean[pnro_yr_ix], sigma ./ sqrt(count));
    }

