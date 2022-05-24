./m4 variational algorithm=meanfield iter=30000 elbo_samples=1000 eval_elbo=1000 eta_adagrad=0.001 data file=../stan-data.r output refresh=100 file=../advi-meanfield-output.csv

# Does not converge, would require smaller eta and more (~ 30000) iterations, or ~ 4-7 days...
./m4 variational algorithm=fullrank iter=6000 elbo_samples=1000 eval_elbo=100 eta_adagrad=0.002 data file=../stan-data.r output refresh=100 file=../advi-fullrank-output.csv

