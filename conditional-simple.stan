data{
  int N; //number
  int J; //predictors
  int K; //strata
  int y[N]; //Response
  matrix [N, J] x;
}

transformed data {
  int y_marginal = sum(y);
}

parameters{
  vector[J] beta;
}
model {
  vector[K] theta;
  vector[K] exp_sum;

  for( i in 1:K){
    exp_sum[i] = exp( x[i,] * beta);
  }

  for(i in 1:K){

    theta[i] = exp_sum[i]/ sum(exp_sum);

  }

  beta[1] ~ normal(5,1);
  beta[2] ~ normal(0,3);
  y ~ multinomial(theta);


}
generated quantities{
  array[N] int yhat;
  vector[K] theta;
  vector[K] exp_sum;

  for( i in 1:K){
    exp_sum[i] = exp( x[i,] * beta);
  }

  for(i in 1:K){

    theta[i] = exp_sum[i]/ sum(exp_sum);

  }

    yhat = multinomial_rng(theta , y_marginal);
}
