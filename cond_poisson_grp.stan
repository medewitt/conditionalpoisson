data{
	int<lower=1> n_pat; //total number of patients
	int<lower=1> n_vars;
	int<lower=1> n_groups;

	int<lower=1> n_centres;
	
	int new_muts[n_pat]; 
	vector[n_pat] follow_up_t_anal; // event or censoring time
	int group_vec[n_pat];
	int centre_vec[n_pat];
	
	int n_pat_grp[n_groups];
	int N_new_muts_comb_group[n_groups];

	vector[n_vars] X_i[n_pat]; // vectors of predictors for each patient
		
	}

parameters {
	vector[n_vars] beta_vec;
	vector[n_centres] v_vec;
	vector[n_pat] u_vec;


	real<lower=0.0> b_par;
	real<lower=0.0> sigma_v;

	real<lower=0.0> sigma_u;
	}

model {
	int i_index;
	i_index=0;
	
  for (h in 1:n_groups) {
	vector[n_pat_grp[h]] exp_XiB;
	int muts_vec_grp[n_pat_grp[h]]; 
	vector[n_pat_grp[h]] theta_vec_grp; 
	real sum_hazards;
  
  for (k in 1:n_pat_grp[h]) {
		exp_XiB[k] = follow_up_t_anal[k + i_index]*exp(dot_product(beta_vec, X_i[k + i_index])+ sigma_u*u_vec[k + i_index] + sigma_v*v_vec[centre_vec[k + i_index]]);
		muts_vec_grp[k] = new_muts[k + i_index];
		}
		
	i_index=i_index + n_pat_grp[h];
		
	sum_hazards= sum(exp_XiB);
	theta_vec_grp =	exp_XiB/sum_hazards;
	
	muts_vec_grp ~ multinomial(theta_vec_grp);

	}
		
	beta_vec~double_exponential(0,b_par);
	b_par~gamma(2,2);
	v_vec~normal(0,1);

	u_vec~normal(0,1);

	sigma_v~gamma(2,2);

	sigma_u~gamma(2,2);

	
	}


