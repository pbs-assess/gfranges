#include <TMB.hpp>

template <class Type>
bool isNA(Type x)
{
  return R_IsNA(asDouble(x));
}

// ------------------ Main TMB template ----------------------------------------

template <class Type>
Type objective_function<Type>::operator()()
{
  using namespace R_inla;
  using namespace density;
  using namespace Eigen;
  
  // Vectors of real data
  DATA_VECTOR(y_i);      // response
  DATA_MATRIX(X_ij);     // model matrix
  
  // DATA_INTEGER(n_t);  // number of years
  
  // DATA_SPARSE_MATRIX(A); // INLA 'A' projection matrix for original data
  DATA_SPARSE_MATRIX(A_sk); // INLA 'A' projection matrix for unique stations
  DATA_IVECTOR(A_spatial_index); // Vector of stations to match up A_sk output
  
  // Indices for factors
  // DATA_FACTOR(year_i);
  
  // SPDE objects from R-INLA
  DATA_STRUCT(spde, spde_t);
  
  DATA_IVECTOR(k_i); // species index
  DATA_INTEGER(n_k);   // number of species
  DATA_IVECTOR(species_id_k);   // species identifier to calculate the mean for each species across years
  DATA_INTEGER(n_just_species);  // the true number of species
  DATA_VECTOR(n_years_per_species);
  
  DATA_VECTOR(intercept_i);
  DATA_VECTOR(after_i);
  DATA_VECTOR(source_i);
  DATA_IVECTOR(m_i); // cell ID
  DATA_INTEGER(interaction_position);
  
  // ------------------ Parameters ---------------------------------------------
  
  // Parameters
  // Fixed effects
  PARAMETER_VECTOR(b_j);  // fixed effect parameters
  PARAMETER_ARRAY(b_re);  // re parameters
  PARAMETER_VECTOR(b_re_sp);  // re parameters
  PARAMETER_VECTOR(b_cell);  // re parameters
  PARAMETER_VECTOR(log_gamma);  // re parameter sigmas
  PARAMETER(log_omega);  // re sp-level mean BACI interactions
  PARAMETER(log_varphi);  // re cell sigma
  // PARAMETER(ln_tau_O);    // spatial process
  PARAMETER(ln_tau_E);    // spatio-temporal process
  PARAMETER(ln_kappa);    // Matern parameter
  
  PARAMETER(ln_phi);           // sigma / dispersion / etc.
  
  // Random effects
  // PARAMETER_VECTOR(omega_s);    // spatial effects; n_s length
  PARAMETER_ARRAY(epsilon_sk);  // spatio-temporal effects; n_s by n_t matrix
  
  // ------------------ End of parameters --------------------------------------
  
  int n_i = y_i.size();   // number of observations
 // int n_j = X_ij.cols();  // number of fixed effect parameters
  
  Type nll_data = 0;     // likelihood of data
  Type nll_epsilon = 0;    // spatial effects
  Type nll_re = 0;  // re

  // ------------------ Geospatial ---------------------------------------------
  
  // Matern:
  Type range = sqrt(Type(8.0)) / exp(ln_kappa);
  // Type sigma_O = 1 / sqrt(Type(4.0) * M_PI * exp(Type(2.0) * ln_tau_O) *
  //   exp(Type(2.0) * ln_kappa));
  // REPORT(sigma_O);
  // ADREPORT(sigma_O);
  
  Type sigma_E = 1 / sqrt(Type(4.0) * M_PI * exp(Type(2.0) * ln_tau_E) *
    exp(Type(2.0) * ln_kappa));
  REPORT(sigma_E);
  ADREPORT(sigma_E);
  
  Eigen::SparseMatrix<Type> Q; // Precision matrix
  
  Q = R_inla::Q_spde(spde, exp(ln_kappa));
  
  // ------------------ INLA projections ---------------------------------------
  
  // Here we are projecting the spatial random effects to the
  // locations of the data using the INLA 'A' matrices.
  array<Type> epsilon_sk_A(A_sk.rows(), A_sk.cols());
  for (int k = 0; k < n_k; k++)
    epsilon_sk_A.col(k) = A_sk * vector<Type>(epsilon_sk.col(k));
  // vector<Type> omega_s_A = A * omega_s;
  vector<Type> epsilon_sk_A_vec(n_i);
  
  // ------------------ Linear predictor ---------------------------------------
  
  vector<Type> eta_fixed_i = X_ij * b_j;
  vector<Type> mu_i(n_i), eta_i(n_i);
  for (int i = 0; i < n_i; i++) {
    eta_i(i) = Type(0);
  }
  for (int i = 0; i < n_i; i++) {
    eta_i(i) = eta_fixed_i(i) + 
      b_re(k_i(i),0) * intercept_i(i) + 
      b_re(k_i(i),1) * after_i(i) + 
      b_re(k_i(i),2) * source_i(i) +
      b_re(k_i(i),3) * after_i(i) * source_i(i) + 
      b_re_sp(species_id_k(k_i(i))) * after_i(i) * source_i(i) +
      b_cell(m_i(i)) * Type(1.0);
    
    epsilon_sk_A_vec(i) = epsilon_sk_A(A_spatial_index(i), k_i(i)); // record it
      
    eta_i(i) += epsilon_sk_A_vec(i);  // spatial
    mu_i(i) = eta_i(i);
  }
  
  // ------------------ Probability of random effects --------------------------
  
  for(int k = 0; k < b_re.rows(); k++) {
    for(int r = 0; r < (b_re.cols() - 1); r++) {
      int z;
      // 1st and 2nd random effects share a variance bc cells are matched
      if (r == 0) { 
        z = 0;
      } else {
        z = r - 1;
      }
      nll_re -= dnorm(b_re(k,r), Type(0.0), exp(log_gamma(z)), true);
    }
  }
  for(int k = 0; k < b_re.rows(); k++) {
    nll_re -= dnorm(b_re(k,3), Type(0.0), exp(log_omega), true);
  }
  for(int u = 0; u < n_just_species; u++) {
    nll_re -= dnorm(b_re_sp(u), Type(0.0), exp(log_omega), true);
  }

  for(int m = 0; m < b_cell.size(); m++) {
    nll_re -= dnorm(b_cell(m), Type(0.0), exp(log_varphi), true);
  }
  
  // Spatial random effects:
  for (int k = 0; k < n_k; k++) {
    nll_epsilon += SCALE(GMRF(Q, true), 1. / exp(ln_tau_E))(epsilon_sk.col(k));
  }
  
  // ------------------ Probability of data given random effects ---------------
  
  for (int i = 0; i < n_i; i++) {
    if (!isNA(y_i(i))) {
      nll_data -= dnorm(y_i(i), mu_i(i), exp(ln_phi), true);
    }
  }
  // ------------------ Predictions on new data --------------------------------
  // ------------------ Derived quantities ---------------------------------
  
  vector<Type> b_baci_interaction(n_k);
  
  for(int k = 0; k < b_re.rows(); k++) {
    b_baci_interaction(k) = b_re(k,3) + b_re_sp(species_id_k(k)) + b_j(interaction_position);
  }
  
  // vector<Type> b_baci_interaction_spp(n_just_species);
  // for(int k = 0; k < b_re.rows(); k++) {
  //   b_baci_interaction_spp(species_id_k(k)) += b_baci_interaction(k);
  // }
  // for(int i = 0; i < n_years_per_species.size(); i++) {
  //   b_baci_interaction_spp(i) = b_baci_interaction_spp(i) / n_years_per_species(i);
  // }
  // 
  REPORT(b_baci_interaction);    
  ADREPORT(b_baci_interaction);
  
  // REPORT(b_baci_interaction_spp);    
  // ADREPORT(b_baci_interaction_spp);
  
  // ------------------ Reporting ----------------------------------------------
  
  // REPORT(sigma_E);      // spatio-temporal process parameter
  // ADREPORT(sigma_E);      // spatio-temporal process parameter
  REPORT(epsilon_sk_A_vec);   // spatio-temporal effects; vector
  // REPORT(b_rw_t);   // time-varying effects
  // REPORT(omega_s_A);      // spatial effects; n_s length vector
  // REPORT(omega_s_trend_A); // spatial trend effects; n_s length vector
  // REPORT(eta_fixed_i);  // fixed effect predictions in the link space
  // REPORT(eta_i);        // fixed and random effect predictions in link space
  // REPORT(eta_rw_i);     // time-varying predictions in link space
  // REPORT(rho);          // AR1 correlation in -1 to 1 space
  REPORT(range);        // Matern approximate distance at 10% correlation
  // ADREPORT(range);      // Matern approximate distance at 10% correlation
  
  // ------------------ Joint negative log likelihood --------------------------
  
  Type jnll = nll_data + nll_epsilon + nll_re;
  return jnll;
}
