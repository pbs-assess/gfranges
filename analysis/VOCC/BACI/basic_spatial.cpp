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
  
  DATA_SPARSE_MATRIX(A); // INLA 'A' projection matrix for original data
  // DATA_SPARSE_MATRIX(A_st); // INLA 'A' projection matrix for unique stations
  DATA_IVECTOR(A_spatial_index); // Vector of stations to match up A_st output
  
  // Indices for factors
  // DATA_FACTOR(year_i);
  
  // SPDE objects from R-INLA
  DATA_STRUCT(spde, spde_t);
 
  // ------------------ Parameters ---------------------------------------------
  
  // Parameters
  // Fixed effects
  PARAMETER_VECTOR(b_j);  // fixed effect parameters
  PARAMETER(ln_tau_O);    // spatial process
  // PARAMETER(ln_tau_E);    // spatio-temporal process
  PARAMETER(ln_kappa);    // Matern parameter
  
  PARAMETER(ln_phi);           // sigma / dispersion / etc.
  
  // Random effects
  PARAMETER_VECTOR(omega_s);    // spatial effects; n_s length
  // PARAMETER_ARRAY(epsilon_st);  // spatio-temporal effects; n_s by n_t matrix
  
  // ------------------ End of parameters --------------------------------------
  
  int n_i = y_i.size();   // number of observations
  int n_j = X_ij.cols();  // number of observations
  
  Type nll_data = 0;     // likelihood of data
  Type nll_omega = 0;    // spatial effects
  Type nll_epsilon = 0;  // spatio-temporal effects

  // ------------------ Geospatial ---------------------------------------------
  
  // Matern:
  Type range = sqrt(Type(8.0)) / exp(ln_kappa);
  Type sigma_O = 1 / sqrt(Type(4.0) * M_PI * exp(Type(2.0) * ln_tau_O) *
    exp(Type(2.0) * ln_kappa));
  REPORT(sigma_O);
  ADREPORT(sigma_O);
  
  // Type sigma_E = 1 / sqrt(Type(4.0) * M_PI * exp(Type(2.0) * ln_tau_E) *
  //   exp(Type(2.0) * ln_kappa));
  
  Eigen::SparseMatrix<Type> Q; // Precision matrix
  
  Q = R_inla::Q_spde(spde, exp(ln_kappa));
  
  // ------------------ INLA projections ---------------------------------------
  
  // Here we are projecting the spatiotemporal and spatial random effects to the
  // locations of the data using the INLA 'A' matrices.
  // array<Type> epsilon_st_A(A_st.rows(), n_t);
  // for (int i = 0; i < n_t; i++)
  //   epsilon_st_A.col(i) = A_st * vector<Type>(epsilon_st.col(i));
  vector<Type> omega_s_A = A * omega_s;
  // vector<Type> epsilon_st_A_vec(n_i);
  
  // ------------------ Linear predictor ---------------------------------------
  
  vector<Type> eta_fixed_i = X_ij * b_j;
  vector<Type> mu_i(n_i), eta_i(n_i);
  for (int i = 0; i < n_i; i++) {
    eta_i(i) = Type(0);
  }
  for (int i = 0; i < n_i; i++) {
    eta_i(i) = eta_fixed_i(i);
    eta_i(i) += omega_s_A(i);  // spatial
    // epsilon_st_A_vec(i) = epsilon_st_A(A_spatial_index(i), year_i(i)); // record it
    // eta_i(i) += epsilon_st_A_vec(i); // spatiotemporal
    mu_i(i) = eta_i(i);
  }
  
  // ------------------ Probability of random effects --------------------------
  
  // Spatial (intercept) random effects:
  nll_omega += SCALE(GMRF(Q, true), 1.0 / exp(ln_tau_O))(omega_s);
  
  // Spatiotemporal random effects:
  // for (int t = 0; t < n_t; t++) {
  //   nll_epsilon += SCALE(GMRF(Q, true), 1. / exp(ln_tau_E))(epsilon_st.col(t));
  // }
  
  // ------------------ Probability of data given random effects ---------------
  
  for (int i = 0; i < n_i; i++) {
    if (!isNA(y_i(i))) {
      nll_data -= dnorm(y_i(i), mu_i(i), exp(ln_phi), true);
    }
  }
  // ------------------ Predictions on new data --------------------------------
  // ------------------ Derived quantities ---------------------------------
  
  
  // ------------------ Reporting ----------------------------------------------
  
  // REPORT(sigma_E);      // spatio-temporal process parameter
  // ADREPORT(sigma_E);      // spatio-temporal process parameter
  // REPORT(epsilon_st_A_vec);   // spatio-temporal effects; vector
  // REPORT(b_rw_t);   // time-varying effects
  // REPORT(omega_s_A);      // spatial effects; n_s length vector
  // REPORT(omega_s_trend_A); // spatial trend effects; n_s length vector
  // REPORT(eta_fixed_i);  // fixed effect predictions in the link space
  // REPORT(eta_i);        // fixed and random effect predictions in link space
  // REPORT(eta_rw_i);     // time-varying predictions in link space
  // REPORT(rho);          // AR1 correlation in -1 to 1 space
  // REPORT(range);        // Matern approximate distance at 10% correlation
  // ADREPORT(range);      // Matern approximate distance at 10% correlation
  
  // ------------------ Joint negative log likelihood --------------------------
  
  Type jnll = nll_data + nll_omega + nll_epsilon;
  return jnll;
}
