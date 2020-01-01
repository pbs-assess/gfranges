#include <TMB.hpp>

template <class Type>
bool isNA(Type x)
{
  return R_IsNA(asDouble(x));
}

template <class Type>
Type dstudent(Type x, Type mean, Type sigma, Type df, int give_log = 0)
{
  // from metRology::dt.scaled()
  // dt((x - mean)/sd, df, ncp = ncp, log = TRUE) - log(sd)
  Type logres = dt((x - mean) / sigma, df, true) - log(sigma);
  if (give_log)
    return logres;
  else
    return exp(logres);
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
  
  DATA_SPARSE_MATRIX(A_sk); // INLA 'A' projection matrix for unique stations
  DATA_IVECTOR(A_spatial_index); // Vector of stations to match up A_sk output

  DATA_STRUCT(spde, spde_t); // SPDE objects from R-INLA
  
  DATA_IVECTOR(k_i); // species index
  DATA_INTEGER(n_k);   // number of species
  
  DATA_SCALAR(nu);   // dt(df = nu)
  
  // ------------------ Parameters ---------------------------------------------
  
  // Parameters
  // Fixed effects
  PARAMETER_VECTOR(b_j);  // fixed effect parameters
  PARAMETER_VECTOR(log_gamma);  // re parameter sigmas
  PARAMETER(ln_tau_O);    // spatio-temporal process
  PARAMETER(ln_kappa);    // Matern parameter
  PARAMETER(ln_phi);           // sigma / dispersion / etc.
  
  // Random effects
  PARAMETER_ARRAY(omega_sk);  // spatio-temporal effects; n_s by n_k matrix
  PARAMETER_ARRAY(b_re);  // re parameters
  
  // ------------------ End of parameters --------------------------------------
  
  int n_i = y_i.size();   // number of observations
  Type nll_data = 0;     // likelihood of data
  Type nll_omega = 0;    // spatial effects
  Type nll_gamma = 0;       // other re parameters
  
  // ------------------ Geospatial ---------------------------------------------
  
  // Matern:
  Type range = sqrt(Type(8.0)) / exp(ln_kappa);
  vector<Type> sigma_O(n_k);
  for(int k = 0; k < n_k; k++) {
    sigma_O(k) = 1 / sqrt(Type(4.0) * M_PI * exp(Type(2.0) * ln_tau_O) *
      exp(Type(2.0) * ln_kappa));
  }
  Eigen::SparseMatrix<Type> Q; // Precision matrix
  Q = R_inla::Q_spde(spde, exp(ln_kappa));
  
  // ------------------ INLA projections ---------------------------------------
  
  // Here we are projecting the spatial random effects to the
  // locations of the data using the INLA 'A' matrices.
  array<Type> omega_sk_A(A_sk.rows(), A_sk.cols());
  for (int k = 0; k < n_k; k++)
    omega_sk_A.col(k) = A_sk * vector<Type>(omega_sk.col(k));
  vector<Type> omega_sk_A_vec(n_i);
  
  // ------------------ Linear predictor ---------------------------------------
  
  vector<Type> eta_fixed_i = X_ij * b_j;
  vector<Type> eta_i(n_i);
  for (int i = 0; i < n_i; i++) {
    eta_i(i) = eta_fixed_i(i);
    for (int j = 0; j < (b_re.cols()); j++) {
      eta_i(i) += X_ij(i, j) * b_re(k_i(i), j);
    }
    omega_sk_A_vec(i) = omega_sk_A(A_spatial_index(i), k_i(i)); // record it
    eta_i(i) += omega_sk_A_vec(i);  // spatial
  }
  
  // ------------------ Probability of random effects --------------------------
  
  for (int k = 0; k < b_re.rows(); k++) {
    for (int j = 0; j < (b_re.cols()); j++) {
      nll_gamma -= dnorm(b_re(k,j), Type(0), exp(log_gamma(j)), true);
    }
  }
  
  // Spatial random effects:
  for (int k = 0; k < n_k; k++) {
    nll_omega += SCALE(GMRF(Q, true), 1. / exp(ln_tau_O))(omega_sk.col(k));
  }
  
  // ------------------ Probability of data given random effects ---------------
  
  for (int i = 0; i < n_i; i++) {
    if (!isNA(y_i(i))) {
      // nll_data -= dnorm(y_i(i), eta_i(i), exp(ln_phi), true);
      nll_data -= dstudent(y_i(i), eta_i(i), exp(ln_phi), nu /*df*/, true);
    }
  }
  // ------------------ Predictions on new data --------------------------------
  // ------------------ Derived quantities ---------------------------------
  // ------------------ Reporting ----------------------------------------------
  
  REPORT(omega_sk_A_vec);   // spatio-temporal effects; vector
  REPORT(range);        // ~ Matern approximate distance at 10% correlation
  ADREPORT(range);      // ~ Matern approximate distance at 10% correlation
  REPORT(sigma_O);
  ADREPORT(sigma_O);
  
  // ------------------ Joint negative log likelihood --------------------------
  
  Type jnll = nll_data + nll_omega + nll_gamma;
  return jnll;
}