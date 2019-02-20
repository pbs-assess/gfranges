#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
{
  // ------------------ Data ---------------------------------------------
  DATA_IVECTOR(g_i);
  DATA_VECTOR(y_i);
  DATA_VECTOR(x);
  
  
  // ------------------ Parameters ---------------------------------------------
  PARAMETER( a ); // intercept
  PARAMETER( b ); // slope
  PARAMETER( p ); // tweedie power
  PARAMETER( log_phi ); // dispersion / sigma
  
  PARAMETER( log_SDZ ); // omega / random 
  PARAMETER_VECTOR( z_g );
  

  
  // ------------------ negative log likelihood --------------------------
   
  Type power = invlogit(p) + Type(1.0);
  Type phi = exp(log_phi);
  Type SDZ = exp(log_SDZ);
  
  int n = y_i.size(); // get number of data points to loop over
  
  Type nll = 0.0;  // initialize negative log likelihood
  
  // Probability of data conditional on fixed and random effect values
  for( int i = 0; i < n; i++){
    nll -= dtweedie(y_i[i], exp(a + z_g(g_i[i]) + b * x[i]), phi, power, true );
  }

  // Probability of random coefficients
  for( int g=0; g < z_g.size(); g++){
    nll -= dnorm( z_g[g], Type(0.0), SDZ, true );
  }
  
  // Reporting
  REPORT( SDZ );
  REPORT(power);
  REPORT(phi);
  
  ADREPORT( SDZ );
  ADREPORT( power );
  ADREPORT( phi );
  
  return nll;
}
