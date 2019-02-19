// Normal linear mixed model specified through sparse design matrices.
#include <TMB.hpp>
template<class Type>
Type objective_function<Type>::operator() ()
{
  // data:
  DATA_VECTOR(y);
  DATA_VECTOR(x);
  // parameters:
  PARAMETER(a); // intercept
  PARAMETER(b); // slope
  PARAMETER(logSigma);
  // procedures: (transformed parameters)
  // Type sigma = exp(logSigma);
  Type nll = -sum(dnorm(y, a+b*x, exp(logSigma), true));
  return nll;
}
