#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
DataFrame ybanova(List fit_model) {
    // Read information from the fit model
  int R_df = as<int>(fit_model["R_df"]);
  int M_df = as<int>(fit_model["M_df"]);
  double F_statistic = as<double>(fit_model["F_statistic"]);
  double Pr_F = as<double>(fit_model["p_value_F"]);
  double RSE = as<double>(fit_model["RSE"]);
  double SS = as<double>(fit_model["SS_X"]);
    //Calculate MSE amd MSR used the information provided in fit model
  double mse = pow(RSE, 2);
  double msr = SS / M_df;

  double rss = mse * R_df;
    //Form a database that generate ANOVA table
  return DataFrame::create(
    Named("Source") = CharacterVector::create("X", "Residuals"),
    Named("Df") = IntegerVector::create(M_df, R_df),
    Named("Sum Sq") = NumericVector::create(SS, rss),
    Named("Mean Sq") = NumericVector::create(msr, mse),
    Named("F value") = NumericVector::create(F_statistic, NA_REAL),
    Named("Pr(>F)") = NumericVector::create(Pr_F, NA_REAL)
  );
}
