#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
NumericVector fcnExtractElementsAtIndices(IntegerVector row_indices, NumericMatrix mat) {
  int n = row_indices.size();
  NumericVector result(n);

  for (int i = 0; i < n; ++i) {
    int row_idx = row_indices[i] - 1; // R uses 1-based indexing
    if (row_idx >= 0 && row_idx < mat.nrow()) {
      result[i] = mat(i, row_idx); // Assuming you want the first column
    } else {
      result[i] = NA_REAL; // Handle out-of-range indices with NA
    }
  }

  return result;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
fcnExtractElementsAtIndices(c(1,3,2), matrix(c(1,2,3,1,2,3,1,2,3), nrow = 3, ncol = 3, byrow = T))
*/
