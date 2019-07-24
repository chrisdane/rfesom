// Rcpp::sourceCpp(this file)
// http://adv-r.had.co.nz/Rcpp.html
// https://teuder.github.io/rcpp4everyone_en
// https://csgillespie.github.io/efficientR/performance.html#rcpp

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector sub_e2_to_n2(IntegerMatrix elem2d, NumericVector data_elem2d, int nod2d_n) {
    
    int elem2d_n = elem2d.ncol(); // number of 2d elements
    int i, j, elnode;
    NumericVector tmp(nod2d_n);
    NumericVector inds(nod2d_n);
    NumericVector result(nod2d_n);

	for (i=0; i<elem2d_n; i++) { 
        for (j=0; j<3; j++) {
            elnode = elem2d(j,i) - 1L; // C counts from 0
            tmp[elnode] = tmp[elnode] + data_elem2d[i];
            inds[elnode] = inds[elnode] + 1;
        } 
    } 

    result = tmp/inds ;
    return result;

} // sub_e2_to_n2


#include <Rcpp.h>
// sub_e2_to_n2
NumericVector sub_e2_to_n2(IntegerMatrix elem2d, NumericVector data_elem2d, int nod2d_n);
RcppExport SEXP sourceCpp_1_sub_e2_to_n2(SEXP elem2dSEXP, SEXP data_elem2dSEXP, SEXP nod2d_nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerMatrix >::type elem2d(elem2dSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type data_elem2d(data_elem2dSEXP);
    Rcpp::traits::input_parameter< int >::type nod2d_n(nod2d_nSEXP);
    rcpp_result_gen = Rcpp::wrap(sub_e2_to_n2(elem2d, data_elem2d, nod2d_n));
    return rcpp_result_gen;
END_RCPP
}
