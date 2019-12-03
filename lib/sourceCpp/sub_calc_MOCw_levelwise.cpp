// Rcpp::sourceCpp(this file)
// http://adv-r.had.co.nz/Rcpp.html
// https://teuder.github.io/rcpp4everyone_en
// https://csgillespie.github.io/efficientR/performance.html#rcpp

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector sub_calc_MOCw_levelwise(IntegerMatrix elem2d, NumericVector voltriangle, NumericVector ycsur, 
                                      NumericVector moc_reg_lat_global, NumericVector moc_mask, IntegerMatrix moc_topo,
                                      NumericVector data_node, int total_rec) {
    
    int elem2d_n = elem2d.ncol(); // number of 2d elements
    int ei, e123, di, elnode;
    NumericVector elnodes(3);
    NumericVector tmp(nod2d_n);
    NumericVector inds(nod2d_n);
    NumericVector result(nod2d_n);

    for (ei=0; ei<elem2d_n; ei++) {
        for (e123=0; e123<3; e123++) {
            elnodes[e123] = elem2d(e123,ei) - 1L; // C counts from 0
        }
        m = moc_mask[elnodes]
        
    return result;

} // sub_calc_MOCw_levelwise

