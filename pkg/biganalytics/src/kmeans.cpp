/*
 *  biganalytics: an R package containing a library of functions for
 *  use with big.matrix objects of package bigmemory.

 *  Copyright (C) 2010 John W. Emerson and Michael J. Kane
 *
 *  This file is part of biganalytics.
 *
 *  biganalytics is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published
 *  by the Free Software Foundation; either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

#include <string>
#include <fstream>
#include <sstream>
#include <iostream>
#include <algorithm>

#include "bigmemory/BigMatrix.h"
#include "bigmemory/MatrixAccessor.hpp"
#include "bigmemory/bigmemoryDefines.h"
#include "bigmemory/isna.hpp"

#include <stdio.h>
#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <stdlib.h>
#include <sys/types.h>

template<typename T, typename MatrixType>
SEXP kmeansMatrix(MatrixType x, index_type n, index_type m,
                  SEXP pcen, SEXP pclust, SEXP pclustsizes,
                  SEXP pwss, SEXP itermax)
{

  index_type j, col, nchange;

  int maxiters = INTEGER_VALUE(itermax);
  SEXP Riter;
  PROTECT(Riter = NEW_INTEGER(1));
  int *iter = INTEGER_DATA(Riter);
  iter[0] = 0;

  BigMatrix *pcent = reinterpret_cast<BigMatrix*>(R_ExternalPtrAddr(pcen));
  MatrixAccessor<double> cent(*pcent);
  BigMatrix *Pclust = reinterpret_cast<BigMatrix*>(R_ExternalPtrAddr(pclust));
  MatrixAccessor<int> clust(*Pclust);
  BigMatrix *Pclustsizes = reinterpret_cast<BigMatrix*>(R_ExternalPtrAddr(pclustsizes));
  MatrixAccessor<double> clustsizes(*Pclustsizes);
  BigMatrix *Pwss = reinterpret_cast<BigMatrix*>(R_ExternalPtrAddr(pwss));
  MatrixAccessor<double> ss(*Pwss);

  int k = (int) pcent->nrow();                // number of clusters
  int cl, bestcl, oldcluster, newcluster;
  int done = 0;

  double temp;
  vector<double> d(k);                        // Vector of distances, internal only.
  vector<double> temp1(k);
  vector<vector<double> > tempcent(m, temp1); // For copy of global centroids k x m

  // At this point I can use [][] to access things, with ss[0][cl]
  // being used for the vectors, for example.
  // Before starting the loop, we only have cent (centers) as passed into the function.
  // Calculate clust and clustsizes, then update cent as centroids.
  
  for (cl=0; cl<k; cl++) clustsizes[0][cl] = 0.0;
  for (j=0; j<n; j++) {
    bestcl = 0;
    for (cl=0; cl<k; cl++) {
      d[cl] = 0.0;
      for (col=0; col<m; col++) {
        temp = (double)x[col][j] - cent[col][cl];
        d[cl] += temp * temp;
      }
      if (d[cl]<d[bestcl]) bestcl = cl;
    }
    clust[0][j] = bestcl + 1;          // Saving the R cluster number, not the C index.
    clustsizes[0][bestcl]++;
    for (col=0; col<m; col++)
      tempcent[col][bestcl] += (double)x[col][j];
  }
  for (cl=0; cl<k; cl++)
    for (col=0; col<m; col++)
      cent[col][cl] = tempcent[col][cl] / clustsizes[0][cl];

  do {

    nchange = 0;
    for (j=0; j<n; j++) { // For each of my points, this is offset from hash position

      oldcluster = clust[0][j] - 1;
      bestcl = 0;
      for (cl=0; cl<k; cl++) {         // Consider each of the clusters
        d[cl] = 0.0;                   // We'll get the distance to this cluster.
        for (col=0; col<m; col++) {    // Loop over the dimension of the data
          temp = (double)x[col][j] - cent[col][cl];
          d[cl] += temp * temp;
        }
        if (d[cl]<d[bestcl]) bestcl = cl;
      } // End of looking over the clusters for this j

      if (d[bestcl] < d[oldcluster]) {           // MADE A CHANGE!
        newcluster = bestcl;
        clust[0][j] = newcluster + 1;
        nchange++;
        clustsizes[0][newcluster]++;
        clustsizes[0][oldcluster]--;
        for (col=0; col<m; col++) {
          cent[col][oldcluster] += ( cent[col][oldcluster] - (double)x[col][j] ) / clustsizes[0][oldcluster];
          cent[col][newcluster] += ( (double)x[col][j] - cent[col][newcluster] ) / clustsizes[0][newcluster];
        }
      }

    } // End of this pass over my points.

    iter[0]++;
    if ( (nchange==0) || (iter[0]>=maxiters) ) done = 1;

  } while (done==0);

  // Collect the sums of squares now that we're done.
  for (cl=0; cl<k; cl++) ss[0][cl] = 0.0;
  for (j=0; j<n; j++) {
    for (col=0; col<m; col++) {
      cl = clust[0][j]-1;
      temp = (double)x[col][j] - cent[col][cl];
      ss[0][cl] += temp * temp;
    }
  }

  UNPROTECT(1);
  return(Riter);

}

extern "C"
{

SEXP kmeansBigMatrix(SEXP x, SEXP cen, SEXP clust, SEXP clustsizes,
                     SEXP wss, SEXP itermax)
{
  BigMatrix *pMat =  reinterpret_cast<BigMatrix*>(R_ExternalPtrAddr(x));
  if (pMat->separated_columns())
  {
    switch (pMat->matrix_type())
    {
      case 1:
        return kmeansMatrix<char>(SepMatrixAccessor<char>(*pMat),
          pMat->nrow(), pMat->ncol(), cen, clust, clustsizes, wss, itermax);
      case 2:
        return kmeansMatrix<short>(SepMatrixAccessor<short>(*pMat),
          pMat->nrow(), pMat->ncol(), cen, clust, clustsizes, wss, itermax);
      case 4:
        return kmeansMatrix<int>(SepMatrixAccessor<int>(*pMat),
          pMat->nrow(), pMat->ncol(), cen, clust, clustsizes, wss, itermax);
      case 8:
        return kmeansMatrix<double>(SepMatrixAccessor<double>(*pMat),
          pMat->nrow(), pMat->ncol(), cen, clust, clustsizes, wss, itermax);
    }
  }
  else
  {
    switch (pMat->matrix_type())
    {
      case 1:
        return kmeansMatrix<char>(MatrixAccessor<char>(*pMat),
          pMat->nrow(), pMat->ncol(), cen, clust, clustsizes, wss, itermax);
      case 2:
        return kmeansMatrix<short>(MatrixAccessor<short>(*pMat),
          pMat->nrow(), pMat->ncol(), cen, clust, clustsizes, wss, itermax);
      case 4:
        return kmeansMatrix<int>(MatrixAccessor<int>(*pMat),
          pMat->nrow(), pMat->ncol(), cen, clust, clustsizes, wss, itermax);
      case 8:
        return kmeansMatrix<double>(MatrixAccessor<double>(*pMat),
          pMat->nrow(), pMat->ncol(), cen, clust, clustsizes, wss, itermax);
    }
  }
  return R_NilValue;
}

SEXP kmeansRIntMatrix(SEXP x, SEXP cen, SEXP clust, SEXP clustsizes,
                      SEXP wss, SEXP itermax)
{
  index_type numRows = static_cast<index_type>(nrows(x));
  index_type numCols = static_cast<index_type>(ncols(x));
  MatrixAccessor<int> mat(INTEGER_DATA(x), numRows);
  return kmeansMatrix<int, MatrixAccessor<int> >(mat,
    numRows, numCols, cen, clust, clustsizes, wss, itermax);
}

SEXP kmeansRNumericMatrix(SEXP x, SEXP cen, SEXP clust, SEXP clustsizes,
                          SEXP wss, SEXP itermax)
{
  index_type numRows = static_cast<index_type>(nrows(x));
  index_type numCols = static_cast<index_type>(ncols(x));
  MatrixAccessor<double> mat(NUMERIC_DATA(x), numRows);
  return kmeansMatrix<double, MatrixAccessor<double> >(mat,
    numRows, numCols, cen, clust, clustsizes, wss, itermax);
}

} // extern "C"

