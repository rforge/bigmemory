/*
 *  bigmemoryAnalytics: an R package containing a library of functions for
 *  use with big.matrix objects of package bigmemory.

 *  Copyright (C) 2009 John W. Emerson and Michael J. Kane
 *
 *  This file is part of bigmemory.
 *
 *  bigmemory is free software; you can redistribute it and/or modify
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


template<typename T1, typename BMAccessorType>
int Ckmeans2(BigMatrix *pMat, SEXP centAddr, SEXP ssAddr,
              SEXP clustAddr, SEXP clustsizesAddr, 
              SEXP nn, SEXP kk, SEXP mm, SEXP mmaxiters)
{
  // BIG x m
  //  T1 **x = (T1**) pMat->matrix();
  BMAccessorType x(*pMat);

  // k x m
  BigMatrix *pcent = (BigMatrix*)R_ExternalPtrAddr(centAddr);
  //double **cent = (double**) pcent->matrix();
  MatrixAccessor<double> cent(*pcent);

  // k x 1
  BigMatrix *pss = (BigMatrix*)R_ExternalPtrAddr(ssAddr);
  //double **ss = (double**) pss->matrix();
  MatrixAccessor<double> ss(*pss);

  // n x 1
  BigMatrix *pclust = (BigMatrix*)R_ExternalPtrAddr(clustAddr);
  //int **clust = (int**) pclust->matrix();
  MatrixAccessor<int> clust(*pclust);

  // k x 1
  BigMatrix *pclustsizes = (BigMatrix*)R_ExternalPtrAddr(clustsizesAddr);
  //double **clustsizes = (double**) pclustsizes->matrix();
  MatrixAccessor<double> clustsizes(*pclustsizes);

  long n = (long) NUMERIC_VALUE(nn);        // Very unlikely to need long, but...
  int k = INTEGER_VALUE(kk);                // Number of clusters
  long m = (long) NUMERIC_VALUE(mm);        // columns of data
  int maxiters = INTEGER_VALUE(mmaxiters); // maximum number of iterations

  int oldcluster, newcluster;           // just for ease of coding.
  int cl, bestcl;
  long col, j;
  double temp;
  int done = 0;
  long nchange;
  int iter = 0;

  vector<double> d(k);                        // Vector of distances, internal only.
  vector<double> temp1(k);
  vector<vector<double> > tempcent(m, temp1);   // For copy of global centroids k x m

  //char filename[10];
  //int junk;
  //junk = sprintf(filename, "Cfile%d.txt", INTEGER_VALUE(ii));
  //ofstream outFile;
  //outFile.open(filename, ios::out);
  //outFile << "This is node " << INTEGER_VALUE(ii) << endl;
  //outFile << "Before do: n, i, k, m, p, start:" <<
  //       n << ", " << i << ", " << k << ", " << m << ", " << p <<
  //       ", " << start << endl;


  // Before starting the loop, we only have cent (centers) as passed into the function.
  // Calculate clust and clustsizes, then update cent as centroids.
  
  for (cl=0; cl<k; cl++) clustsizes[0][cl] = 0;
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
    clust[0][j] = bestcl + 1;
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

    iter++;
    if ( (nchange==0) || (iter>=maxiters) ) done = 1;

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

  // At this point, cent is the centers, ss is the within-groups sums of squares,
  // clust is the cluster memberships, clustsizes is the cluster sizes.

  //outFile.close();
  return iter;

}


extern "C"
{

SEXP Ckmeans2main(SEXP matType, 
                  SEXP bigMatrixAddr, SEXP centAddr, SEXP ssAddr,
                  SEXP clustAddr, SEXP clustsizesAddr,
                  SEXP nn, SEXP kk, SEXP mm, SEXP mmaxiters)
{
  SEXP ret = PROTECT(NEW_NUMERIC(1));
  BigMatrix *pMat = (BigMatrix*)R_ExternalPtrAddr(bigMatrixAddr);
  int iter = 0;
  if (pMat->separated_columns())
  {
    switch (INTEGER_VALUE(matType)) 
    {
      case 1:
        iter = Ckmeans2<char, SepMatrixAccessor<char> >(
          pMat, centAddr, ssAddr, clustAddr, clustsizesAddr,
          nn, kk, mm, mmaxiters);
        break;
      case 2:
        iter = Ckmeans2<short, SepMatrixAccessor<short> >(
          pMat, centAddr, ssAddr, clustAddr, clustsizesAddr,
          nn, kk, mm, mmaxiters);
        break;
      case 4:
        iter = Ckmeans2<int, SepMatrixAccessor<int> >(
          pMat, centAddr, ssAddr, clustAddr, clustsizesAddr,
          nn, kk, mm, mmaxiters);
        break;
      case 8:
        iter = Ckmeans2<double, SepMatrixAccessor<double> >(
          pMat, centAddr, ssAddr, clustAddr, clustsizesAddr,
          nn, kk, mm, mmaxiters);
        break;
    }
  }
  else
  {
    switch (INTEGER_VALUE(matType)) 
    {
      case 1:
        iter = Ckmeans2<char, MatrixAccessor<char> >(
          pMat, centAddr, ssAddr, clustAddr, clustsizesAddr,
          nn, kk, mm, mmaxiters);
        break;
      case 2:
        iter = Ckmeans2<short, MatrixAccessor<short> >(
          pMat, centAddr, ssAddr, clustAddr, clustsizesAddr,
          nn, kk, mm, mmaxiters);
        break;
      case 4:
        iter = Ckmeans2<int, MatrixAccessor<int> >(
          pMat, centAddr, ssAddr, clustAddr, clustsizesAddr,
          nn, kk, mm, mmaxiters);
        break;
      case 8:
        iter = Ckmeans2<double, MatrixAccessor<double> >(
          pMat, centAddr, ssAddr, clustAddr, clustsizesAddr,
          nn, kk, mm, mmaxiters);
        break;
    }
  }
  NUMERIC_DATA(ret)[0] = (double)iter;
  UNPROTECT(1);
  return ret;
}

} // extern "C"

