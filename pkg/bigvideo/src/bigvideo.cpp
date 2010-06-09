#include "bigmemory/BigMatrix.h"
#include "bigmemory/MatrixAccessor.hpp"
#include "bigmemory/isna.hpp"
#include "bigmemory/util.h"

#include <utility>
#include <vector>
#include <algorithm>
#include <map>
#include <boost/shared_ptr.hpp>
#include <iostream>
#include <sstream>
#include <stdexcept>

#include "cv.h"
#include "highgui.h"

#include <math.h>

#include <R.h>
#include <Rdefines.h>

//#include <iostream>            // Why is this here twice?

using namespace cv;
using namespace std;

template<typename T, typename MatrixAccessorType>
SEXP recordvideo(MatrixAccessorType m, SEXP width, SEXP height,
                 SEXP frames, SEXP start, SEXP color)
{
  SEXP ret = PROTECT(NEW_LOGICAL(1));
  LOGICAL_DATA(ret)[0] = (Rboolean) 1;
  int i, j, k;
  Mat frame, edges;
  uchar *pRow;
  VideoCapture cap(0); // open the default camera
  namedWindow("Camera", 1);
  if(!cap.isOpened()) {
    printf("opencv: unable to open camera device.\n");
    LOGICAL_DATA(ret)[0] = (Rboolean) 0;
    UNPROTECT(1);
    return ret;
  } else {
    if (LOGICAL_VALUE(color)==0) {
      for (i=0; i<INTEGER_VALUE(frames); i++) {
        cap >> frame; // get a frame from camera
        cvtColor(frame, edges, CV_BGR2GRAY); // for grayscale
        imshow("Camera", edges);
        for (j=0; j < INTEGER_VALUE(height); ++j) {
          pRow = edges.ptr(j);
          for (k=0; k < INTEGER_VALUE(width); ++k) {
            m[INTEGER_VALUE(start)-1+i][j*INTEGER_VALUE(width)+k] =
              (T) pRow[k];
          }
        }
        if(waitKey(20) >= 0) break;
      }
    } else {
      /*                  // COLOR DATA STRUCTURE???
      for (i=0; i<T[0]; i++) {
        cap >> frame; // get a frame from camera
        imshow("Camera", frame);
        if(waitKey(20) >= 0) break;
      }
      */
    }
  }
  UNPROTECT(1);
  return ret;
}


extern "C" {

void Cprobecamera(int *Width, int *Height,
                  int *elemSize, int *channelSize) {

  Size frameSize, edgesSize;
  VideoCapture cap(0); // open the default camera
  if(!cap.isOpened()) {
    printf("opencv: unable to open camera device.\n");
    Width[0] = -1;
  } else {
    Mat frame, edges;
    cap >> frame; // get a frame from camera
    cvtColor(frame, edges, CV_BGR2GRAY); // for grayscale
    edgesSize = edges.size();
    frameSize = frame.size();

    Width[0] = (int) edgesSize.width;
    Height[0] = (int) edgesSize.height;
    Width[1] = (int) frameSize.width;
    Height[1] = (int) frameSize.height;

    elemSize[0] = (int) edges.elemSize();
    channelSize[0] = (int) edges.channels();
    elemSize[1] = (int) frame.elemSize();
    channelSize[1] = (int) frame.channels();
  }
}

void Ctestcamera(int *color, int *T) {

  int i;
  Mat frame, edges;
  VideoCapture cap(0); // open the default camera
  namedWindow("Camera", 1);
  if(!cap.isOpened()) {
    printf("opencv: unable to open camera device.\n");
    color[0] = -1;
  } else {
    if (color[0]==0) {
      for (i=0; i<T[0]; i++) {
        cap >> frame; // get a frame from camera
        cvtColor(frame, edges, CV_BGR2GRAY); // for grayscale
        imshow("Camera", edges);
        if(waitKey(20) >= 0) break;
      }
    } else {
      for (i=0; i<T[0]; i++) {
        cap >> frame; // get a frame from camera
        imshow("Camera", frame);
        if(waitKey(20) >= 0) break;
      }
    }
  }
}

// The following handles the type templating for big.matrix
// objects.
SEXP CPPrecordvideoBigMatrix(SEXP addr, SEXP width, SEXP height,
                    SEXP frames, SEXP start, SEXP color)
{
  BigMatrix *pMat = reinterpret_cast<BigMatrix*>(
    R_ExternalPtrAddr(addr));
  if (pMat->separated_columns())
  {
    switch (pMat->matrix_type())
    {
      case 1:
        return recordvideo<char>(SepMatrixAccessor<char>(*pMat), 
          width, height, frames, start, color);
      case 2:
        return recordvideo<short>(SepMatrixAccessor<short>(*pMat), 
          width, height, frames, start, color);
      case 4:
        return recordvideo<int>(SepMatrixAccessor<int>(*pMat), 
          width, height, frames, start, color);
      case 8:
        return recordvideo<double>(SepMatrixAccessor<double>(*pMat), 
          width, height, frames, start, color);
    }
  }
  else
  {
    switch (pMat->matrix_type())
    {
      case 1:
        return recordvideo<char>(MatrixAccessor<char>(*pMat), 
          width, height, frames, start, color);
      case 2:
        return recordvideo<short>(MatrixAccessor<short>(*pMat), 
          width, height, frames, start, color);
      case 4:
        return recordvideo<int>(MatrixAccessor<int>(*pMat), 
          width, height, frames, start, color);
      case 8:
        return recordvideo<double>(MatrixAccessor<double>(*pMat), 
          width, height, frames, start, color);
    }
  }
  return R_NilValue;
}

SEXP CPPinitcamera(SEXP color, SEXP T) {
  SEXP ret = PROTECT(NEW_LOGICAL(1));
  LOGICAL_DATA(ret)[0] = (Rboolean) 1;

  int i;
  Mat frame, edges;
  VideoCapture cap(0); // open the default camera
  namedWindow("Camera", 1);
  if(!cap.isOpened()) {
    printf("opencv: unable to open camera device.\n");
    LOGICAL_DATA(ret)[0] = (Rboolean) 0;
  } else {
    if (LOGICAL_VALUE(color)==FALSE) {
      for (i=0; i<INTEGER_VALUE(T); i++) {
        cap >> frame; // get a frame from camera
        cvtColor(frame, edges, CV_BGR2GRAY); // for grayscale
        imshow("Camera", edges);
        if(waitKey(5) >= 0) break;
      }
    } else {
      for (i=0; i<INTEGER_VALUE(T); i++) {
        cap >> frame; // get a frame from camera
        imshow("Camera", frame);
        if(waitKey(1) >= 0) break;
      }
    }
  }

  UNPROTECT(1);
  return ret;
}

} // End extern
     

