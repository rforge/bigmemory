#include "bigmemory/BigMatrix.h"
#include "bigmemory/MatrixAccessor.hpp"

#include "cv.h"
#include "highgui.h"

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

#include <iostream>

using namespace cv;
using namespace std;

void CDestroyBigMatrix(SEXP bigMatrixAddr)
{
  BigMatrix *pm=(BigMatrix*)(R_ExternalPtrAddr(bigMatrixAddr));
  delete pm;
  R_ClearExternalPtr(bigMatrixAddr);
}


extern "C" {

SEXP GrabVideo( SEXP T ) {
  VideoCapture cap(0); // open the default camera
  if(!cap.isOpened()) {
    printf("Unable to open camera device!\n");
//    return NULL;
  }
 
  LocalBigMatrix *bm = new LocalBigMatrix();
  Mat edges; 
  namedWindow("output window", 1);
  int i, j, k;
  Size frameSize;
  uchar *pRow;
  for(i=0; i < INTEGER_VALUE(T); ++i)
  {
        Mat frame;
        cap >> frame; // get a new frame from camera
        cvtColor(frame, edges, CV_BGR2GRAY);
        if (i==0) {
          frameSize = edges.size();
          size_t pixelSize = edges.elemSize();
          int channelSize = edges.channels();
          cout << "Frame size is " << frameSize.width << " by " << 
            frameSize.height << endl;
          cout << "elemSize is " << pixelSize << endl;
          cout << "channelsSize is " << channelSize << endl;
          bm->create( frameSize.width*frameSize.height, INTEGER_VALUE(T),
            1, false );
        }
        MatrixAccessor<char> bigmat(*bm);
        for (j=0; j < frameSize.height; ++j)
        {
          pRow = edges.ptr(j);
          for (k=0; k < frameSize.width; ++k)
          {
            bigmat[i][j*frameSize.width+k] = (char)pRow[k];
          }
        }
//       GaussianBlur(edges, edges, Size(7,7), 1.5, 1.5);
//       Canny(edges, edges, 0, 30, 3);
//        imshow("edges", edges);
        imshow("output window", frame);
//        if(waitKey(30) >= 0) break;
        if(waitKey(20) >= 0) break;
    }
  SEXP ret = R_MakeExternalPtr( dynamic_cast<BigMatrix*>(bm),
      R_NilValue, R_NilValue);
    R_RegisterCFinalizerEx(ret, (R_CFinalizer_t) CDestroyBigMatrix,
      (Rboolean) TRUE);

  return ret;
}

}
