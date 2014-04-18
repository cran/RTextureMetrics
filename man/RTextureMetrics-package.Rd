\name{RTextureMetrics-package}
\alias{RTextureMetrics-package}
\alias{RTextureMetrics}
\docType{package}
\title{
Calculation of texture metrics for Grey Level Co-occurrence matrices
}

\description{
This package contains several functions for calculation of important texture metrics for Grey Level Co-occurrence matrices
}

\details{
\tabular{ll}{
Package: \tab RTextureMetrics\cr
Type: \tab Package\cr
Version: \tab 1.1\cr
Date: \tab 2013-04-18\cr
License: \tab GPL (>=2)\cr
LazyLoad: \tab yes\cr
}
Read in Image (e.g. using readJPEG in biOps-package or readJPEG in rimage-package) and transform Image to a Grey-Image (e.g. using rgb2Grey-function of biOps or RGB2GREY in rimage).
Use afterwards genGLCM of this package to generate a GLC-Matrix and calculate afterwards (more than) one of the measures. 
}

\author{Hans-Joachim Klemmt, Bayerische Landesanstalt fuer Wald und Forstwirtschaft, (Bavarian Institute of Forestry), Freising\cr
Maintainer: Hans-Joachim Klemmt \cr
<hans-joachim.klemmt@lwf.bayern.de>
}

\references{
Toennies, D., 2005: Grundlagen der Bildverarbeitung, 341 S., Pearson Studium
Harralick, R.M., Shanmugam, K., Dinstein, I., 1973: Textural Features for image classification. IEEE Transactions on Systems, Man and Cybernetics, SMC vol. 3 no. 6, pp. 610-620.
}

\keyword{ package }

\examples{
data<-c(0,0,1,1,0,0,1,1,0,2,2,2,2,2,3,3)
mat<-matrix(data, nrow=4, byrow=TRUE)
(mat)
GLCM<-genGLCM(2,1,mat)
(GLCM)
calcCON(GLCM)
calcHOM(GLCM)
calcDIS(GLCM)
calcASM(GLCM)
calcENT(GLCM)
findMaxPropability(GLCM)
plotGLCM(GLCM)
}
