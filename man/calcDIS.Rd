\name{calcDIS}
\alias{calcDIS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
DIS Dissimilarity
}
\description{
calculates DISSIMILARITY measure for Grey Level Co-occurrence Matrices
}
\usage{
calcDIS(rawmat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rawmat}{
assigns the GLC-Matrix for DISSIMILARITY calculation 
}
}
\details{
DISSIMILARITY measure belongs to the Contrast group of texture metrics. In the DISSIMILARITY measure weights increase linearly.
}
\value{
returns the DISSIMILARITY measure
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Toennies, D., 2005: Grundlagen der Bildverarbeitung, 341 S., Pearson Studium
Harralick, R.M., Shanmugam, K., Dinstein, I., 1973: Textural Features for image classification. IEEE Transactions on Systems, Man and Cybernetics, SMC vol. 3 no. 6, pp. 610-620.
}
\author{
H.-J. Klemmt
}


%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
data<-c(0,0,1,1,0,0,1,1,0,2,2,2,2,2,3,3)
mat<-matrix(data, nrow=4, byrow=TRUE)
(mat)
GLCM<-genGLCM(2,1,mat)
(GLCM)
(calcDIS(GLCM))
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (rawmat) 
{
    size <- dim(rawmat)[1]
    matconweights <- matrix(0, nrow = size, ncol = size)
    for (i in 1:size) {
        for (a in 1:size) {
            matconweights[i, a] <- abs(a - i)
        }
    }
    dis <- rawmat * matconweights
    return(sum(dis))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IO }

