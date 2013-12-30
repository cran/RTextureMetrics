\name{calcCON}
\alias{calcCON}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
CON Contrast
}
\description{
calculates CONTRAST measure for Grey-Level Co-occurrence Matrices
}
\usage{
calcCON(rawmat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rawmat}{
assigns Grey Level Co-occurrence Matrix for calculation of CONTRAST measure
}
}
\details{
CONTRAST measure belongs to the Contrast group of texture metrics. CONTRAST measure is also called sum of square variances
}
\value{
returns CONTRAST measure
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

\seealso{
The GLCM Tutorial by Mryka Hall-Beyer, http://www.fp.ucalgary.ca/mhallbey/contrast.htm
}
\examples{
data<-c(0,0,1,1,0,0,1,1,0,2,2,2,2,2,3,3)
mat<-matrix(data, nrow=4, byrow=TRUE)
(mat)
GLCM<-genGLCM(2,1,mat)
(GLCM)
(calcCON(GLCM))
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (rawmat) 
{
    size <- dim(rawmat)[1]
    matconweights <- matrix(0, nrow = size, ncol = size)
    for (i in 1:size) {
        for (a in 1:size) {
            matconweights[i, a] <- (a - i)^2
        }
    }
    con <- rawmat * matconweights
    return(sum(con))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IO }

