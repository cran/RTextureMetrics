\name{calcHOM}
\alias{calcHOM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
HOM Homogeinity
}
\description{
calculates HOMOGENEITY measure for Grey Level co-occurrence matrices 
}
\usage{
calcHOM(rawmat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rawmat}{
assigns the GLC-Matrix to use
}
}
\details{
Homogeneity weigths values by the inverse of the Contrast weight with weights decreasing exponentially away from diagonal
Homogeneity measure is sometimes also called 'Inverse Difference Moment'.
}
\value{
returns Homogeneity value
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Toennies, D., 2005: Grundlagen der Bildverabeitung, 341 S., Pearson Studium
Harralick, R.M., Shanmugam, K., Dinstein, I., 1973: Textural Features for image classification. IEEE Transactions on Systems, Man and Cybernetics, SMC vol. 3 no. 6, pp. 610-620.
}
\author{
H.-J. Klemmt
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
GLCM Tutorial by Mryka-Hall-Beyer, http://www.fp.ucalgary.ca/mhallbey/homogeneity.htm
}
\examples{
data<-c(0,0,1,1,0,0,1,1,0,2,2,2,2,2,3,3)
mat<-matrix(data, nrow=4, byrow=TRUE)
(mat)
GLCM<-genGLCM(2,1,mat)
(GLCM)
(calcHOM(GLCM))
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (rawmat) 
{
    size <- dim(rawmat)[1]
    mathomweights <- matrix(0, nrow = size, ncol = size)
    for (i in 1:size) {
        for (a in 1:size) {
            mathomweights[i, a] <- 1/(1 + (a - i)^2)
        }
    }
    hom <- rawmat * mathomweights
    return(sum(hom))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IO }
