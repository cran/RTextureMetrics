\name{calcENT}
\alias{calcENT}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ENT Entropy
}
\description{
calculates ENTROPY measure for Grey Level Co-occurrence Matrices
}
\usage{
calcENT(rawmat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rawmat}{
assigns the GLC-Matrix 
}
}
\details{
Entropy is a texture measure related to the orderline group. Entropy characterizes 'energy' values for pixel combinations.
}
\value{
returns Entropy value
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Toennies, D., 2005: Grundlagen der Bildverarbeitung. 341 S., Pearson Studium
Harralick, R.M., Shanmugam, K., Dinstein, I., 1973: Textural Features for image classification. IEEE Transactions on Systems, Man and Cybernetics, SMC vol. 3 no. 6, pp. 610-620.
}
\author{
H.-J. Klemmt
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
The GLCM Tutorial by Myrka Hall-Beyer, http://www.fp.ucalgary.ca/mhallbey/oderlines_group.htm 
}
\examples{
data<-c(0,0,1,1,0,0,1,1,0,2,2,2,2,2,3,3)
mat<-matrix(data, nrow=4, byrow=TRUE)
(mat)
GLCM<-genGLCM(2,1,mat)
(GLCM)
(calcENT(GLCM))
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (rawmat) 
{
    lnrawmat <- log(rawmat)
    size <- dim(lnrawmat)[1]
    for (i in 1:size) {
        for (a in 1:size) {
            if (lnrawmat[a, i] == "-Inf") {
                lnrawmat[a, i] <- 0
            }
        }
    }
    return(sum(rawmat * lnrawmat))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IO }
