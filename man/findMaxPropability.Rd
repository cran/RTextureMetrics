\name{findMaxPropability}
\alias{findMaxPropability}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
findMaxPropability
}
\description{
finds and assings the largest Pij value found in a picture
}
\usage{
findMaxPropability(rawmat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rawmat}{
assigns the Grey Level Co-occurrence Matrix for Maximum Propability characterization
}
}
\details{
Maximum Probabilty is a simple statistic which records in the central pixel of a picture or window the largest found Pij value.
High values occur if one combination of pixels dominate the pixel pairs. 
}
\value{
returns Maximum Propability value
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
The GLCM Tutorial by Mryka Hall-Beyer, http://www.fp.ucalgary.ca/mhallbey/max_propabilty.htm
}
\examples{
data<-c(0,0,1,1,0,0,1,1,0,2,2,2,2,2,3,3)
mat<-matrix(data, nrow=4, byrow=TRUE)
(mat)
GLCM<-genGLCM(2,1,mat)
(GLCM)
(findMaxPropability(GLCM))
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (rawmat) 
{
    return(max(rawmat))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IO }

