\name{calcASM}
\alias{calcASM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ASM (Angular Second Moment)
}
\description{
calculates Angular Second Moment (ASM) measure
}
\usage{
calcASM(rawmat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rawmat}{
assigns the Grey Level Co-occurrence Matrix for ASM calculation
}
}
\details{
Angular Second Moment (ASM) measure belongs to the orderlines group of texture measures. It is sometimes also called Energy
or Uniformity. High values occur when the window ist very orderly
}
\value{
returns ASM value
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
The GLCM-Tutorial by Mryka Hall-Beyer, http://www.fp.ucalgary.ca/mhallbey/asm.htm
}
\examples{
data<-c(0,0,1,1,0,0,1,1,0,2,2,2,2,2,3,3)
mat<-matrix(data, nrow=4, byrow=TRUE)
(mat)
GLCM<-genGLCM(2,1,mat)
(GLCM)
(calcASM(GLCM))
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (rawmat) 
{
    return(sum(rawmat^2))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IO }

