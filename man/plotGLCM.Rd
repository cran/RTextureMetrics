\name{plotGLCM}
\alias{plotGLCM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot (plot GLC-Matrix)
}
\description{
\code{plotGLCM} plots a Grey Level Co-occurrence Matrix (GLCM)
}
\usage{
plotGLCM(GLCM)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{GLCM}{
 data frame with GLCM data
}

}

\details{
GLCM is a tabulation of how often different combinations of pixel brightness values (grey levels) occur in an image.
}
\value{
no return value
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Toennies, D., 2005: Grundlagen der Bildverarbeitung. Pearson Studium, 341 S. 
Harralick, R.M., Shanmugam, K., Dinstein, I., 1973: Textural Features for image classification. IEEE Transactions on Systems, Man and Cybernetics, SMC vol. 3 no. 6, pp. 610-620.
}
\author{
H.-J. Klemmt
}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
data<-c(0,0,1,1,0,0,1,1,0,2,2,2,2,2,3,3)
mat<-matrix(data,nrow=4, byrow=TRUE)
GLCM<-genGLCM(2,1,mat)
plotGLCM(GLCM)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IO }

