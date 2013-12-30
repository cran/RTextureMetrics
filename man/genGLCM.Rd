\name{genGLCM}
\alias{genGLCM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
genGLCM (generate GLC-Matrix)
}
\description{
\code{genGLCM} generates a Grey Level Co-occurrence Matrix (GLCM)
}
\usage{
genGLCM(direction, distance, rawmat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{direction}{
 assigns the direction of the neighbour (1=east, right), (2=south, down), (3=west, left), (4=north, up)
}
  \item{distance}{
 assigns the number of pixels between reference and neighbour
}
  \item{rawmat}{
  assigns the grey level [0..255] pixel matrix of a picture
}
}
\details{
GLCM is a tabulation of how often different combinations of pixel brightness values (grey levels) occur in an image.
}
\value{
returns a normalized propability matrix of the occurrence of different grey values
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
(GLCM)


## The function is currently defined as
function (direction, distance, rawmat) 
{
    number_coloums <- max(rawmat) + 1
    number_rows <- max(rawmat) + 1
    GLCM <- matrix(0, ncol = 255, nrow = 255)
    (GLCM)
    if (direction == 1) {
        for (i in 1:number_coloums - 1) {
            for (a in 1:number_rows) {
                GLCM[rawmat[a, i] + 1, rawmat[a, i + 1] + 1] <- GLCM[rawmat[a, 
                  i] + 1, rawmat[a, i + 1] + 1] + 1
            }
        }
    }
    if (direction == 2) {
        for (i in 1:number_coloums) {
            for (a in 1:number_rows - 1) {
                GLCM[rawmat[a, i] + 1, rawmat[a + 1, i] + 1] <- GLCM[rawmat[a, 
                  i] + 1, rawmat[a + 1, i] + 1] + 1
            }
        }
    }
    transGLCM <- t(GLCM)
    print("INVERTIERT")
    GLCM <- GLCM + transGLCM
    GLCMprob <- round(GLCM/sum(GLCM), digits = 4)
    return(GLCMprob)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IO }

