\name{simulate_gamma_mixture}
\alias{simulate_gamma_mixture}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Simulate Finite Gamma Mixtures}
\description{
This function simulates a finite mixture of multiple gamma distributions.
}
\usage{
simulate_gamma_mixture(n_count, theta, shapes, scales)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n_count}{The number of samples, n, that we want to generate from a gamma distribution.}
  \item{theta}{A vector of theta values, where sum(theta) = 1.}
  \item{shapes}{A vector of the shape parameter of the gamma distribution.}
  \item{scales}{A vector of the scale parameter of the gamma distribution.}
}
\details{
Please see R documentation for what shape and scale represents exactly for the gamma distribution (functions dgamma, pgamma, qgamma, rgamma.)
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Anna Ly
}
\note{
Although rate is an alternative way to specify the scale, it is not supported within this function.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
n = 10^5
theta = (1:5)/15
shapes = c(3, 3, 3, 3, 3)
scales = c(1, 2, 3, 4, 5)
samples = simulate_gamma_mixture(n, theta, shapes, scales)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
