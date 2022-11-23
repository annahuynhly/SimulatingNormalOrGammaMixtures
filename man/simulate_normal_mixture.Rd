\name{simulate_normal_mixture}
\alias{simulate_normal_mixture}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Simulate Finite Normal Mixtures}
\description{
This function simulates a finite mixture of multiple normal distributions.
}
\usage{
simulate_normal_mixture(n_count, theta, mean_exact, sd_exact)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n_count}{The number of samples, n, that we want to generate from a normal distribution.}
  \item{theta}{A vector of theta values, where sum(theta) = 1.}
  \item{mean_exact}{A vector of the respective mean values of the normal distributions.}
  \item{sd_exact}{A vector of the respective sd values of the normal distribution.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
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
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
theta=c(0.2,.3,0.5)
mean.exact=c(0,5,2)
sd.exact=c(1,0.5,3)
samples = simulate_normal_mixture(10000, theta, mean.exact, sd.exact)
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
