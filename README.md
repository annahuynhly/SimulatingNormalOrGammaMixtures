# SimulatingNormalOrGammaMixtures
Simulating finite mixtures of normal or gamma distributions from the standard uniform distribution.

The goal of this library is for users to easily generate finite mixture of normal or gamma distributions without having to write a bunch of if, else if, and else statements.\
\
To install, please do the following:\
If you haven't installed devtools already, please do: install.packages("devtools")\
Then:\
library(devtools)\
install_github("annahuynhly/FiniteMixturesSimulation")\
If the above doesn't work, try:\
devtools::install_github("annahuynhly/FiniteMixturesSimulation")\
Now, finally you need to call my library.
library("FiniteNormOrGammaMix")
\
Here's an example of how to use:\
theta=c(0.2,.3,0.5)\
mean.exact=c(0,5,2)\
sd.exact=c(1,0.5,3)\
samples = simulate_normal_mixture(10000, theta, mean.exact, sd.exact)\
You may construct your own histograms and density plots to see what this generated!\
