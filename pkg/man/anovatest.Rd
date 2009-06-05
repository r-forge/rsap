\name{anova.test}
\alias{anova.test}
\title{Analysis of Variance}
\description{
 'anova.test' is used to compare two or more population means. As part of a complete package, all methods functions have been implemented for a more complete analysis.

}
\usage{
anova.test(xbar, sd, n, alpha)
}
\arguments{
	\item{xbar}{a vector of the observed means. Use the c(x1,x2,..,xn) command to enter the means where x1...xn values are numbers}
	\item{sd}{a vector of the observed standard deviations. Use the c(sd1,sd2,...,sdn) command to enter the standard deviations where sd1..sdn values are numbers}
	\item{n}{a vector of the sample sizes of the observed populations. Use the c(n1,n2,...,nn) command to enter the population sizes where n1...n1 values are numbers}
	\item{alpha}{the type I error rate that the user of the function wishes to maintain. It is also known as the significance level.
}
\details{this function compares the population means through the estimates of variance of the populations. The ratio of the estimates will follow Fisher's F-distribution. The expected value of this ratio is one, however, if the means differ, then the ratio will be larger than one. How far away one must get from the statistic in order to reject the null hypothesis is determined by the significance level.
}
\value{
	'anova.test' returns three objects of class 'numeric'. The three objects are the observed Fisher's F-Statistic, the critical value that one must reach to reject the null hypothesis, and the probability of observing an F-Statistic as large or larger than the observed value.
}
\author{Alex Momich}
\keyword{stats}