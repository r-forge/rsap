\name{t_test}
\alias{t_test}
\title{2 Sample t-Test}
\description{
  Calculates and displays two sample t-test with intermediate calculations   
}
\usage{
t_test(xbar,sd,n,var.equal=F,alpha=0.05,alternative = c("two.sided", 
    "less", "greater"), method = c("p.value", "conf.int", "crit.val"))

S3 method for formula

t_test(formula,data,subset,var.equal=F,alpha=0.05,alternative = c("two.sided", 
    "less", "greater"), method = c("p.value", "conf.int", "crit.val"))


}
\arguments{
  \item{xbar}{vector containing the two sample means}
  \item{sd}{vector of two sample standard deviations}
  \item{n}{vector of sample size for both samples}
  \item{formula}{standard formula argument of the form \emph{response~group} }
  \item{data}{data set to use for analysis}
  \item{subset}{vector specifying the observations to be used}
  \item{var.equal}{logical specifying whether to assume equality of
    variances}
  \item{alpha}{specification of the Type I error rate}
  \item{alternative}{string indicating whether to conduct a two-sided or
    a single sided alternative}
  \item{method}{string indicating whether to use the p-value, critical
    value, or confidence interval approach to carry out the test}
}
\details{
  This function performs the standard two sample t-test but includes in
  the output all of the intermediate details including the critical
  values, the degrees of freedom, and the pooled standard deviation.
  The output is displayed using the Hypothesis testing logic/framework.  
}
\value{
  t_test writes out the standard notation for the hypotheses for the
  population mean.  Next it displays the appropriate decision rule for
  each of the different methods.  Then the function returns the summary
  statistics and all the nescicary calculations.  Finally, it states the
  statistical conclusion in terms of the hypotheses. 


}

\seealso{
}
\author{Timothy Hess
  \email{hesst@ripon.edu}}
\examples{
data(sleep)
t_test(extra~group,data=sleep)  # 2-sided p-value approach

t_test(extra~group,data=sleep,method="crit.val")  # 2-sided critical value approach


}

\keyword{stats}
