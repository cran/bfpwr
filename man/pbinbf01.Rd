% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pbinbf01.R
\name{pbinbf01}
\alias{pbinbf01}
\title{Cumulative distribution function of the binomial Bayes factor}
\usage{
pbinbf01(
  k,
  n,
  p0 = 0.5,
  type = c("point", "direction"),
  a = 1,
  b = 1,
  dp = NA,
  da = a,
  db = b,
  dl = 0,
  du = 1,
  lower.tail = TRUE
)
}
\arguments{
\item{k}{Bayes factor threshold}

\item{n}{Number of trials}

\item{p0}{Tested binomial proportion. Defaults to \code{0.5}}

\item{type}{Type of test. Can be \code{"point"} or \code{"directional"}.
Defaults to \code{"point"}}

\item{a}{Number of successes parameter of the beta analysis prior
distribution. Defaults to \code{1}}

\item{b}{Number of failures parameter of the beta analysis prior
distribution. Defaults to \code{1}}

\item{dp}{Fixed binomial proportion assumed for the power calculation. Set to
\code{NA} to use a truncated beta design prior instead (specified via the
\code{da}, \code{db}, \code{dl}, and \code{du} arguments). Defaults to
\code{NA}}

\item{da}{Number of successes parameter of the truncated beta design prior
distribution. Is only taken into account if \code{dp = NA}. Defaults to
the same value \code{a} as specified for the analysis prior}

\item{db}{Number of failures parameter of the truncated beta design prior
distribution. Is only taken into account if \code{dp = NA}. Defaults to
the same value \code{b} as specified for the analysis prior}

\item{dl}{Lower truncation limit of of the truncated beta design prior
distribution. Is only taken into account if \code{dp = NA}. Defaults to
\code{0}}

\item{du}{Upper truncation limit of of the truncated beta design prior
distribution. Is only taken into account if \code{dp = NA}. Defaults to
\code{1}}

\item{lower.tail}{Logical indicating whether Pr(\eqn{\mathrm{BF}_{01}}{BF01}
\eqn{\leq}{<=} \code{k}) (\code{TRUE}) or Pr(\eqn{\mathrm{BF}_{01}}{BF01}
\eqn{>} \code{k}) (\code{FALSE}) should be computed. Defaults to
\code{TRUE}}
}
\value{
The probability that the Bayes factor is less or greater (depending
    on the specified \code{lower.tail}) than the specified threshold \code{k}
}
\description{
This function computes the probability of obtaining a binomial
    Bayes factor (\link{binbf01}) more extreme than a threshold \code{k} with
    a specified sample size.
}
\examples{
## compute probability that BF > 10 under the point null
a <- 1
b <- 1
p0 <- 3/4
k <- 10
nseq <- seq(1, 1000, length.out = 100)
powH0 <- pbinbf01(k = k, n = nseq, p0 = p0, type = "point", a = a, b = b,
                  dp = p0, lower.tail = FALSE)
plot(nseq, powH0, type = "s", xlab = "n", ylab = "Power")

## compare to normal approximation
pm <- a/(a + b) # prior mean under H1
psd <- sqrt(a*b/(a + b)^2/(a + b + 1)) # prior standard deviation under H1
pownormH0 <- pbf01(k = k, n = nseq, usd = sqrt(p0*(1 - p0)), null = p0,
                   pm = pm, psd = psd, dpm = p0, dpsd = 0, lower.tail = FALSE)
lines(nseq, pownormH0, type = "s", col = 2)
legend("right", legend = c("Exact", "Normal approximation"), lty = 1,
       col = c(1, 2))

## compute probability that BF < 1/10 under the p|H1 ~ Beta(a, b) alternative
a <- 10
b <- 5
p0 <- 3/4
k <- 1/10
powH1 <- pbinbf01(k = k, n = nseq, p0 = p0, type = "point", a = a, b = b,
                  da = a, db = b, dl = 0, du = 1)
plot(nseq, powH1, type = "s", xlab = "n", ylab = "Power")

## compare to normal approximation
pm <- a/(a + b) # prior mean under H1
psd <- sqrt(a*b/(a + b)^2/(a + b + 1)) # prior standard deviation under H1
pownormH1 <- pbf01(k = k, n = nseq, usd = sqrt(pm*(1 - pm)), null = p0,
                   pm = pm, psd = psd, dpm = pm, dpsd = psd)
lines(nseq, pownormH1, type = "s", col = 2)
legend("right", legend = c("Exact", "Normal approximation"), lty = 1,
       col = c(1, 2))

## probability that directional BF <= 1/10 under uniform [3/4, 1] design prior
pow <- pbinbf01(k = 1/10, n = nseq, p0 = 3/4, type = "direction", a = 1, b = 1,
                da = 1, db = 1, dl = 3/4, du = 1)
plot(nseq, pow, type = "s", xlab = "n", ylab = "Power")
}
\seealso{
\link{binbf01}, \link{nbinbf01}
}
\author{
Samuel Pawel
}
