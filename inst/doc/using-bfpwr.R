## ----"knitr-options", echo = FALSE--------------------------------------------
library(knitr)
opts_chunk$set(fig.height = 4.5,
               fig.align = "center",
               cache = FALSE,
               message = FALSE,
               warning = FALSE)

## ----"installation", eval = FALSE---------------------------------------------
# install.packages("bfpwr")

## ----"load"-------------------------------------------------------------------
library("bfpwr")

## ----"bf01-illustration", echo = TRUE-----------------------------------------
## glm to quantify association between Virginica species and sepal width
iris$virginica <- as.numeric(iris$Species == "virginica")
irisglm <- glm(virginica ~ Petal.Width, data = iris, family = "binomial")
estimate <- summary(irisglm)$coefficients[2,1] # logOR estimate
se <- summary(irisglm)$coefficients[2,2] # standard error

## Bayes factor parameters
null <- 0 # null value
pm <- 0 # analysis prior mean
psd <- 2 # analysis prior sd

## compute z-test Bayes factor
bf01(estimate = estimate, se = se, null = null, pm = pm, psd = psd)

## ----"powerbf01-illustration", echo = TRUE------------------------------------
## Bayes factor and sample size parameters
null <- 0 # null value
pm <- 0.3 # analysis prior mean
psd <- 1 # analysis prior sd
k <- 1/10 # desired Bayes factor threshold (BF01 < k)
type <- "two.sample" # two-sample z-test
sd <- 1 # sd of one observation, set to 1 for standardized mean difference scale

## compute sample size to achieve desired power
pow <- 0.9
(res1 <- powerbf01(k = k, power = pow, sd = sd, null = null, pm = pm, psd = psd,
                   type = type))

## compute power for a given sample size
n <- 500
(res2 <- powerbf01(k = k, n = n, sd = sd, null = null, pm = pm, psd = psd,
                   type = type))

## ----"plot.powerbf01-illustration", echo = TRUE, fig.height = 6---------------
## plot power curves (also tweaking the limits and resolution of the x-axis)
plot(res1, nlim = c(1, 3000), ngrid = 1000)

## ----"prior-illustration", fig.height = 3.5, echo = FALSE---------------------
tprior <- function(d,
                   plocation = 0,
                   pscale = 1,
                   pdf = 1,
                   alternative = "two.sided") {
    if (alternative == "two.sided") {
        normConst <- 1
        lower <- -Inf
        upper <- Inf
    }
    else if (alternative == "greater") {
        normConst <- 1 - stats::pt(q = (0 - plocation)/pscale, df = pdf)
        lower <- 0
        upper <- Inf
    }
    else {
        normConst <- stats::pt(q = (0 - plocation)/pscale, df = pdf)
        lower <- -Inf
        upper <- 0
    }
    stats::dt(x = (d - plocation)/pscale, df = pdf, ncp = 0)/
        (pscale*normConst)*as.numeric(d >= lower)*as.numeric(d <= upper)
}

dseq <- seq(-3, 3, 0.01)
pars <- data.frame(plocation = c(0, 0, 1), pscale = c(0.7, 0.7, 0.3), pdf = c(1, 1, 30),
                   alternative = c("two.sided", "greater", "two.sided"))
dens <- sapply(X = seq(1, nrow(pars)), FUN = function(i) {
    d <- tprior(d = dseq, plocation = pars$plocation[i], pscale = pars$pscale[i],
                pdf = pars$pdf[i], alternative = pars$alternative[i])
})
cols <- palette.colors(n = 4, alpha = 0.8)[-1]
oldpar <- par(mar = c(4, 5, 2, 2))
matplot(dseq, dens, lty = 1, type = "l", lwd = 1.5, las = 1, col = cols,
        xlab = "Standardized mean difference", ylab = "Prior density",
        panel.first = grid(lty = 3, col = adjustcolor(col = 1, alpha = 0.1)),
        ylim = c(0, 2))
leg <- sapply(X = seq(1, nrow(pars)),
              FUN = function(i) paste(names(pars), pars[i,], sep =  " = ",
                                      collapse = ", ")
              )
legend("topright", legend = leg, col = cols, lwd = 1.5, lty = 1, cex = 0.8)
par(oldpar)

## ----"tbf01-illustration", echo = TRUE----------------------------------------
## paired t-test JZS Bayes factor analyses from Rouder et al. (2009, p.232)
tbf01(t = 2.03, n = 80, plocation = 0, pscale = 1, pdf = 1, type = "paired")

## informed prior analysis from Gronau et al. (2020, Figure 1)
tbf01(t = -0.90, n1 = 53, n2 = 57, plocation = 0.350, pscale = 0.102, pdf = 3,
      alternative = "greater", type = "two.sample")

## ----"powertbf01-illustration", echo = TRUE-----------------------------------
## determine sample size for a given power
(tres <- powertbf01(k = 1/6, # Bayes factor threshold
                    power = 0.95, # target power
                    type = "two.sample", # two-sample test
                    ## normal design prior
                    dpm = 0.5, # design prior mean at d = 0.5
                    dpsd = 0, # point design prior (standard deviation is zero)
                    ## directional JSZ analysis prior
                    plocation = 0,
                    pscale = 1/sqrt(2),
                    pdf = 1,
                    alternative = "greater"))

## ----"plot-powertbf01-illustration", fig.height = 6---------------------------
plot(tres)

## ----"nm-prior-illustration", fig.height = 4, echo = FALSE--------------------
dnmoment <- function(x, location = 0, spread = 1) {
    stats::dnorm(x = x, mean = location, sd = spread)*(x - location)^2/spread^2
}
xseq <- seq(-4, 4, length.out = 500)
taus <- c(0.5, 1, 2)
null <- 0
dens <- sapply(X = taus, FUN = function(tau) dnmoment(x = xseq, location = 0, spread = tau))
cols <- hcl.colors(n = length(taus), alpha = 0.8)
oldpar <- par(mar = c(4, 5, 2, 2))
matplot(xseq, dens, type = "l", lty = 1, col = cols, lwd = 1.5,
        xlab = bquote("Parameter" ~ theta), ylab = "Density", las = 1,
        panel.first = grid(lty = 3, col = adjustcolor(col = 1, alpha = 0.1)))
leg <- paste("null = 0, psd =", taus)
legend("topright", legend = leg, col = cols, lty = 1, lwd = 1.5, cex = 0.8,
       bg = "white")
par(oldpar)

## ----"sessionInfo", echo = TRUE-----------------------------------------------
cat(paste(Sys.time(), Sys.timezone(), "\n"))
sessionInfo()

