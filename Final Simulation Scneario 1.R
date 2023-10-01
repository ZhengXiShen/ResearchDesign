#Calculate sample size
library(pwr)

d = (20-16)/sqrt((49+25)/2)

pwr.t.test( d = d, sig.level = 0.05, power = 0.9, 
            alternative = "greater" , type = 'two.sample')
?pwr.t.test


library(data.table)
library(DT)
library(dplyr)
set.seed(seed = 329)

#Set up one round of simulation with 200 sample size, 3 as mean and sd of 1
n <- 200
rc.dat1 <- data.table(Group = c(rep.int(x = "Treatment",
                                       times = n/2), rep.int(x = "Control", 
                                                             times = n/2)))
rc.dat1[Group == "Control", rc :=round(x = rnorm(n = n/2, mean = 12,
                                            sd = 4))]
rc.dat1[Group == "Treatment", rc :=round(x = rnorm(n = n/2,
                                              mean = 12, sd = 4))]
data = rc.dat1

analyze.experiment <- function(the.dat) {
  require(data.table)
  setDT(the.dat)
  the.test <- t.test(x = the.dat[Group == "Treatment", 
                                    rc], y = the.dat[Group == "Control", rc], alternative = "greater")
  the.effect <- the.test$estimate[1] - the.test$estimate[2]
  upper.bound <- the.test$conf.int[2]
  lower.bound <- the.test$conf.int[1]
  p <- the.test$p.value
  result <- data.table(effect = the.effect, upper_ci = upper.bound,
                       lower_ci = lower.bound, p = p)
  return(result)
}

analyze.experiment(the.dat = rc.dat1)

# Repeating the Scenario 1000 times
B <- 1000
n <- 200
RNGversion(vstr = 3.6)
set.seed(seed = 4172)

Experiment <- 1:B
Group <- c(rep.int(x = "Treatment", times = n/2), rep.int(x = "Control",
                                                          times = n/2))
sim.dat  <- as.data.table(expand.grid(Experiment = Experiment,
                                     Group = Group))
setorderv(x = sim.dat, cols = c("Experiment", "Group"), order = c(1,1))
sim.dat[Group == "Control", rc := round(x = rnorm(n = .N, mean = 16, sd = 7))]
sim.dat[Group == "Treatment", rc := round(x = rnorm(n = .N, mean = 16, sd = 5))]
dim(sim.dat)

exp.results <- sim.dat[, analyze.experiment(the.dat = .SD), 
                       keyby = "Experiment"]

scenario1_1000 <- DT::datatable(data = round(x = exp.results[1:100, ], digits = 3), 
                                rownames = F)

# Percentage of False positive
exp.results[, mean(p < 0.05)]

# Percentage of True negative
1 - exp.results[, mean(p < 0.05)]


# Summary of P-Value
exp.results[, summary(p)]


# Summary of difference for prop-test
exp.results[, summary(effect)]

# Mean effect of the simulated data
exp.results[, mean(effect)]

# Summary of upper confidence interval of mean effect
exp.results[, summary(upper_ci)]

# Summary of lower confidence interval of mean effect
exp.results[, summary(lower_ci)]

