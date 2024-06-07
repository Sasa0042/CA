
library('aplore3')
data("polypharm")

data <- polypharm[, c(1:4,11,12,14)]

data$gender <- ifelse( data$gender == 'Male', yes =  1, no = 0)
data$race <- ifelse( data$race == 'White', yes =  0, no = 1)

data$mhv1 <- ifelse( data$mhv4 == '1-5', yes =  1, no = 0)
data$mhv2 <- ifelse( data$mhv4 == '6-14', yes =  1, no = 0)
data$mhv3 <- ifelse( data$mhv4 == '> 14', yes =  1, no = 0)

data$inptmhv <- ifelse( data$inptmhv3 != '0', yes =  1, no = 0)

data1 <- data[, c(1,2,5:11)]


# Model fitting with brms --------------------------------------------------------------------

library('brms')
library('rstan')

nlform <- bf(polypharmacy ~ gender+race+age+mhv1+mhv2+mhv3+inptmhv+(1|id))

nlprior <- c( 
  #set_prior('normal(0, 10)', class = "Intercept"),
  set_prior('normal(0, 10)', class = "b"),
  set_prior('normal(0, exp(2*tau))', class = 'sd', group = 'id'), # here is where we add tau as a hyperparameter;
  set_prior("target += normal_lpdf(tau | 0, 10)", check = FALSE) # here is where we define the prior for tau;
)

stanvars <- stanvar(scode = " real<lower=0> tau;",  # here is where we add the parameter for tau
                    block = "parameters")

fit <- brm( formula = nlform,
            data = data1, 
            family = bernoulli(link = 'logit'),
            prior = nlprior,
            stanvars = stanvars,
            warmup = 1000, iter = 2000, chains = 4,
            control = list(adapt_delta = 0.95)
            )
summary(fit, waic = TRUE)


## To visually investigate the chains as well as the posterior distributions
plot(fit, , nvariables = 4)
plot(fit, , variable = c("b_mhv1", "b_mhv2", "b_mhv3", "b_inptmhv"))

## obtain the posterior samples
draws <- as.data.frame(as_draws_array(fit))


# Model checking for prior -------------------------------------------------------------

## If the model were true, we would expect any single simulation draw of the vectors of
## ui(random intercept) parameters to look like independent draws from the its prior
## distribution. 


## 选取四倍数列
draws_4 <- draws[, seq(0,ncol(draws),4)] 

#library(dplyr)
u <- as.vector( t(sample_n(draws_4, 1)) )
#boxplot(u)
outlier <- u > quantile(u, 0.75)+1.5*IQR(u) | u < quantile(u, 0.25)-1.5*IQR(u)
u <- u[!outlier]
hist(u)

plot(density(u), xlim=c(-10,10))
x_values <- seq(-10, 10, length = 500 )
y_values <- dnorm(x_values, mean = 0, sd = 2.5)
lines(x_values, y_values, col='red' )

legend(x = "topright", box.col = "brown",
       box.lwd = 2 ,  
       legend=c("a single draw", "N(0,(2.5)^2"), 
       fill = c("black","red"))


## It suggests that the prior for random intercept can be normal with mean 0 and sd 2.5.

nlform <- bf(polypharmacy ~ gender+race+age+mhv1+mhv2+mhv3+inptmhv+(1|id))

nlprior <- c( 
  set_prior('normal(0, 10)', class = "Intercept"),
  set_prior('normal(0, 10)', class = "b"),
  set_prior('normal(0, 2.5)', class = 'sd', group = 'id')
)

fit <- brm( formula = nlform,
            data = data1, 
            family = bernoulli(link = 'logit'),
            prior = nlprior,
            warmup = 1000, iter = 2000, chains = 4
)
summary(fit, waic = TRUE)



# predicting --------------------------------------------------------------

## predict responses
newdata <- data.frame(Trt = c(0, 1), zAge = 0, zBase = 0)
predict(fit1, newdata = newdata, re_formula = NA)

## While the predict method returns predictions of the responses, 
## the fitted method returns predictions of the regression line.

fitted(fit1, newdata = newdata, re_formula = NA)
## Both methods return the same estimate (up to random error), while the latter 
## has smaller variance, because the uncertainty in the regression line is smaller
## than the uncertainty in each response. 



# Model checking with DHARMa----------------------------------------------------------

library('DHARMa')

check_brms <- function(model,             # brms model
                       integer = FALSE,   # integer response? (TRUE/FALSE)
                       plot = TRUE,       # make plot?
                       ...                # further arguments for DHARMa::plotResiduals 
) {
  
  mdata <- brms::standata(model)
  if (!"Y" %in% names(mdata))
    stop("Cannot extract the required information from this brms model")
  
  dharma.obj <- DHARMa::createDHARMa(
    simulatedResponse = t(brms::posterior_predict(model, ndraws = 1000)),
    observedResponse = mdata$Y, 
    fittedPredictedResponse = apply(
      t(brms::posterior_epred(model, ndraws = 1000, re.form = NA)),
      1,
      mean),
    integerResponse = integer)
  
  if (isTRUE(plot)) {
    plot(dharma.obj, ...)
  }
  
  invisible(dharma.obj)
}


model.check <- check_brms(fit, integer = TRUE)
testOutliers(model.check, alternative = "two.sided", margin = "both", 
             type = "bootstrap", nBoot = 100, plot = T)
testDispersion(model.check)



# brms model checking -----------------------------------------------------

methods(class = "brmsfit")

## Compute a Bayesian version of R-squared for regression models
bayes_R2(fit)

loo(fit) ## leave-one-out (LOO) cross-validation.

pp_check(fit, ndraws = 500) ## posterior predictive check.


