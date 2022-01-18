#writing it with a linear model to understand the logic.
# Faking condit data
# Passing through MLE function
# Dec 2021

data = data.frame(x = runif(50, 0, 30), y = runif(50, 0, 30))

#############
# Functions
#############

#setting up functional form for iterations
model_iter <- function(pars, data) {
  pars[1] * data$x + pars[2] * data$y
}

#negative log likelihood
NLL <- function(pars, data) {
  # Values prediced by the model

  if(tail(pars, 1) < 0){ #avoiding NAs by keeping the st dev positive
    return(-Inf)
  }
  pred <- model_iter(pars, data)
  # Negative log-likelihood 
  fun <- -sum(dnorm(x = data$R, mean = pred, sd = tail(pars, 1), log = TRUE))
  #print(pars)
  return(fun)
}

#runs optim, using model_iter and NLL
mle_optim_LM <- function(data) {
  
  pars <- c(runif(1, 1, 10), runif(1, 1, 10))
  
  data$R <- pars[1] * data$x + pars[2] * data$y + rnorm(50,0,1)
  
  #plot(data$x, data$y)
  
  #curve(model_iter(pars, x), 0, 30, add = TRUE)
  
  par0 <- c(5,5,1)
  NLL(par0, data=data)
  #curve(model_iter(par0, x), 0, 30, add = TRUE, col = 'green')
  
  o <- optim(par = par0, fn = NLL, data = data, control = list(parscale = abs(par0)), 
             hessian = FALSE, method = "BFGS")
  o
  
  #plot(data$x, data$y)
  #curve(model_iter(o$par, x), 0, 30, col = "blue", add = TRUE)
  
  iter <- c(pars[1], pars[2], o$par[1], o$par[2])
  return(iter)
}

#############
# Body
#############

df <- replicate(n = 100, mle_optim_LM(data = data))
df <- t(df)
colnames(df) <- c('a', 'b', 'a_hat', 'b_hat')
df <- as.data.frame(df)

plot(df$a, df$a_hat)
abline(0, 1, col = 'red')

plot(df$b, df$b_hat)
abline(0, 1, col = 'red')

#Now, plot the distributions of the real values around the expected:
pars <- c(runif(1, 1, 10), runif(1, 1, 10))
data = data.frame(x = runif(50, 0, 30))
data$R <- pars[1] * data$x + pars[2] * data$y  + rnorm(50,0,3)

#plot(data$x, data$R)
#curve(model_iter(pars, x), 0, 30, add = TRUE)

o <- optim(par = par0, fn = NLL, data = data, control = list(parscale = abs(par0)), 
           hessian = FALSE, method = "BFGS")

#diff <- model_iter(o$par, data$x) - data$y
#plot(density(diff))

# shapiro.test(diff)
# 
# qqnorm(diff)
# qqline(diff, col = 2)

#looks like the errors are indeed normally distributed, so it looks fine.







