# Faking condit data
# Passing through MLE function
# Dec 2021

condit <- readRDS('sums.rds')

set.seed(42)
data = data.frame(t = runif(109, 0, 30), #how should these be distributed?
                  burn = runif(109, 0, 300), #how are they distributed in the real data?
                  temp = condit$temperature,
                  ppt = condit$annual_ppt)
# # #

#finding valid initial parameters (any that will generate a curve that exists)
f1 <- function(x, r, b, t){
  A <- 10-exp(-0.1*r+1) #asymptote, based on rainfall
  B <- 1+1*(1-exp(-1*b))^1 #delay, based on # of days from last burn to abandonment
  k <- -0.00001*t^2+0.001*t #growth parameter, based on temperature
  fun <- A * ((1 - exp(-k * x))^((B) - 1) * ((B) * (exp(-k * x) * k)))
  print(length(k))
  print(length(x))
  return (fun) #absolute growth value
}

#plotting age of forest vs agb
curve(expr = f1(x,2000,300,240), from = 0, to = 25)

#the curve exists, so those values should run.
# # #


pars <- c(100, 0.001, 2, 0.0000025, 0.00125, 2, 1, 0.01)

#see range of valid parameters.

#make new fake data to the model
Gmax = pars[1]-exp(-pars[2]*data$ppt+pars[3]) #asymptote, based on rainfall
k = -pars[4]*data$temp^2+pars[5]*data$temp #growth parameter, based on temperature
B = pars[6]+pars[7]*(1-exp(-pars[8]*data$burn))^2  #delay, based on # of days from last burn to abandonment
# Prediction of the model

data$G <- Gmax * (1 - exp(-k*(data$t)))^B + rnorm(109,0,5)

plot(data$t, data$G, las = 1, xlab = "Days after abandonment", ylab = "Biomass")

G = function(pars, temp, burn, ppt, t) {
  # Extract parameters of the model
  Gmax = pars[1]-exp(-pars[2]*ppt+pars[3]) #asymptote, based on rainfall
  k = -pars[4]*temp^2+pars[5]*temp #growth parameter, based on temperature
  B = pars[6]+pars[7]*(1-exp(-pars[8]*burn))^2 #delay, based on # of days from last burn to abandonment
  # Prediction of the model
  Gmax * (1 - exp(-k*(t)))^B
}

plot(data$t, data$G, las = 1,  xlab = "Days after abandonment", ylab = "Biomass")
curve(G(c(50, 0.001, 2, 0.0000025, 0.00125, 2, 1, 0.01), 225, 150, 1000, x), 0, 30, add = TRUE)

NLL = function(pars, data) {
  # Values prediced by the model
  if(pars[9] < 0){ #avoiding NAs by keeping the st dev positive
    return(-Inf)
  }
  Gpred = G(pars, data$temp, data$burn, data$ppt, data$t)
  # Negative log-likelihood 
  fun <- -sum(dnorm(x = data$G, mean = Gpred, sd = pars[9], log = TRUE))
  #print(pars)
  return(fun)
}

par0 <- c(10, 0.001, 3, 0.000001, 0.001, 1, 1, 0.01, 0.01)
NLL(par0, data=data)

meth0 <- c("Nelder-Mead", "BFGS", "CG", "SANN")
for (i in meth0){
  o <- optim(par = par0, fn = NLL, data = data, control = list(parscale = abs(par0)), 
             hessian = FALSE, method = i)
  print(i)
  print(o)
}

# BFGS appears to be the best method

o <- optim(par = par0, fn = NLL, data = data, control = list(parscale = abs(par0)), 
           hessian = FALSE, method = "BFGS")
print(o)

plot(data$t, data$G, las = 1,  xlab = "Days after abandonment", ylab = "Biomass")
curve(G(o$par, 225, 150, 1000, x), 0, 60, add = TRUE)

pred <- G(o$par[1:8], data$temp, data$burn, data$ppt, data$t)

plot(data$G, pred, abline(0,1))

mean(data$G - pred)



