library("dplyr")
# Assignment 2

# Import Data and Set Parameters --------------------------------------------------------------

data <- read.table("data_test_assg2.txt", header = T)
colnames(data) <- c("BusNo.", "Timeperiod","DecisionNo.","Mileage","Chosen")
beta = 0.9
tol = 1e-5

# Likelihood Function -----------------------------------------------------

loglik <- function(param, V, data,beta){
  cr = param[1]
  cm = param[2]
  
  # get log liklihood
  loglik <- data %>% mutate(expu = ifelse(DecisionNo. == 0, 
                                          exp(- cm* Mileage + beta * V[Mileage + 1]), exp(-cr  + beta * V[1]))) %>% 
    group_by(BusNo., Timeperiod) %>% 
    mutate(prob = expu / sum(expu)) %>% 
    mutate(lik = prob * Chosen) %>% 
    ungroup() %>% 
    filter(Chosen == 1) %>% 
    summarise(loglik = sum(log(lik))) %>% 
    pull()
  
  return(-loglik)
}


# Nested Fixed Points Alogrithm --------------------------------------------------------------
nested.fix <- function(data, V, param, tol = 1e-5){
  diff = 1
  while(diff > tol){
    abs_diff = 1
    while(abs_diff > tol){
      V_next = matrix(c(V[2:M], V[M]), ncol = 1)
      V.updated = log(exp(-cm * X + beta * V_next ) + exp(-cr + beta * V[1]))
      abs_diff = max(abs(V.updated - V))
      V = V.updated
    }
    m <- nlm(loglik, param, V, data, beta)
    cr <- m$estimate[1]
    cm <- m$estimate[2]
    param.updated <- c(cr,cm)
    diff = max(abs(param.updated - param))
    param = param.updated
    cr = param[1]
    cm = param[2]
  }
  return(param)
}

# Estimation --------------------------------------------------------------

# use parameter estimates from the static model as the starting value for theta1
data.logit <- data %>% mutate(next.mileage = ifelse(DecisionNo. == 0, Mileage +1, 1)) %>% 
  mutate(mileage.cost = ifelse(DecisionNo. == 0, next.mileage, 0)) 
logit <- glm(Chosen ~ DecisionNo.+ mileage.cost, data= data.logit,family = "binomial")
cr = -logit$coefficients[2]
cm = -logit$coefficients[3]
param <- c(cr,cm)

# initialize expected value matrix
V <- matrix(rep(0,M),ncol =  1) 

estimates <- nested.fix(data, V, param)

# boostrap standard errors ------------------------------------------------

for (i in 1:250){
  idx <- sample(seq(0,59), size = 60, replace = T)
  data.shuffled <- data %>% group_by(BusNo.) %>% 
    sample_n(., 60, replace = T)
  est <- nested.fix(data.shuffled, V, param)
  cr[i] <- est[1]
  cm[i] <- est[2]
}

# get the standard errors

