# Assignment 1
library("dplyr")
library("data.table")
library("ggplot2")
# input parameters

N = 100
T = 100 -1
M = 10
beta = 0.9
cr = 3.5
cm = 2.5

set.seed(123)
# generate expected value function ----------------------------------------
V <- matrix(rep(0,M),ncol =  1)
X <- matrix(seq(1,M), ncol = 1)

tol = 1e-6
abs_diff = 1
while(abs_diff > tol){
  V_next = matrix(c(V[2:M], V[M]), ncol = 1)
  V.updated = log(exp(-cm * X + beta * V_next ) + exp(-cr + beta * V[1]))
  abs_diff = max(abs(V.updated - V))
  V = V.updated
}


# Simulate Decisions ------------------------------------------------------
data <- data.frame(BusNo. = rep(0:(N-1), each = 2 * (T+1)), 
                   Timeperiod = rep(rep(0:T, each = 2),N),
                   DecisionNo. = rep(c(0,1),N * (T+1) ), 
                   Mileage = rep(NA, N * (T+1) * 2),
                   ExpChoiceSpecificValuefunction = rep(NA, N * (T+1) * 2),
                   Chosen = rep(NA, N * (T+1) * 2))

# initialize milage =0 when t = 0
data <- data %>% mutate(Mileage = ifelse(Timeperiod == 0, 0, Mileage))

for (i in 0:(N-1)){
  for (t in 0: T){
   mileage <-  data[which(data$BusNo. == i & data$Timeperiod == t),]$Mileage
   data[which(data$BusNo. == i & data$Timeperiod == t),]$ExpChoiceSpecificValuefunction = V[c(mileage[1] + 1, 1)]
   Noreplace_pr = exp((-cm * mileage[1]) + beta * V[mileage[1] + 1])/(exp((-cm * mileage[1]) + beta * V[mileage[1] + 1]) + exp((-cr) + beta * V[1]))
   Chosen <- rbinom(1,1, Noreplace_pr)
   data[which(data$BusNo. == i & data$Timeperiod == t),]$Chosen <- c(Chosen, 1- Chosen)
   # update next time period Mileage
   if (t+1 <= T){
     data[which(data$BusNo. == i & data$Timeperiod == t+1),]$Mileage = ifelse(Chosen == 1, mileage +1 ,1 )
   }
  }
}

write.table(data,file = "simulated_data.txt",row.names = F)

# observed probability distribution of mileage
data %>% filter(Chosen == 1) %>% group_by(Mileage) %>% 
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

# histogram
data %>% filter(Chosen == 1) %>% 
  group_by(Mileage) %>% 
  summarise(prob = sum(DecisionNo.) / n()) %>% 
  rbind(., data.frame(Mileage = seq(4,M), prob = 1)) %>% 
  ggplot(data = .,) +
  geom_bar(aes(x = Mileage, y = prob),stat = "identity") + 
  theme_bw()


  



