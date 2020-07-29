## Solving Differential equations in R
# N is the state variable, and alpha is the parameter
library(deSolve)
library(ggplot2)
library(reshape2)

## To solve differential eqn using ode() in the deSolve package
state_variables<-c(N=1)
times<- seq(0,40,by=0.5)
parameter<-c(alpha=log(2))
expo_func<-function(time,state,parameter){
  N<-state['N']
  dN<-parameter['alpha']*N
  return(list(c(dN)))
}
result<-ode(y=state_variables,times=times,func=expo_func,parms=parameter)
head(data.frame(result))



# Plot some graphs
result<-as.data.frame(result)
ggplot(result,aes(x=times,y=N))+geom_line(colour="blue")




# More realistic Logistic Growth
state_variables<-c(N=1)
times<-seq(0,40,by=0.5)
parameters<-c("alpha","K")
logistic_function<-function(time,state,parameter){
  N<-state['N']
  dN<-parameters['alpha']*N*(1-{N/(parameters['K'])})
  return(list(c(dN)))
}
parameters<-c(alpha=log(2),K=3)
return<-ode(y=state_variables,times=times,func=logistic_function,parms=parameters)
result_logistic<-as.data.frame(return)
head(result)



#Plot the logistic graph
ggplot(result_logistic,aes(x=times,y=N))+geom_line(colour="red")




## Modelling an infected cohort
library(deSolve)
library(ggplot2)
library(reshape2)

initial_no_infected<-1000000
initial_no_recovered<-0
recovery_rate<-1/10
follow_up_time<-28

state_variables1<-c(I=initial_no_infected,R=initial_no_recovered)
times<-seq(from=0,to=follow_up_time,by=1)
parameters<-c(gamma=recovery_rate)

cohort_model<-function(time,state,parameter){
  with(as.list(c(state,parameters)),{
    dI=(-1)*gamma*I
    dR= gamma*I
    return(list(c(dI,dR)))
  })
}

result1<-as.data.frame(ode(y=state_variables1,times=times,func=cohort_model,parms=parameters))
result1

ggplot(result1,aes(x=times,y=result1$I))+geom_line(colour="blue")
ggplot(result1,aes(x=times,y=I))+geom_line(colour="blue")
ggplot(result1,aes(x=times,y=R))+geom_line(colour="blue")



## To get both I and R in the same graph
result1_long<-melt(as.data.frame(result1),id="time")
result1_long
ggplot(result1_long,aes(x=time,y=value,colour=variable,group=variable))+geom_line()+labs(title="gamma is 0.1 day^-1",xlab="Times in days",ylab="Number of people")




## Observe the shape of the plot with different values of gamma
initial_no_infected<-1000000
initial_no_recovered<-0
recovery_rate<-1/20
follow_up_time<-28

state_variables1<-c(I=initial_no_infected,R=initial_no_recovered)
times<-seq(from=0,to=follow_up_time,by=1)
parameters<-c(gamma=recovery_rate)

cohort_model2<-function(time,state,parameter){
  with(as.list(c(state,parameters)),{
    dI=(-1)*gamma*I
    dR= gamma*I
    return(list(c(dI,dR)))
  })
}

result2<-as.data.frame(ode(y=state_variables1,times=times,func=cohort_model2,parms=parameters))
result2_long<-melt(as.data.frame(result2),id="time")
result1_long
ggplot(result2_long,aes(x=time,y=value,colour=variable,group=variable))+geom_line()+labs(title="gamma is 0.05 day^-1",xlab="Times in days",ylab="Number of people")




## Competing Risk Model
#Load the packages
initial_no_infected<-1000000
initial_no_recovered<-0
initial_no_dead<-0

recovery_rate<-0.1
mortality_rate<-0.2
follow_up_time<-28

state_variables3<-c(I=initial_no_infected,R=initial_no_recovered,M=initial_no_dead)
times<-seq(from=0,to=follow_up_time,by=1)
parameters3<-c(gamma=recovery_rate,mu=mortality_rate)

cohort_model3<-function(time,state,parameter){
  with(as.list(c(state,parameters)),{
    dI=(-1)*(mu+gamma)*I
    dR= gamma*I
    dM= mu*I
    return(list(c(dI,dR,dM)))
  })
}

result3<-as.data.frame(ode(y=state_variables3,times=times,func=cohort_model3,parms=parameters3))
result3

result3_long<-melt(as.data.frame(result3),id="time")
ggplot(result3_long,aes(x=time,y=value,colour=variable,group=variable))+geom_line()+labs(title="gamma is 0.1 day^-1 and mu is 0.2 day^-1",xlab="Times in days",ylab="Number of people")


## Case fatality is 50% i.e. mu=gamma=0.1
initial_no_infected<-1000000
initial_no_recovered<-0
initial_no_dead<-0

recovery_rate1<-0.1
mortality_rate1<-0.1
follow_up_time<-28

state_variables3<-c(I=initial_no_infected,R=initial_no_recovered,M=initial_no_dead)
times<-seq(from=0,to=follow_up_time,by=1)
parameters3<-c(gamma=recovery_rate1,mu=mortality_rate1)

cohort_model3_new<-function(time,state,parameter){
  with(as.list(c(state,parameters)),{
    dI=(-1)*(mu+gamma)*I
    dR= gamma*I
    dM= mu*I
    return(list(c(dI,dR,dM)))
  })
}

result3_new<-as.data.frame(ode(y=state_variables3,times=times,func=cohort_model3_new,parms=parameters3))
result3_new

result3_long<-melt(as.data.frame(result3),id="time")
ggplot(result3_long,aes(x=time,y=value,colour=variable,group=variable))+geom_line()+labs(title="gamma is 0.1 day^-1 and mu is 0.2 day^-1",xlab="Times in days",ylab="Number of people")








## S-I-R Model
initial_no_suseptible<-99999
initial_no_infected<-1
initial_no_recovered<-0
follow_up_time<-60
force_of_infection<-0.2
rate_of_recovery<-0.1

state_variables_SIR<-c(I=initial_no_infected,R=initial_no_recovered,S=initial_no_suseptible)
times_SIR<-seq(from=0,to=follow_up_time,by=1)
parameters_SIR<-c(lambda=force_of_infection,gamma=rate_of_recovery)

SIR_model<-function(time,state,parameter){
  with(as.list(c(state,parameters)),{
    dS=(-1)*lambda*S
    dI=(lambda*S)-(gamma*I)
    dR=gamma*I
    return(list(c(dS,dI,dR)))
  })
}

result_SIR<-as.data.frame(ode(y=state_variables_SIR,times=times_SIR,func=SIR_model,parms=parameters_SIR))



## S-I-R model with varying value of lamba
state_var<-c(S=999999,I=1,R=0)
times<-seq(from=0,to=60,by=1)
parameters_new<-c(gamma=0.1,beta=1)

model_new<-function(state,time,parameter){
  with(as.list(c(state,parameters)),{
    N=S+I+R
    lambda <- beta * I/N
    dS=(-1)*lambda*S
    dI <- lambda * S - gamma * I
    dR=gamma*I
    return(list(c(dS,dI,dR)))
  })
}
 
result_new<-as.data.frame(ode(y=state_var,
                              times=times,
                              func=model_new,
                              parms=model_new))

## Building model with beta and gamma value
library(deSolve)
library(reshape2)
library(ggplot2)

initial_state_values <- c(S = 1000000-1,I=1,R=0)
parameters <- c(beta = 0.5,gamma = 0.25)
times <- seq(from = 0, to = 100, by = 1)
sir_model <- function(time, state, parameters) {  
  with(as.list(c(state, parameters)), { 
    N <- S+I+R
    lambda <- beta * I/N
    dS <- -lambda * S
    dI <- lambda * S - gamma * I
    dR <- gamma * I
    return(list(c(dS, dI, dR))) 
  })
  
}

output <- as.data.frame(ode(y = initial_state_values, 
                            times = times, 
                            func = sir_model,
                            parms = parameters))
output
output_long <- melt(as.data.frame(output), id = "time")    
output_long

ggplot(data = output_long,aes(x = time, y = value, colour = variable, group = variable))+geom_line()

# Where the infection reaches maximum value
output_long_1<-output_long[102:202,]
output_long_1
which.max(output_long_1$value)

# To find out the proportional value
output_prop<-output$S/sum(initial_state_values)
output_prop

output <- as.data.frame(ode(y = initial_state_values, 
                            times = times, 
                            func = sir_model,
                            parms = parameters),output$S/sum(initial_state_values))
output


## Simulating effective reproduction number(Reff)
initial_state_values <- c(S = 1000000-1,I=1,R=0)
parameters <- c(beta = 0.5,gamma = 0.25)
times <- seq(from = 0, to = 100, by = 1)
sir_model <- function(time, state, parameters) {  
  with(as.list(c(state, parameters)), { 
    N <- S+I+R
    lambda <- beta * I/N
    dS <- -lambda * S
    dI <- lambda * S - gamma * I
    dR <- gamma * I
    return(list(c(dS, dI, dR))) 
  })
  
}

output <- as.data.frame(ode(y = initial_state_values, 
                            times = times, 
                            func = sir_model,
                            parms = parameters))
output_long <- melt(as.data.frame(output), id = "time")    
ggplot(data = output_long,aes(x = time, y = value, colour = variable, group = variable))+geom_line()

beta = 0.5
gamma = 0.25
R0<-beta/gamma
R0

output_reff<-R0*output$S/1000000
output_reff

times <- seq(from = 0, to = 100, by = 1)
data_reff<-as.data.frame(cbind(times,output_reff))

# Plot Reff with time
ggplot(data=data_reff,aes(x=times,y=output_reff))+geom_line()
plot(times,output_reff,type = "l", main = "Reff with time",xlab = "Times",ylab = "Reff",col="red")



## Modelling population turnover
library(deSolve)
library(reshape2)
library(ggplot2)

initial_state_values <- c(S = 999999, I=1,R=0)
parameters <- c(beta = 0.4*365,      
                gamma = 0.2*365,     
                mu = 1/70,          
                b = 1/70) 
times <- seq(from = 0, to = 400, by = 2)
sir_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    N <- S+I+R
    lambda <- beta * I/N
    dS <- -lambda * S - mu * S + b * N            
    dI <- lambda * S - gamma * I  - mu * I           
    dR <- gamma * I - mu * R               
    return(list(c(dS, dI, dR))) 
  })
  
}


output <- as.data.frame(ode(y = initial_state_values, 
                            times = times, 
                            func = sir_model,
                            parms = parameters))


ggplot(data = output,aes(x = time, y = I)) +geom_line() + xlab("Time (years)")+ylab("Number of infected people")+xlim(c(0,1)) 

output_long <- melt(as.data.frame(output), id = "time")
output_long$proportion <- output_long$value/sum(initial_state_values)
ggplot(data = output_long,aes(x = time, y = proportion, colour = variable, group = variable)) +
  geom_line() +xlab("Time (years)")+  ylab("Prevalence (proportion)") +  labs(colour = "Compartment",title = "Prevalence of susceptible, infected and recovered people over time")



# Same model for 1 year
initial_state_values <- c(S = 1000000-1,I = 1,R = 0)
parameters <- c(beta = 0.4,gamma = 0.2,mu = 1/28,b = 1/28)
times <- seq(from = 0, to = 365, by = 1)
sir_model <- function(time, state, parameters) { 
  with(as.list(c(state, parameters)), {
    N <- S+I+R
    lambda <- beta * I/N
    dS <- -lambda * S - mu * S + b * N            
    dI <- lambda * S - gamma * I  - mu * I           
    dR <- gamma * I - mu * R               
    return(list(c(dS, dI, dR))) 
  })
  
}

output_1year <- as.data.frame(ode(y = initial_state_values, 
                                 times = times, 
                                 func = sir_model,
                                 parms = parameters))
output_1year_long <- melt(as.data.frame(output_1year), id = "time")
output_1year_long$prevalence <- output_1year_long$value/sum(initial_state_values)

ggplot(data = output_1year_long,
       aes(x = time, y = prevalence, colour = variable, group = variable)) +
  geom_line() +
  xlab("Time (days)")+
  ylab("Prevalence (proportion)") +
  labs(colour = "Compartment",title = "Prevalence of infection, susceptibility and recovery over time")



## Effect of vaccination
p<-1/2
N=1000000
initial_state_values <- c(S = 999999*1/2,I = 1,R = 999999*1/2)
parameters <- c(beta = 0.4,gamma = 0.1)
times <- seq(from = 0, to = 2*365, by = 1)
sir_model <- function(time, state, parameters) { 
  with(as.list(c(state, parameters)), {
    N <- S+I+R
    lambda <- beta * I/N
    dS <- -lambda * S             
    dI <- lambda * S - gamma * I             
    dR <- gamma * I                
    return(list(c(dS, dI, dR))) 
  })
  
}

output_vacc <- as.data.frame(ode(y = initial_state_values, 
                                  times = times, 
                                  func = sir_model,
                                  parms = parameters))
output_vacc_long <- melt(as.data.frame(output_vacc), id = "time")
output_vacc_long$prevalence <- as.data.frame(output_vacc_long$value/sum(initial_state_values))


# Plot the curve
ggplot(data = output_vacc_long$prevalence, 
       aes(x = time, y = prevalence, colour = variable, group = variable)) +
  geom_line() +   
  xlab("Time (days)")+ 
  ylab("Prevalence (proportion)") +
  labs(colour = "Compartment",
       title = "Prevalence of infection, susceptibility and recovery over time")








































  
  
  
  
  










































