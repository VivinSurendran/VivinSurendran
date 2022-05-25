"
****************************************************************
Name: vivin Surendran
Student Number: A00234993

QMM 1001 Lab 9
****************************************************************
"


"
#############								   	                  
#Question #1
1. A group of 350 employees is randomly selected at a large company. They are asked if they spend greater than 4 hours a day looking at a computer screen. 288 responded yes.
a. Find a 95% confidence interval for the population proportion of employees that spend greater than 4 hours a day looking at a computer screen. 
b. Assume that the sample proportion stays the exact same but the number of employees selected is increased to 1400. Find an 85% confidence interval for the
population proportion of employees that spend greater than 4 hours a day
looking at a computer screen. 
c. Is the confidence interval in part b wider or more narrow than the interval in
part a? Explain the result in relation to the sample size and level of confidence 
#############
"

#p = sample proportion, enter as a decimal
#n = sample size
#cl = confidence level, enter as decimal

CI<-function(p, n, cl){
  q<-1-p
  SE<-sqrt(p*q/n) #find standard error
  z.star<-qnorm(cl/2+0.5, 0, 1, lower.tail=TRUE)
  me<-z.star*SE #find margin of error
  lower<-p-me #lower bound
  upper<-p+me #upper bound
  return(c(lower, upper)) #confidence interval
}


#a)
CI(288/350,350,0.95)
#288/350 is sample proportion
#350 is sample size
#0.95 is confidence level
#Ans: 0.782 - 0.862 - proportion of people spend greater than 4 hours a day looking at a computer screen, with 95% confidence
#Notice this is a smaller/narrower range 


#b)
CI(288/1400,1400,0.85)
#288/1400 is sample proportion
#1400 is sample size
#0.85 is confidence level

#c)
#Ans: 0.190 - 0.221 - proportion of people spend greater than 4 hours a day looking at a computer screen, with 85% confidence
#Notice this is a larger/wider range than 95% confidence. since the sample size is larger and it is wider than 'a' since it is having lesser confidents interval compared to part 'a'







"
#############								   	                  
#Question #2
2. In a random sample of 890 adult Canadians there are 400 people that wished
they had chosen a different career.
a. What is a 80% confidence interval for the proportion? 
b. What is a 95% confidence interval for the proportion? 
c. Explain the results in part a and part b are different 
#############
"

#a)
CI(400/890,890,0.80)
#400/890 is sample proportion
#890 is sample size
#0.80 is confidence level
#Ans: 0.428 - 0.470 - proportion of people that wished they had chosen a different career, with 80% confidence
#Notice this is a smaller/wider range than 95% confidence



#b)
CI(400/890,890,0.95)
#400/890 is sample proportion
#890 is sample size
#0.95 is confidence level
#Ans: 0.416 - 0.482 - proportion of people that wished they had chosen a different career, with 95% confidence
#Notice this is a smaller/narrower range 



#c)The confidents interval in the part b is smaller/narrower compared to part a because of the confidents interval , part a having smaller confidence interval of 80 than of b having 95.

