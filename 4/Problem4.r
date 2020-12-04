#PROBLEM 4
library(distrEX)

set.seed(1134)

n<-100 #assuming 100 will be the highest possible number of players.

#SOLUTION-----------------------------------------------------------------------
mem_throwList <-c(rep(NA,n))
for(k in 1: length(mem_throwList)){mem_throwList[k] <-sample(c(1:6),1)}

a4_x_list <-c(rep(0,6))
for(t in 1:length(a4_x_list)){
  for(i in 1:length(mem_throwList)){
    if(mem_throwList[i]==t){a4_x_list[t] <-a4_x_list[t] + 1}
  }
}

a4_ex_STList <-c(rep(NA,6))
for(k in 1:6){a4_ex_STList[k] <-(a4_x_list[k]*(a4_x_list[k]-1))/2}
a4_x_scoreTotal <-sum(a4_ex_STList)

a4_e_Scorelist_means <-c(rep(NA,6))
for(k in 1:6){a4_e_Scorelist_means[k] <-((n^2)-n)/72}
a4_e_ScoreTotal_mean <-sum(a4_e_Scorelist_means)

#A)-----------------------------------------------------------------------------
b4_expect <-1/6 #expectation of the total score of the group.
print(b4_expect)
b4_var <-b4_expect*(1-b4_expect) #variance of the total score of the group.
print(b4_var)

#B)-----------------------------------------------------------------------------
b4_scoreExp <-choose(n,2)*b4_expect #mean of the score of the total score if any pair of players who throw
                                    #the same number scores that number.
print(b4_scoreExp)
b4_scoreVar <-choose(n,2)*b4_var #variance of the total score if any pair of players who throw
                                 #the same number scores that number.
print(b4_scoreVar)
