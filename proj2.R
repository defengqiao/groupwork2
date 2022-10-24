#Defeng Qiao, s2419769; Tianai Ren, s2329207; YiZhou Chen, s2450877
#github：https://github.com/defengqiao/groupwork2.git
#Contributions:Defeng Qiao: 36% function s1s2s3 Pone, part of dloop, code refactoring for efficiency
#              Tianai Ren:  34% function dloop, overview, comments, output
#              YiZhou Chen: 30% function Pall, answer Question4 

rm(list = ls())
#Overview
#We constructed functions for individual prisoners to complete the game under three strategies,
#and estimated single(Pone) and overall(Pall) success probabilities by apply functions and for loops, 
#and we obtained the length of loops appearing in one game by constructing the matrix T where row i is the i-th card of the prisoner, 
#and estimated the probability by repeating the game through the dloop function.

#Remark: methods to reduce the running time
#1.Using the properties of probability, e.g, the magnitude of the probability of test failure increases as the number of test prisoners increases in a single simulation. 
#  Thus, we end the game after one person fails.
#2.For T, we use the method of adding the 2^(i-1) rows on the ith time, in order to simulate the game more efficiently.



#ANS for Question_4
#The surprising part is by using strategy_1, the probability of all prisoners winning this game would slowly decrease 
#and finally approach to 0.3 when n goes to infinity which is run counter to people’s convention that the probability will 
#approach to 0 when the number of participants becomes larger and larger. Under this circumstance, the probabilities of 
#each game are no longer independent. Hence, when we are considering the chance that all people win in this game, instead 
#of multiplying the probability of prisoner win in his period, we should more consider the probability the case with number of loops <= n. 

#As for strategy_3, in this case, everyone has the probability of winning his game, so the probability of all prisoners 
#winning this game is equal to the multiplication of the probabilities that loops<=n in this period occurs, and the 
#probability of anyone in the game winning on his period will decrease from 0.5 and finally approach to 0 with n increase to infinity. 

#Different from strategy_1, strategy_2 use randomly box selection to replace the first step of strategy_1, thus, it is like 
#strategy_1, and the first step of strategy_3 is the same as the first step of strategy_2. In addition to that, the probability 
#that the outcome of the first step can lead the prisoner win on this game could be denoted as P2. However, if the random selection 
#is applied in the first step, the game might no longer be followed by the win loop, therefore, when n goes to infinity, P2 will 
#lead the probability of winning this game will exponentially decrease and approach to 0 as n goes from 1 to infinity. 



#function s1, s2, s3 find whether prisoner finds his card within n times by strategy1, 2, 3
#arguments: n        2*n is the number of prisoners;
#           k        ID of the prisoner;
#           ncard    a vector of card number in sorted boxes
#return:    if he finds his card,return 1, else return 0
s1 = function(n, k,ncard) {
  i = k                     #starts at the box with their number on it
  times = 1
  while (k != ncard[i]) {   #he can find his card at the end of the loop
    i = ncard[i]
    times = times + 1
  }
  win=times<=n              #win=0 if try more than n times,else win=1
  return(win)
}

s2 = function(n,k,ncard) {
  startr = sample(1:(2 * n), 1)         #starting from a randomly selected box
  i = startr
  times = 1
  while (k != ncard[i] & times <= n) {  #he never find his card if he isn't in the right loop
    i = ncard[i]
    times = times + 1
  }
  win=times<=n                          #win=0 if try more than n times,else win=1
  return(win)
}

s3=function(n,k,...){          #just have the same form
  rand = sample(1:(2 * n), n)  #card number he get first n times
  win = k %in% rand            #if he get his card,win=1;if not,win=0
  return(win)
}


#function Pone estimates the probability of a single prisoner succeeding in finding their number
#arguments: n,k      have been shown above;       
#           strategy shows the chosen strategy(1,2 or 3);
#           nreps    is the number of replicate simulations;
#return:    probability estimate
Pone = function(n, k, strategy, nreps = 10000) {
  if(strategy == 1){                #choose strategy
    stra=s1
  }else if(strategy == 2){
    stra=s2
  }else{
    stra=s3
  }
  wint = 0                           #times of success
  for (i in 1:nreps) {
    ncard = sample(1:(2 * n), 2 * n) #index of card in corresponding box
    wint = wint + stra(n, k, ncard)  #times of win
  }
  P = wint / nreps
  return(P)
}

#function game generate the results of a complete game under the three strategies
#arguments: n,strategy have been shown above; 
#return:    if this game is success,return 1, else return 0
game = function(n, strategy) {      #play the whole game once
  if(strategy == 1){                #choose strategy
    stra=s1
  }else if(strategy == 2){
    stra=s2
  }else{
    stra=s3
  }
  ncard = sample(1:(2 * n), 2 * n)   #ncard[i] is card number in ith box
  win = 1
  for (i in 1:(2 * n)) {             #every prisoner try
    if (stra(n, i,ncard) == 0) {     #if one fail, then game over
      win = 0
      break
    }
  }
  return(win)                        #all success, win=1, else win=0
}

#function Pall estimates the probability of success of the complete game under the three strategies
#arguments: n, nreps, strategy have been shown above;
#return:    probability estimate
Pall = function(n, strategy, nreps = 10000) {
  gresult=replicate(nreps,game(n=n,strategy = strategy))  #result of 10000 games
  sn=sum(gresult)                                         #number of wins
  p=round(sn / nreps,6)                                   #probability of game
}


#dloop

#function rloop constructs the matrix T
#arguments: n        have been shown above;
#           T        matrix T,row i represents the card number that each prisioner gets form the (i-1)st time
#return:    nrow(T)-1
# rloop = function(T,n) { #T
#   len=nrow(T)           #the length of column in T
#   nr=sapply(c(1:(2*n)),function(i,T) T[2:len,T[len,i]],T=T) #next card number
#   return(nr)            #return nrow(T)-1
# }
# 
# #function leni obtains the length of the loops in a single game by matirx T
# #arguments: n        have been shown above;
# #return:    the length of the loops
# leni = function(n) {
#   T = c(1:(2 * n))                       #n boxes
#   T = rbind(T,sample(1:(2 * n), 2 * n))  #card number in boxes
#   while (nrow(T)<=(2*n)) {
#     T=rbind(T,rloop(T,n))
#   }
#   
#   len=apply(T,2,function(Ti) match(Ti[1],Ti[2:(2*n+1)]))  #loop length
#   lenlist=rep(0,2*n)
#   lenlist[len]=1                                          #if length i occurring once, lenlist[i]=1
#   return(lenlist)
# }
# 
# #function dloop estimates the probability that the loop length from 1 to 2*n occurring at least once
# #arguments: n,reps       have been shown above;
# #return:    probability estimate
# dloop=function(n,nreps=10000){
#   #c=rep(n,nreps)
#   #lenm=sapply(c, leni)  
#   lenm=replicate(nreps,leni(n))   #10000 columns, each columns is a lenlist
#   p=round(rowSums(lenm)/nreps,6)   
#   return(p)
# }
loop = function(n) {
  n = 50
  ncard = sample(1:(2 * n), 2 * n)
  nbox = 1:(2 * n)
  listl = c()
  while (length(nbox) != 0) {
    aloop = c(nbox[1])
    lenl = 1
    while (nbox[1] != ncard[aloop[lenl]]) {
      card = aloop[lenl]
      aloop = c(aloop, ncard[card])
      lenl = lenl + 1
    }
    listl = c(listl, lenl)
    iloop = match(aloop, nbox)
    nbox = nbox[-iloop]
  }
  lenlist = rep(0, 2 * n)
  lenlist[listl] = 1
  return(lenlist)
}
#function leni obtains the length of the loops in a single game by matirx T
#arguments: n        have been shown above;
#return:    the length of the loops


dloop = function(n, nreps = 10000) {
  lenm = replicate(nreps, loop(n))   #10000 columns, each columns is a lenlist
  p = round(rowSums(lenm) / nreps, 6)
  return(p)
}

#print results:
#poneout is the probabilities of the individual succeeding in finding their number
#Pallout is the probabilities of the joint succeess
#dloop50 estimates the probability that the loop length is less than 50 occurring at least once
#plot the probabilities of each loop length
a=system.time({Poneout = data.frame("strategy" = c(1,2,3), "Pone5" = sapply(c(1:3), Pone,n=5,k=1), "Pone50" = sapply(c(1:3), Pone,n=50,k=1))
Pallout = data.frame("strategy" = c(1,2,3), "Pall5" = sapply(c(1:3), Pall,n=5), "Pall50" =sapply(c(1:3),Pall,n=50))
dloopp = dloop(50)
dloop50 = 1- sum(dloopp[51:100])
plot(dloopp, ylim = range(0:1), xlim = range(1:100), xlab = "length", ylab = "probablity", main = paste("The probability of each loop length occurs at least once"), cex = 0.8, type = "p")
print(Poneout)
print(Pallout)
cat("P(length<=50):",dloop50)})
print(a)