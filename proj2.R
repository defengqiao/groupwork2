#Defeng Qiao, s2419769; Tianai Ren, s2329207; YiZhou Chen, s2450877
#https://github.com/defengqiao/groupwork2.git
#Contributions

rm(list = ls())


#find whether prisoner finds his card and times he tries no matter whether he finds with strategy1
#arguments: n        2*n is the number of prisoners;
#           k        is the ID of the prisoner;
#return:    whether he finds his card,times he tries

s1 = function(n, k,ncard) {
  i = k                         #starts at the box with their number on it
  times = 1
  while (k != ncard[i]) {
    #he can find his card at the end of the loop
    i = ncard[i]
    times = times + 1
  }
  win=times<=n           #win=0 if try more than n times,else win=1
  return(win)
}

s2 = function(n,k,ncard) {
  startr = sample(1:(2 * n), 1)         #starting from a randomly selected box
  i = startr
  times = 1
  while (k != ncard[i] & times <= n) { #he may never find his card if he isn't in the right loop
    i = ncard[i]
    times = times + 1
  }
  win=times<=n             #win=0 if try more than n times,else win=1
  return(win)
}

s3=function(n,k,...){          #just have the same form
  rand = sample(1:(2 * n), n)  #card number he get first n times
  win = k %in% rand                #if he get his card,win=1;if not,win=0
  return(win)
}

# fcard = function(n, k, strategy, ncard) {
#   if (strategy == 1) {
#     i = k                         #starts at the box with their number on it
#     times = 1
#     while (k != ncard[i]) {
#       #he can find his card at the end of the loop
#       i = ncard[i]
#       times = times + 1
#     }
#   } else if (strategy == 2) {
#     startr = sample(1:(2 * n), 1)         #starting from a randomly selected box
#     i = startr
#     times = 1
#     #loop = 0
#     while (k != ncard[i] & times <= n) {
#       #loop = (startr == ncard[i]) #loop=1 when a loop occurs and k is not in the loop
#       i = ncard[i]
#       times = times + 1
#     }
#   } else{
#     rand = sample(1:(2 * n), n)     #card number he get first n times
#     times = (!(k %in% rand)) * (n + 1)     #if he get his card,times=0;if not,times=n+1
#   }
#   return(times)
# }

#function Pone estimates the probability of a single prisoner succeeding in finding their number
#arguments: n        2*n is the number of prisoners;
#           k        is the ID of the prisoner;
#           strategy shows the chosen strategy(1,2 or 3);
#           nreps    is the number of replicate simulations;
#return:    probability estimate
Pone = function(n, k, strategy, nreps = 10000) {
  if(strategy == 1){             #choose strategy
    stra=s1
  }else if(strategy == 2){
    stra=s2
  }else{
    stra=s3
  }
  wint = 0   #times of success
  for (i in 1:nreps) {
    ncard = sample(1:(2 * n), 2 * n) #index of card in corresponding box
    wint = wint + stra(n, k, ncard)  #times of win
  }
  P = wint / nreps
  return(P)
}

game = function(n, strategy) {   #play the whole game once
  if(strategy == 1){             #choose strategy
    stra=s1
  }else if(strategy == 2){
    stra=s2
  }else{
    stra=s3
  }
  ncard = sample(1:(2 * n), 2 * n)   #ncard[i] is card number in ith box
  win = 1
  for (i in 1:(2 * n)) {          #every prisoner try
    if (stra(n, i,ncard) == 0) { #if one fail, then game over
      win = 0
      break
    }
  }
  return(win)                #all success, win=1, else win=0
}

Pall = function(n, strategy,nreps = 10000) {
  gresult=replicate(nreps,game(n=n,strategy = strategy))  #result of 10000 games
  sn=sum(gresult)               #number of wins
  p=round(sn / nreps,6)         #probability of game
  
}

#---------------------------------------------------

#print(system.time(Pall(50,2)))

#------------------------------------------------------
#dloop

rloop = function(T,n) {#T
  len=nrow(T)       #the length of column in T
  nr=sapply(c(1:(2*n)),function(i,T) T[2:len,T[len,i]],T=T) #next card number
  return(nr)    #return next 2^(i-1) row(s) on the ith time of the loop!
}

leni = function(n) {
  T = c(1:(2 * n))              #n boxes
  T = rbind(T,sample(1:(2 * n), 2 * n))  #card number in boxes
  while (nrow(T)<=(2*n)) {
    T=rbind(T,rloop(T,n))
  }
  
  len=apply(T,2,function(Ti) match(Ti[1],Ti[2:(2*n+1)]))  #loop length
  lenlist=rep(0,2*n)
  lenlist[len]=1            #if length i occurring once, lenlist[i]=1
  return(lenlist)
}

dloop=function(n,nreps=10000){
  #c=rep(n,nreps)
  #lenm=sapply(c, leni)  
  lenm=replicate(nreps,leni(n)) #10000columns, each columns is a lenlist
  p=round(rowSums(lenm)/nreps,6)   
  return(p)
}

#plot the probabilities of each loop length
plot(dloop(50), ylim = range(0:1), xlab = "length", ylab = "probablity", main = paste("The probability of each loop length occurs at least once"), cex = 0.8, type = "p")

a=system.time({Pone5=sapply(c(1:3), Pone,n=5,k=1)
Pone50=sapply(c(1:3), Pone,n=50,k=1)
Pall5=sapply(c(1:3), Pall,n=5)
Pall50=sapply(c(1:3),Pall,n=50)
ploop=dloop(50)
barplot(ploop,space=0)})
print(a)
#print(Pone(5,5,3))
# a=system.time({Pall2=Pall(50,2)})
# print(a)
#ploop=dloop(10,50000)

