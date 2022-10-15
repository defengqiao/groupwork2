#Defeng Qiao, s2419769; Tianai Ren, s2329207; YiZhou Chen, s2450877
#https://github.com/defengqiao/groupwork2.git
#Contributions

rm(list = ls())


#find whether prisoner finds his card and times he tries no matter whether he finds with strategy1
#arguments: n        2*n is the number of prisoners;
#           k        is the ID of the prisoner;
#return:    whether he finds his card,times he tries

fcard = function(n, k, strategy, ncard) {
  #decide which strategy should be used
  # if (strategy == 1) {
  #   fcard = fcard1(n, k, ncard)
  # } else if (strategy == 2) {
  #   fcard = fcard2(n, k, ncard)
  # } else{
  #   fcard = fcard3(n, k)
  # }
  if (strategy == 1) {
    i = k                         #starts at the box with their number on it
    times = 1
    while (k != ncard[i]) {
      #he can find his card at the end of the loop
      i = ncard[i]
      times = times + 1
    }
  } else if (strategy == 2) {
    startr = sample(1:(2 * n), 1)         #starting from a randomly selected box
    i = startr
    times = 1
    #loop = 0
    while (k != ncard[i] & times <= n) {
      #loop = (startr == ncard[i]) #loop=1 when a loop occurs and k is not in the loop
      i = ncard[i]
      times = times + 1
    }
  } else{
    rand = sample(1:(2 * n), n)     #card number he get first n times
    times = (!(k %in% rand)) * (n + 1)     #if he get his card,times=0;if not,times=n+1
  }
  return(times)
}

#function Pone estimates the probability of a single prisoner succeeding in finding their number
#arguments: n        2*n is the number of prisoners;
#           k        is the ID of the prisoner;
#           strategy shows the chosen strategy(1,2 or 3);
#           nreps    is the number of replicate simulations;
#return:    probability estimate
Pone = function(n, k, strategy, nreps = 10000) {
  wint = 0   #times of success
  for (i in 1:nreps) {
    ncard = sample(1:(2 * n), 2 * n)
    wint = wint + (fcard(n, k, strategy, ncard)<=n)
  }
  P = wint / nreps
  return(P)
}

#m means prisoner can try m times (for probability distribution)
gall=function(n, strategy){    #game for all
  ncard = sample(1:(2 * n), 2 * n)   #ncard[i] is card number in ith box
  pid = array(1:(2*n),c(2*n))        #prisoner id
  winn=sapply(pid, fcard,n=n,strategy=strategy,ncard=ncard)  #times of every prisoner try until they get card
  winL=(winn<=n)                     #FALSE if times of prisoner i try is bigger than n
  win=all(winL)                      #win=1 means they win once
  return(win)
}
Pall = function(n, strategy, nerps = 10000){
  gresult=sapply(rep(n,nerps), gall,strategy=strategy)
  sn=sum(gresult)               ##number of wins
  p=round(sn / nerps,5)         #probability of game
}
#-------------------------------

#print(system.time(Pall(50,2)))
Pone5=sapply(c(1:3), Pone,n=5,k=1)
Pone50=sapply(c(1:3), Pone,n=50,k=1)
Pall5=sapply(c(1:3), Pall,n=5)
Pall50=sapply(c(1:3),Pall,n=50)
#------------------------------------------------------
#dloop

rloop = function(T,n) {#T
  len=nrow(T)       #the length of column in T
  nr=sapply(c(1:(2*n)),function(i,T) T[2:len,T[len,i]],T=T) #next card number
  return(nr)    #return next 2^(i-1) row(s) on the ith time of the loop!
}

leni = function(n) {
  #ncard = sample(1:(2 * n), 2 * n)
  #T = matrix(0, 2 * n+1, 2 * n)
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

dloop=function(n,nerps=10000){
  c=rep(n,nerps)
  lenm=sapply(c, leni)  #10000columns, each columns is a lenlist
  p=round(rowSums(lenm)/nerps,7)   
  return(p)
}

a=system.time(dloop(50))
print(a)