#Defeng Qiao, s2419769; Tianai Ren, s2329207; YiZhou Chen, s2450877
#https://github.com/defengqiao/groupwork2.git
#Contributions

rm(list = ls())


#find whether prisoner finds his card and times he tries no matter whether he finds with strategy1
#arguments: n        2*n is the number of prisoners;
#           k        is the ID of the prisoner;
#return:    whether he finds his card,times he tries
fcard1 = function(n, k,ncard) {
  #ncard = sample(1:(2 * n), 2 * n)   #ncard[i] is card number in ith box
  i = k                         #starts at the box with their number on it
  times = 1
  while (k != ncard[i]) {   #he can find his card at the end of the loop
    i = ncard[i]
    times = times + 1
  }
  return(times)             #times he try = length of the loop
}

fcard2 = function(n, k,ncard) {
  #ncard = sample(1:(2 * n), 2 * n)   #ncard[i] is card number in ith box
  startr = sample(1:(2 * n), 1)         #starting from a randomly selected box
  i = startr
  times = 1
  loop = 0
  while (k != ncard[i] & !loop) {
    loop = (startr == ncard[i]) #loop=1 when a loop occurs and k is not in the loop   
    i = ncard[i]
    times = times + 1
  }
  if (loop==1) times= 2*n+1     #it means he can never find his number
  return(times)
}

fcard3 = function(n, k,ncard) {
  rand = sample(1:(2 * n), 2 * n) #the order of opening boxes
  i = match(k, ncard)             #the card is in the box i
  times=match(i,rand)             #times he try
  return(times)
}

fcard= function(n, k, strategy, ncard) {
  #decide which strategy should be used  
  if (strategy == 1) {
    fcard = fcard1(n, k, ncard)
  } else if (strategy == 2) {
    fcard = fcard2(n, k, ncard)
  } else{
    fcard = fcard3(n, k, ncard)
  }
  return(fcard)
}

#function Pone estimates the probability of a single prisoner succeeding in finding their number
#arguments: n        2*n is the number of prisoners;
#           k        is the ID of the prisoner;
#           strategy shows the chosen strategy(1,2 or 3);
#           nreps    is the number of replicate simulations;
#return:    probability estimate
Pone = function(n, k, strategy, nreps = 10000,m=n) {
  wint = 0   #times of success
  for (i in 1:nreps) {
    ncard = sample(1:(2 * n), 2 * n)
    wint = wint + (fcard(n, k, strategy, ncard)<=m)
  }
  P = wint / nreps
  return(P)
}

#m means prisoner can try m times (for probability distribution)
gall=function(n, strategy, m=n){    #game for all
  ncard = sample(1:(2 * n), 2 * n)   #ncard[i] is card number in ith box
  pid = array(1:(2*n),c(2*n))        #prisoner id
  winn=sapply(pid, fcard,n=n,strategy=strategy,ncard=ncard)  #times of every prisoner try until they get card
  winL=(winn<=m)                     #FALSE if times of prisoner i try is bigger than n
  win=all(winL)                      #win=1 means they win once
  return(win)
}
Pall = function(n, strategy, nerps = 10000,m=n){
  gresult=sapply(rep(n,nerps), gall,strategy=strategy)
  sn=sum(gresult)               ##number of wins
  p=round(sn / nerps,7)         #probability of game
}
#------------------------------------------------------
#dloop

rloop = function(Ti,ncard) {#T[i,]
  nr=sapply(Ti,function(i,ncard) ncard[i],ncard=ncard ) #next card number
  return(nr)    #return next row
}
leni = function(n) {
  ncard = sample(1:(2 * n), 2 * n)
  T = matrix(0, 2 * n+1, 2 * n)
  T[1, ] = c(1:(2 * n))
  for (i in 1:((2 * n))) {
    T[i + 1, ] = rloop(T[i, ], ncard)
  }
  len=apply(T,2,function(Ti) match(Ti[1],Ti[2:(2*n+1)]))  #loop length
  lenlist=rep(0,2*n)
  lenlist[len]=1            #if length i occurring once, lenlist[i]=1
  return(lenlist)
}

dloop=function(n,nerps=10000){
  lenm=sapply(rep(n,nerps), leni)  #10000columns, each columns is a lenlist
  p=round(rowSums(lenm)/nerps,7)   
  return(p)
}



a=dloop(5)
print(a)