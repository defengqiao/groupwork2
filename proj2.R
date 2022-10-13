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

fcard3 = function(n, k,rand) {
  #rand = sample(1:(2 * n), 2 * n)   # open n boxes at random, rand[i] means the card number in ith time
  times = match(k, rand)
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
  #win=(fcard(n, k, ncard) <= m) #win=1 if he can find his number within n times
  return(fcard)
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
    # if (Pones(n, k, strategy, ncard) <= n) {  #if he can find his number within n times
    #   wint = wint + 1
    # }
    wint = wint + (fcard(n, k, strategy, ncard)<=n)
  }
  P = wint / nreps
  return(P)
}

Pall = function(n, strategy, nerps = 10000) {
  nn=0
  for (j in 1:nerps) {
    ncard = sample(1:(2 * n), 2 * n)   #ncard[i] is card number in ith box
    pid = array(1:(2*n),c(2*n))        #prisoner id
    winn=apply(pid,1, fcard,n=n,strategy=strategy,ncard=ncard)  #times of every prisoner try until they get card
    winL=(winn<=n)                     #FALSE if times of prisoner i try is bigger than n
    win=!(FALSE%in%winL)               #win=1 means they win once
    nn=nn+win
  }
  p = nn / nerps
  return(p)
}

print(Pall(50, 1))
print(Pone(50, 1, 1))