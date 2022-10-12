#Defeng Qiao, s2419769; Tianai Ren, s2329207; YiZhou Chen, s2450877
#https://github.com/defengqiao/groupwork2.git
#Contributions



#find whether prisoner finds his card and times he tries no matter whether he finds with strategy1
#arguments: n        2*n is the number of prisoners;
#           k        is the ID of the prisoner;
#return:    whether he finds his card,times he tries
fcard1 = function(n, k) {
  ncard = sample(1:(2 * n), 2 * n)   #ncard[i] is card number in ith box
  i = k                         #starts at the box with their number on it
  times = 1
  loop = 1
  while (k != ncard[i] & loop) {
    i = ncard[i]
    times = times + 1
    loop = (k != ncard[i]) #loop=0 when a loop occurs
  }
  return(c(loop, times))
}

fcard2 = function(n, k) {
  ncard = sample(1:(2 * n), 2 * n)   #ncard[i] is card number in ith box
  startr = sample(1:(2 * n), 1)         #starting from a randomly selected box
  i = startr
  times = 1
  loop = 1
  while (k != ncard[i] & loop) {
    i = ncard[i]
    times = times + 1
    loop = (startr != ncard[i]) #loop=0 when a loop occurs
  }
  return(c(loop, times))
}

fcard3 = function(n, k) {
  rand = sample(1:(2 * n), 2 * n)   # open n boxes at random, rand[i] means the card number in ith time
  times = match(k, rand)
  return(c(1, times))
}
#function Pone estimates the probability of a single prisoner succeeding in finding their number
#arguments: n        2*n is the number of prisoners;
#           k        is the ID of the prisoner;
#           strategy shows the chosen strategy(1,2 or 3);
#           nreps    is the number of replicate simulations;
#return:    probability estimate
Pone = function(n, k, strategy, nreps = 10000) {
  #decide which strategy should be used  
  if (strategy == 1) {
    fcard = fcard1
  } else if (strategy == 2) {
    fcard = fcard2
  } else{
    fcard = fcard3
  }
  wint = 0   #times of success
  for (i in 1:nreps) {
    result = fcard(n, k)
    if ((result[1] == 1) & (result[2] <= n)) {
      wint = wint + 1
    }
  }
  P = wint / nreps
  return(P)
}
print(Pone(50, 1, 3))