def absValue(x):
  if (x < 0):
    return -x
  else :
    return x
    
def power(x,p):
  if p == 0:
    return 1
  if p == 1:
    return x
  return x*power(x,p-1)
  
def isPrime(x):
  y = absValue(x)
  if (y < 2):
    return False
  for i in range(2,y):
    if y%i == 0:
      return False
  
  return True
  
  
def slowFib(n):
  if n==0:
    return 0
  elif n==1:
    return 1
  elif n >= 2:
    return slowFib(n-1)+slowFib(n-2)
    
def quickFib(n):
  if n==0:
    return 0
  elif n==1:
    return 1
  elif n >= 2:
    if n%2 ==0:
     return ( (2 * quickFib((n/2)-1)) + quickFib(n/2) ) * quickFib(n/2)
    else :
      return power(quickFib((n+1)/2),2) + power(quickFib(((n+1)/2)-1),2)

