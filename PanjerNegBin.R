
#panjer Negative Binomial 
mean_NBin = 20 
var_NBin = 59
p = mean_NBin/var_NBin
a = 1-p
r = p*mean_NBin/a
b = a*(r-1)
p0 = 1/9

fy <- function(k){
  return(dgeom(k,p0))
}
fn <- function(k){
  return(dnbinom(k,r,p))
}


Panjer <- function(m) {
 
  S = 0
  if(m == 0){
    return (fn(0))
  }
  else {
    for (k in 1:m) {
      S = S + (a+b*k/m)*fy(k-1)*Panjer(m-k)
    }
   
  }
  return (S)
}

Panjer(100)
