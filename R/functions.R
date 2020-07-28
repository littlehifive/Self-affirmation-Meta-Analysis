
getB <- function(x){
  return(as.numeric(unlist(x)[c(1,3,4,5)]))
}

getgv <- function(d,v_d,n){
  J= 1- 3/ (4*n-9)
  g = J*d
  v_g = J^2 * v_d
  return(c(g, v_g))
}

# Peterson & Brown, 2005, <On the Use of Beta Coefficients in Meta-Analysis>
# This is for standardized beta only
B2d <- function(B,n){
  # B to r
  lamda = sign(B)
  r = B + 0.05 * lamda
  # r to d
  d = res(r = r,n = n)
  return(d)
}