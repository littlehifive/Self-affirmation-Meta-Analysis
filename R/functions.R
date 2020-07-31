# A list of functions used in calculations:

# Extract useful information about g from esc functions outputs
extract_g <- function(x){
  return(as.numeric(unlist(x)[c(1,3,4,5)]))
}

# Reported effect size and variance to Hedges' g
dv2g <- function(d,v_d,n){
  J= 1- 3/ (4*n-9)
  g = J*d
  v_g = J^2 * v_d
  lowerCI = g - 1.96 * sqrt(v_g)
  upperCI = g + 1.96 * sqrt(v_g)
  return(c(g, v_g, lowerCI, upperCI))
}

# Cohen's d to Hedges'g
d2g <- function(d, nt, nc){
  
  g = des(d, nt, nc, verbose = F, dig = 5)[1,12:15]
  return(as.numeric(g))
  
}

#  F statistic to Hedges'g
f2g <- function(d, nt, nc){
  
  g = fes(d, nt, nc, verbose = F, dig = 5)[1,12:15]
  return(as.numeric(g))
  
}

# Peterson & Brown, 2005, <On the Use of Beta Coefficients in Meta-Analysis>
# This is for standardized beta only
# This function is unused in the current meta-analysis, 
# but could be useful for future analysis if one only reports standardized beta but nothing else

B2d <- function(B,n){
  # B to r
  lamda = sign(B)
  r = B + 0.05 * lamda
  # r to d
  d = res(r = r,n = n)
  return(d)
}
