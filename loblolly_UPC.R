#Loblolly Pine Growth and Yield Equations
#for the piedmont Area

H <- function(S,A){ 
  #H lobloly UPC
  #b0,b1,b2 parameters from the equation
  #S is site index
  #A is the Age
  b0 =   0.30323
  b1 =  -0.014452
  b2 =  -0.8216
  
  res <- S * (b0/(1-exp(b1 * A)))^b2
  return(res)} 

H2 <- function(H1,A2, A1){
  #H2 is height at Age A2 loblolly UPC
  #A1 is the projection Age
  #b1,b2,b2 parameters from the equation
  b1 = -0.014452 
  b2 =  0.8216
  
  res <- H1 * ((1-exp(b1*A2))/(1-exp(b1*A1)))^b2
  return(res)}


G  <- function(H,N,A,Nt,Nb,At){
  #G is the basal area for loblolly UPC
  #H is the dominant height
  #N is the number of trees per acre
  #Nt is the number of trees after thinning
  #Nb is the number of trees before thinning
  #A  is the stand age
  #b0..b5 are the function parameters
  
  b0 =  -0.855557 
  b1 = -36.050347
  b2 =   0.299071
  b3 =   0.980246
  b4 =   3.309212
  b5 =   3.787258
  
  
  res <- exp(b0 + b1*(1/A)      + b2 * log(N)   + b3* log(H) 
             + b4 * log(N)/A + b5 * log(H)/A)
  
             return(res)}

G2 <- function(G1, A2, A1, H2, H1, N2, N1, Nt, Nb, At){
  #G2 basal area at time A2 projecting from age A1 for loblolly UPC
  #H2, H1 are Dominant Heights at ages Age2 and Age1
  #N2, N1 are the number of trees at ages A2 and A1
  #Nb, Nt number of trees before and after thinning
  #At  thinning Age
  #b1..b5 equation parameters
  b1 = -36.050347
  b2 =   0.299071
  b3 =   0.980246
  b4 =   3.309212
  b5 =   3.787258
  
  res <- exp(log(G1) + b1 * (1/A2- 1/A1) + b2 * (log(N2) - log(N1))
             + b3 * (log(H2) - log(H1)) + b4 * (log(N2)/A2 - log(N1)/A1)
             + b5 * (log(H2)/A2 - log(H1)/A1))
}


N2 <- function(N1,A2, A1) {
  #N2 loblolly UPC
  b0 = -0.1524
  b1 =  0.9156

  res <- N1 * exp(b0*((A2/10)^b1-(A1/10)^b1))
  return(res)}

N1 <- function(N2, A2, A1)
{
  #N1 loblolly UPC
  b0 = -0.1524
  b1 =  0.9156

  res <- N2 / exp(b0*((A2/10)^b1-(A1/10)^b1))
  return(res)}


V  <- function(H, N, G, A) {
  #V loblolly UPC
  b0 =  2.6275
  b1 = -0.6725
  b2 = -0.3875
  b3 =  1.6193
  
  
  res <- exp(b0 + b1 * log(H)/A + b2 * log(N) + b3 * log(G))
  return(res)}

V2 <- function(H2,H1,A2,A1,N2, N1,G2,G1){
  #V2 loblolly UPC
  b1 = -0.6725
  b2 = -0.3875
  b3 = 1.6193
  
  res <- exp(log(V1) + b1 * (log(H2)/A2 - log(H1)/A1) 
             + b2 * log(N2)- log(N1) + b3 * (log(G2)-log(G1)))
  return(res)
}

Vprod <- function(V, t, D, N, d){
  #Vprod loblolly UPC
  b0 = -0.5217
  b1 =  3.8386
  b2 = -0.6887
  b3 = -0.1167
  b4 =  5.7185
  
  res <- V * exp(b0 * (t/D)^b1 + b2 * N^b3 * (d/D)^b4)
  return(res)
}

DW  <- function(H,A,N,G){
  #DW loblolly UPC
  b0 = -1.1192
  b1 = -1.8680
  b2 = -0.5402
  b3 =  1.7524
  
  res <- exp(b0 + b1 * log(H)/A + b2 * log(N) + b3 * log(G))
}

DW2 <- function(DW1, H2,A2, H1, A1, N2, N1, B2, B1) {
  #DW2 loblolly UPC
  b1 = -1.8680
  b2 = -0.5402
  b3 =  1.7524
  
  res <- exp(log(DW1)+ b1* (log(H2)/A2 - log(H1)/A1)
             + b2 * (log(N2)- log(N1)) + b3 * (log(G2)- log(G1)))
  return(res)
}

VIB <- function(V,t,Dq,N,d)
{
  #VIB loblolly UPC
  b0 = -0.5217
  b1 =  3.8386
  b2 = -6.887
  b3 = -0.1167
  b4 =  5.7185
  
  res <- V * exp(b0 * (t/Dq)^b1 + b2 * N^b3 * (d/Dq)^b4)
  return(res)
}


################################ Merchandizing ##########################################################
GetWeibullParameters<- function(Age, N, H, G, S)
{
  #After Borders 1990 PMRC Report
  RS = (sqrt(43560/ N)) / H #relative spacing index
  
  Dhat.0  = exp( 1.8945007+ 0.9472899 * log(G/N)   + 0.0069688  * S)
  Dhat.50 = exp( 2.9142853+ 0.5379221 * log(G/N))
  Dhat.25 = exp(-1.7792268+ 1.2742723 * log(Dhat.50) + 0.44517*RS + 0.1578344*log(N))
  Dhat.95 = exp( 0.5636465+ 0.9031187  * log(Dhat.50) - 0.19434368 * RS)
  
  DQuad = sqrt((G/N)/0.005454154)
  
  a = ((N / 10.0)^(1.0 / 3.0) *  Dhat.0 - Dhat.50) / ((N / 10.0)^ (1.0 / 3.0) - 1.0)
  
  if (a < 0.0) a = 0.0
  
  c = 2.343088/(log(Dhat.95-a)-log(Dhat.25-a))
  
  rho1 = gamma(1 + (1.0 / c))
  rho2 = gamma(1 + (2.0 / c))
  
  b = - (a * rho1 / rho2) + (((a^2)/(rho2^2) *(rho1^2-rho2)+ (DQuad^2)/rho2))^0.5
  
  values<- c(a,b,c)
  return(values)
}


GetWeibullValue<- function(UPDbh, DwnDbh,a,b,c)
{
  x = (1 - exp(-((UPDbh  - a) / b)^c)) -(1 - exp( - ((DwnDbh - a) / b)^c))
  return(x)
}


GetDiameterDistribution<- function(Age, N, H, G, S)
{
  params <- GetWeibullParameters(Age, N, H, G, S)
  
  a      <- params[1]
  b      <- params[2]
  c      <- params[3]
  
  if(a < 3.0)
    Init_Class=3
  else 
    Init_Class = a
  
  result <- numeric(20)  
  
  for (x in seq(1,20,1))
    result[x] <- N * GetWeibullValue(x + 0.5,x - 0.499999999, a, b, c)
  
  return(result)
}

