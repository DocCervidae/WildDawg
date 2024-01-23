#Helper functions

Get_ShapeParameter<- function(value)
{
  f1 = 4.9022 #from Tauyield
  
  gln = gamma(1.0 + 2.0 / (f1 * value))
  pw  = (f1*value/2.0)
  res = -gln^pw
  
  return (res)
}


MinMaxdbh <- function(BA, N, Ga, Gb, YST,  tf, Age)
{ 
  
  f1   <- 4.9022
  TyTr <- numeric()
  
  Qd   <- sqrt((BA/N)/0.005454154)
  
  
  if ((Ga == Gb)|(tf < 2)) #row thinning
     TyTr=1
  else
    TyTr = TYRespToThinning(Ga, Gb, Age, YST);
  
  
  c = Get_ShapeParameter(TyTr)
  
  if (N <= 0.49) N = 0.5
  
  min_dbh  = Qd * (log( (N-0.49) / N) / c)^(1.0 / (f1 * TyTr)) + 0.49999999
  max_dbh  = Qd * (log(   0.49   / N) / c)^(1.0 / (f1 * TyTr)) + 0.49999999
  nclasses = max_dbh - min_dbh +1
  
  return(list(min_dbh  = min_dbh,
              max_dbh  = max_dbh,
              nclasses = nclasses)) 
} 

TYRespToThinning<- function(Ga, Gb, Age, YST)
{
  
  r = -4.3136
  k = 18.2586
  
  value <- (Ga / Gb)^((r * ( - YST)^ 2  + k * (YST)) / Age^2)
  if (Ga==0) value <- 1
  
  return(value)
}


TNMerchantize <- function(Ga,     Gb,  BA,
                          N,      Age, Class,
                          TCount, YST, Qd)
{
  ttt <- numeric()
  f1 = 4.9022
  
  
  if (TCount < 1)
    ttt = 1;
  else
    ttt = TYRespToThinning(Ga, Gb, Age, YST, -4.3136, 18.2586)
  
  y    =  (Class/Qd)^(f1*ttt)
  egam =  gamma(1.0+2.0/(f1*ttt))
  b1   = - (sqrt(egam))^(f1*ttt)
  
  return (N*exp(b1*y))
  
}


#Merchandizing Algorithm

TMerchantise <-function(Ga, Gb,
                        N,  Dh, BA,
                        Age,  TCount, YST,
                        TThresh,   BAremove,
                        Ghost)
{
  TVol <- numeric()  #Total Yield
  MVol <- numeric()  #Merchantable Yield
  
  CClass         <- numeric() #Label for the Class
  CNTrees        <- numeric() #Number of Trees Counter
  CNTreesRemain  <- numeric() #Remaining Trees Counter
  CNTreesRemoved <- numeric() #Removed Trees Counter 
  
  
  CBA            <- numeric() #Basal Area Counter
  CBARemain      <- numeric() #Basal Area Remained Counter
  CBARemoved     <- numeric() #Basal Area Removed Counter
  
  InitialClass   <- 0  #Specifies the initial Class
  
  
  Qd             <- sqrt((BA/N)/0.005454154) #Calculates Basal Area
  
  BottomClass    <- numeric() #Specifies bottom class
  TopClass       <- numeric() #Specifies top class
  
  GhostFactor    <- numeric() #Specifies Ghosting Factor
  GhostCounter   <- numeric() #Specifies number of ghosting trees
  
  SawtimberProportion = 0     #Specifies initial SawTimber Proportion
  
  
  GhostFactor        = 1
  GhostCounter       = 0
  
  Remove$TPA         = 0
  Remove$BA          = 0
  Remove$TotalVolume = 0
  Remove$Pulpwood    = 0
  Remove$Chip_N_Saw  = 0
  Remove$SawTimber   = 0
  
  Remain$TPA         = 0
  Remain$BA          = 0
  Remain$TotalVolume = 0
  Remain$Pulpwood    = 0
  Remain$Chip_N_Saw  = 0
  Remain$SawTimber   = 0
  
  
  
 #    Get minimum and maximum dbh classes for loop counters
  
  MyClass <- MinMaxdbh( BA, N, Ga, Gb, YST,  TCount, Age)
  
  if (MyClass$min_dbh > MyClass$max_dbh) 
        return()
  
  if (InitDiameterClass > MyClass$min_dbh)  InitDiameterClass = MyClass$min_dbh
  
  LastDiameterClass     = MyClass$max_dbh
  
  TVol              = TYield(N, BA, Dh, Age, TCount, TThresh, YST)
  
  class.interval    = My_Class$max_dbh - My_Class$min_dbh + 1
  CClass            = numeric(class.interval)
  CNTrees           = numeric(class.interval)
  CNTreesRemain     = numeric(class.interval)
  CNTreesRemoved    = numeric(class.interval)
  CBA               = numeric(class.interval)
  CBARemain         = numeric(class.interval)
  CBARemoved        = numeric(class.interval)
  

  #Start loop to get product breakdowns
  
  if (YST!=0)                #get values per class without thinning
  { 
    for (x in (1:class.interval))
    {
      CClass[x]   =  x
      
      if (x==1)
      {
        BottomClass = 0.0
        TopClass    = 1.5
      }
      else
      {
        BottomClass = x - 0.499999999
        TopClass    = x + 0.5
      }
      
      CNTrees[x] = TNMerchantize(Ga, Gb, BA, N, Age, BottomClass, TCount, YST, Qd) - TNMerchantize(Ga, Gb, BA, N, Age, TopClass, TCount, YST, Qd)
      
      
      CNTreesRemain[x]  = CNTrees[x]
      CNTreesRemoved[x] = 0
      
      CBA[x]            = 0.005454 * CClass[x]^2 * CNTrees[x]
      CBARemain[x]      = CBA[x]
      CBARemoved[x]     = 0
    }
    
    Remove$BA = 0
 
      #calculates products without thinnings
  } 
  else if ((YST==0) & (TCount==1))  #do row thinning
  {
    
    for (x in (1:class.interval))
    {
      CClass[x]  = x
      
      if (x==1)
      {
        BottomClass = 0.0
        TopClass    = 1.5
      }
      
      else
      {
        BottomClass = x - 0.49999999
        TopClass    = x + 0.5
      }
      
      CNTrees[x] = TNMerchantize(Ga,Gb,BA,N,Age,BottomClass,TCount, YST,Qd) - TNMerchantize(Ga,Gb,BA,N,Age,TopClass,TCount,YST,Qd)
      
      CNTreesRemain[x]  = CNTrees[x] * (1 - BAremove)
      CNTreesRemoved[x] = CNTrees[x] * BAremove
      Remove$TPA        = Remove$TPA + CNTreesRemoved[x]
      
      CBA[x]            = 0.005454 * CClass[x] ^2 * CNTrees[x]
      CBARemain[x]      = CBA[x] * ( 1 - BAremove)
      CBARemoved[x]     = CBA[x] - CBARemain[x]
      
      Remove.BA         = Remove$BA + CBARemoved[x]

    }
  }
  
  else  if ((YST==0)&(TCount>1))  #do the low thinning
  {
    BAtoRemove     = BA * BAremove
    BARemovedSoFar = 0
    
    
    for (x in 1:class.interval)
    {
      CClass[x]  = x
      
      if (x==1)
      {
        BottomClass = 0.0
        TopClass    = 1.5
      }
      
      else
      {
        BottomClass = x - 0.499999999
        TopClass    = x + 0.5
      }
      
      CNTrees[x] = TNMerchantize(Ga,Gb,BA,N,Age,BottomClass,TCount, YST,Qd) - TNMerchantize(Ga,Gb,BA,N,Age,TopClass,TCount,YST,Qd)
      
      CBA[x]            = 0.005454 * CClass[x]^2 *CNTrees[x]
      CNTreesRemoved[x] = 0
      CNTreesRemain[x]  = CNTrees[x]

      #Ghosting Trees
      if (x>=Ghost.ClassThshld)
      {
        GhostFactor = 1.0 + (GhostCounter/(N-GhostCounter))
      }
      
      else if ((x==Ghost.ClassThshld) & (CNTrees[x] > Ghost.TreesInClassThshld))
      {
        GhostCounter = GhostCounter + CNTrees[x] - Ghost.TreesInClassThshld
      }
      
      else if ((x < Ghost.ClassThshld) & (CNTrees[x] > Ghost.TreesInClassThshld))
        
      {
        GhostCounter  = GhostCounter + CNTrees[x]
        GhostFactor   = 0.0
      }
      
      #end of Ghosting Trees
      
      if (BAtoRemove-BARemovedSoFar!=0)
      {
        if (CBA[x] <= (BAtoRemove-BARemovedSoFar))
        {
          CNTreesRemain[x]  = 0
          CNTreesRemoved[x] = CNTrees[x] * GhostFactor
          Remove.TPA        = Remove$TPA + CNTreesRemoved[x]
          BARemovedSoFar    = Remove$TPA + 0.005454 * CClass[x] * CClass[x] * CNTreesRemoved[x]
          CBARemain[x]      = 0
          CBARemoved[x]     = CBA[x]
          
          Remove$BA         = Remove$BA + CBARemoved[x]

        }
        else  #(CBA[x] > (BAtoRemove-BARemovedSoFar))
        {
          CBARemoved[x]     = BAtoRemove - BARemovedSoFar
          CBARemain[x]      = CBA[x] - CBARemoved[x]
          
          CNTreesRemain[x]  = CNTrees[x] * (CBARemain[x]/CBA[x]) * GhostFactor
          if (CNTreesRemain[x]<0) CNTreesRemain[x] = 0
          CNTreesRemoved[x] = CNTrees[x] - CNTreesRemain[x]
          Remove$TPA        = Remove$TPA + CNTreesRemoved[x]
          BARemovedSoFar    = BAtoRemove
          
          Remove$BA         = Remove$BA + CBARemoved[x]
          
          Ghost$ClassThshld        = x
          Ghost$TreesInClassThshld = CNTreesRemain[x]

        }
      }

    }

  }
  
  
  if (Ghost$ClassThshld>MyClass$min_dbh)
           InitialClass = Ghost$ClassThshld
      else InitialClass = MyClass$min_dbh

  
#Here comes the product break down  
    
  for (x in MyClass$min_dbh:MyClass$max_dbh)
  {
    BottomClass = x - 0.49999999
    TopClass    = x + 0.5
    
    if (x<=12)
    SawtimberProportion      = SawtimberVolumeProportion[x]
    else SawtimberProportion = 1.0
    
    if ((x>=Products[0].MinDBH) & (x<Products[1].MinDBH))
    {
      MVol = TYieldm(BA, BottomClass, N, TVol, Products[0].TopDBH, TCount, TThresh, YST) - TYieldm(BA, TopClass, N, TVol, Products[0].TopDBH, TCount, TThresh, YST)
      
      if (CNTreesRemain[x]>0)
      {
        Remove.Pulpwood = Remove$Pulpwood + MVol * CNTreesRemoved[x]/CNTrees[x]
        Remain.Pulpwood = Remove$Pulpwood + MVol * (1-(CNTreesRemoved[x]/CNTrees[x]))
      }
      else
      {
        Remove.Pulpwood = Remove$Pulpwood + MVol
        Remain.Pulpwood = Remove$Pulpwood + 0
      }
    }
    
    else
      
      if ((x>=Products[1].MinDBH) & (x<Products[2].MinDBH))
      {

        MVol = TYieldm(BA, BottomClass, N, TVol, Products[1].TopDBH, TCount, TThresh, YST) - TYieldm(BA, TopClass,    N, TVol, Products[1].TopDBH, TCount, TThresh, YST);
        
        if (CNTreesRemain[x]>0)
        {
          Remove.Chip_N_Saw = Remove$Chip_N_Saw + MVol * CNTreesRemoved[x]/CNTrees[x]
          Remain.Chip_N_Saw = Remove$Chip_N_Saw + MVol * (1-(CNTreesRemoved[x]/CNTrees[x]))
        }
        else
        {
          Remove.Chip_N_Saw = Remove$Chip_N_Saw + MVol
          Remain.Chip_N_Saw = Remove$Chip_N_Saw + 0
        }
      }
    
    else
      
      if (x>=Products[2].MinDBH)
      {
        MVol = TYieldm(BA, BottomClass, N, TVol, Products[2].TopDBH, TCount, TThresh, YST) - TYieldm(BA, TopClass,    N, TVol, Products[2].TopDBH, TCount, TThresh, YST)
        
        if (CNTreesRemain[x]>0)
        {
          Remove.SawTimber = Remove$SawTimber + MVol * CNTreesRemoved[x]/CNTrees[x] * SawtimberProportion
          Remain.SawTimber = Remove$SawTimber + MVol * (1-(CNTreesRemoved[x]/CNTrees[x])) * SawtimberProportion
        }
        else
        {
          Remove.SawTimber = Remove$SawTimber + MVol * SawtimberProportion
          Remain.SawTimber = Remove$SawTimber + 0
        }
      }
    
    PRMdist[Age][x] =  CNTrees[x] * GhostFactor
    
  }

}