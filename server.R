
## Credits ##########################################################################################################
# Program for the Growth and Yield Slash Pine model
# Author: Cristian R. Montes
# Date  : April 2022
# Version 1.5
# Please do not distribute beyond users from Timber Management Class


## Packages ######
library(shiny)
library(tidyverse)
library(reactable)
library(widgetframe)

## Read Data and Define FUNs #########
QMD  <- function(G, N)
{ # QMD = quadratic mean diameter
  # G = plot basal area
  # N = trees per acre  
  return(sqrt(G/(N*0.005454)))
}

##### Initial product diameter ranges #####
pulpwood  <- c(3,4)
chipnsaw  <- c(6,8)
sawtimber <- c(8,10)

##### Read in Wildlife Data #####
deer <- rio::import("deerNCC_est.csv") %>% filter(cp.requirements == 14)
diversity <- rio::import("diversity_est.csv")
quail <- rio::import("noboproxy_est.csv")

## Server Fxn #######
# shinyServer(
server <- function(input, output, session) {

  # deerData <- reactive({
  #   deer %>% filter(cp.requirements == 14)
  # })
  output$logo <- renderImage({list(src = "GAMELablogo.png")}, deleteFile = F)
## Reactive Data: my_base ########
my_base <- reactive({
  
  
  
  if(input$Location == "Piedmont")            source("loblolly_Piedmont.R")
  if(input$Location == "Lower Coastal Plain") source("loblolly_LCP.R")
  if(input$Location == "Upper Coastal Plain") source("loblolly_UPC.R")
  # if(input$Species == "Slash Pine")                                              source("slash.r")

  #Calculates everything for the BASE scenario
  Ages.Base = seq(1, 35, 1)
  Ns.Base   = c(650, N2(N1 = 650, A1 = 1, A2 = Ages.Base[-1]))
  Hs.Base   = H(input$Hi, Ages.Base)
  
  Gs.Base   = G(H = Hs.Base, N = Ns.Base, A = Ages.Base, Nt = 0,  Nb = Ns.Base, At = 0)
  
  Vs.Base   = V(H = Hs.Base, N = Ns.Base, G = Gs.Base,    A = Ages.Base)
  DWs.Base  = DW(H = Hs.Base, A = Ages.Base, N = Ns.Base, G = Gs.Base)
  Dq.Base   = QMD(G = Gs.Base, N = Ns.Base)

  Vpulp     = Vprod(V = Vs.Base, t = pulpwood[1],  D = Dq.Base,  N = Ns.Base, d = pulpwood[2])
  Vchns     = Vprod(V = Vs.Base, t = chipnsaw[1], D = Dq.Base,  N = Ns.Base, d = chipnsaw[2])
  Vsaw      = Vprod(V = Vs.Base, t = sawtimber[1],    D = Dq.Base,  N = Ns.Base, d = sawtimber[2])
  
  Vpulp     = Vpulp - (Vchns + Vsaw)
  Vchns     = Vchns  - Vsaw
  
  length.table = length(Ages.Base)
  
  my_base   = data.frame(A       = Ages.Base,
                         N       = Ns.Base,
                         H       = Hs.Base,
                         G       = Gs.Base,
                         V       = Vs.Base,
                         DW      = DWs.Base,
                         Dq      = Dq.Base,
                         Vpulp   = Vpulp,
                         Vchns   = Vchns,
                         Vsaw    = Vsaw,
                         Gpulp   = Vpulp/128 *  2.78,
                         Gchns   = Vchns/128 * 2.78,
                         Gsaw    = Vsaw /128 * 2.78,
                         Rpulp   = c(1:((length.table) - 1), Vpulp[length.table-1]/128 *  2.78 ),
                         RChnS   = c(1:((length.table) - 1), Vchns[length.table-1]/128 * 2.78),
                         RSaw    = c(1:((length.table) - 1), Vsaw[length.table-1]/128 * 2.78))
})

output$firecostUI <- renderUI({
    fluidRow(numericInput("cstBurn",
                 "Prescribed Fire Costs ($/acre)",
                 min = 0, max = 150,
                 value = 35,
                 width = "100px"),
    numericInput("cstShareBurn",
                 "Prescribed Fire Cost Share Received",
                 min = 0, 
                 max = 150,
                 value = 0,
                 width = "100px"))
  })

output$herbcostUI <- renderUI({
     fluidRow(numericInput("cstHerbicide",
                   "Herbicide Costs ($/acre)",
                   min = 0,
                   max = 200,
                   value = 50,
                   width = "100px"))
})

output$mixcostUI <- renderUI({
  fluidRow(numericInput("cstBurn",
                          "Prescribed Fire Costs ($/acre)",
                          min = 0, max = 150,
                          value = 35,
                          width = "100px"),
             numericInput("cstShareBurn",
                          "Prescribed Fire Cost Share Received",
                          min = 0, 
                          max = 150,
                          value = 0,
                          width = "100px"),
             numericInput("cstHerbicide",
                          "Herbicide Costs ($/acre)",
                          min = 0,
                          max = 200,
                          value = 50,
                          width = "100px"))
})

## Calculate Growth and Yield ########
  calc_table<- reactive({

    if(input$Location == "Piedmont")           source("loblolly_Piedmont.R")
    if(input$Location =="Lower Coastal Plain") source("loblolly_LCP.R")
    if(input$Location =="Upper Coastal Plain") source("loblolly_UPC.R")
    # if(input$Species == "Slash Pine")                                             source("slash.r")

    iAge      = 1 #input$Age[1]
    eAge      = 35 #input$Age[2]
    thAge     = input$ThAge
    thDens    = input$ThinDens
    pulpwood  = input$specPr1
    chipnsaw  = input$specPr2
    sawtimber = input$specPr3

    # ifelse(input$SI != "Site Index",
    #       Hi <- input$Hi,
          Hi <- H(input$Hi, iAge)#)
    iDens   = input$N0
    At      = thAge
    Nt      = thDens
    Nb      = N2(N1 = input$N0, A1 = iAge, A2 = thAge)

    Ages    = seq(iAge, eAge, 1)

        #Validate Consistency on the inputs
    if (iAge>= eAge)
      eAge = iAge + 1

    if (thAge<= iAge)
      thAge = iAge + 1

    #Calculate Stand Values Before Thinning
    mx   =  round(N2(N1 = iDens, A1 = iAge, A2 = thAge),0)

    # if(input$chk1 == FALSE)
    # {
    # Hs   = c(Hi,
    #          H2(H1 = Hi, A1 = iAge, A2 = Ages[-1]))
    # Ns   = c(iDens,
    #          N2(N1 = iDens, A1 = iAge, A2 = Ages[-1]))
    #
    # if(input$BA0 == 0){
    #   Gs   = G(H = Hs, N = Ns, A = Ages, Nt = 0, Nb = Ns, At = 0)}
    # else
    # { Gs   = G2(G1 = input$BA0,
    #             A2 = Ages,
    #             A1 = iAge,
    #             H2 = Hs,
    #             H1 = Hs[1],
    #             N2 = Ns,
    #             N1 = Ns[1],
    #             Nt = 0,
    #             Nb = Ns,
    #             At = max(Ages)+1)
    # }
    #
    # Vs   = V(H=Hs, N = Ns, G = Gs, A = Ages)
    # DWs  = DW(H = Hs, A = Ages,N = Ns,G = Gs)
    #
    # As   = Ages
    # }
    # else
    # {

    Ai    = seq(iAge, thAge, 1)
    Af    = seq(thAge, eAge, 1)
    Ai.l  = length(Ai)
    Ai.L  = Ai.l - 1
    Af.l  = length(Af)

    Hs    = c(Hi,
              H2(H1 = Hi, A1 = iAge, A2 = Ai[-1]),
              H2(H1 = Hi, A1 = iAge, A2 = Af))
    Ns   = c(iDens,
             N2(N1 = iDens, A1 = iAge, A2 = Ai[-1]),
             N2(N1 = thDens, A1 = thAge, A2 = Af))
    Gi   = G(H = Hs[1], N = Ns[1], A = Ages[1], Nt = 0, Nb = Ns[1], At = 0)
    Ga   = G(H = Hs[Ai.l], N = Ns[Ai.l+1], A = Ai[Ai.l], Nt = 0, Nb = Ns[Ai.l], At = 0)
    Gs   = c(Gi,
             G2(G1 = Gi,
                A2 = Ai[-1],
                A1 = iAge,
                H2 = Hs[2:Ai.l],
                H1 = Hs[1],
                N2 = Ns[2:Ai.l],
                N1 = Ns[1],
                Nb = Ns[2:Ai.l],
                Nt = Ns[2:Ai.l],
                At = Ai),
             G(Hs[Ai.l],Ns[Ai.l+1],Ages[Ai.l],0,Ns[Ai.l],0),
             G2(G1 = Ga,
                A2 = Af[-1],
                A1 = Ai[Ai.l],
                H2 = Hs[(Ai.l+2):(Af.l+Ai.l)],
                H1 = Hs[Ai.l+1],
                N2 = Ns[(Ai.l+2):(Af.l+Ai.l)],
                N1 = thDens,
                Nt = Ns[(Ai.l+1)] - thDens,
                Nb = Ns[Ai.l+1],
                At = thAge))

    Vs   = V(H = Hs, N = Ns, G = Gs, A = c(Ai, Af))
    DWs  = DW(H = Hs, A = c(Ai, Af),N = Ns,G = Gs)
    As  = c(Ai, Af)
    # }

    Dq.s = QMD(G = Gs, N = Ns)

    Vpulps     = Vprod(V = Vs, t = pulpwood[1], D = Dq.s, N = Ns,  d = pulpwood[2])
    Vchns      = Vprod(V = Vs, t = chipnsaw[1], D = Dq.s, N = Ns,  d = chipnsaw[2])
    Vsaw       = Vprod(V = Vs, t = chipnsaw[1], D = Dq.s, N = Ns,  d = sawtimber[2])

    Vpulps     = Vpulps - (Vchns + Vsaw)
    Vsaws      = Vchns  - Vsaw

    Vpulps     = ifelse(Vpulps   >= 0, Vpulps,   0)
    Vsaws      = ifelse(Vchns    >= 0, Vchns,    0)
    Vveneers   = ifelse(Vsaw     >= 0, Vsaw, 0)


    #Correct interface to reflect changes in density at the age of first thinning
    updateSliderInput(session, "ThAge",
                      min = 1, #input$Age[1],
                      max = 35#input$Age[2] - 1
                      )
    updateSliderInput(session, "ThinDens", max = mx )
    updateSliderInput(session, "cstDistributionAge",
                      min = 1,#input$Age[1],
                      max = 35,#input$Age[2]-1
                      )

    updateSliderInput(session,
                      inputId = "Hi",
                      label = "Stand Site Index (ft)",
                      value = input$Hi)

    ndata    <- length(Vpulps)

    my_table <- data.frame(A = As, N = Ns, H =  Hs, G = Gs, V = Vs, DW = DWs, Dq = Dq.s,
                           Vpulp       = Vpulps,
                           Vchns       = Vchns,
                           Vsaw        = Vsaw,
                           Gpulp       = Vpulps/128 *  2.78,
                           Gchns       = Vchns/128 * 2.78,
                           Gsaw        = Vsaw/128 * 2.78,
                           Remove.Plp  = 0,
                           Remove.ChnS = 0,
                           Remove.Saw  = 0)

  })
  
## Cost Calculation ##########
  calc_costs <- reactive({
      
      final_table         <- calc_table()
      # N1                  <- ifelse(input$Age[1] != 0, 
      #                               N1(input$N0, A2 = input$Age[1], A1 = 0),
      #                               input$N0)
      N1                  <- N1(input$N0, A2 = 1, A1 = 0)
      iage               <- final_table[1,1] #Initial Age
      e_age              <- final_table[nrow(final_table),1]
      cstAdministration  <- input$cstAdministration #Administration Cost
      
      seedling.cost      <- input$cstSeedling/1000 * N1   
      planting.cost      <- input$cstPlanting
      thinning.cost      <- input$cstThinning 
      harvesting.cost    <- input$cstHarvesting
      establishment.cost <- input$cstVeg
      
      # if(input$treatment %in% c("None","Herbicide")){
      #   burn.cost <- 0
      # } else {
      #   burn.cost <- input$cstBurn - input$cstShareBurn
      # }
      burn.cost          <- ifelse(input$pt_treatment %in% c("None","Herbicide"), 0 , input$cstBurn - input$cstShareBurn)
      
      # if(input$treatment %in% c("None", "Fire")){
      #   herbicide.cost <- 0
      # } else {
      #   herbicide.cost <- input$cstHerbicide
      # }
      
      herbicide.cost     <- ifelse(input$pt_treatment %in% c("None", "Fire"),0, input$cstHerbicide)
      
      pulpwood.price     <- input$Pr1 #Price Product 1
      chipnsaw.price     <- input$Pr2 #Price Product 2
      sawtimber.price    <- input$Pr3 #Price Product 3
      
      outs               <- apply(extraction(),2, diffinv)[-1,]
      thage              <- input$ThAge
      interest           <- input$cstInterest/100
      
      #We divide cu.ft/acre by 128 to get total coords
      #We multiply by 2.78 to calculate total Dry Weight (tons/MBF)
      incomes        <-  (pulpwood.price  * final_table[,8]/128 *  2.78 +
                          chipnsaw.price  * final_table[,9]/128 *  2.78 +
                          sawtimber.price * final_table[,10]/128 * 2.78) /(1+interest)^(final_table[,1] - iage) -
                          thinning.cost  /(1+interest)^(final_table[,1])   -
                          harvesting.cost/(1+interest)^(final_table[,1])  +
                         #add extractions
                          outs[,1] * pulpwood.price  /(1+interest)^(thage-iage) +
                          outs[,2] * chipnsaw.price  /(1+interest)^(thage-iage) +
                          outs[,3] * sawtimber.price /(1+interest)^(thage-iage)
      
      anual          <-   cstAdministration /(1+interest)^(final_table[,1] - iage)
      anual_perp     <-   cstAdministration/interest

      BLVn            <-  (pulpwood.price   * final_table[,8]/128 * 2.78 +
                           chipnsaw.price   * final_table[,9]/128 *2.78 +
                           sawtimber.price  * final_table[,10]/128 * 2.78) -
                           harvesting.cost
      
      # BLV              <- BLVn / ((1+interest)^(final_table[,1]) - 1) - anual_perp -
      #                     thinning.cost / ((1+interest)^(final_table[,1]) - 1) -
      #                     (seedling.cost *(1+interest)^(final_table[,1]))/((1+interest)^(final_table[,1]) - 1 ) -
      #                     (planting.cost *(1+interest)^(final_table[,1]))/((1+interest)^(final_table[,1]) - 1 ) -
      #                     (establishment.cost *(1+interest)^(final_table[,1]))/((1+interest)^(final_table[,1]) - 1 ) -
      #                     (burn.cost *(1+interest)^(final_table[,1]))/((1+interest)^(final_table[,1]) - 1 ) +
      #                     outs[,1] * pulpwood.price/((1+interest)^(final_table[,1])-1) +
      #                     outs[,2] * chipnsaw.price/((1+interest)^(final_table[,1])-1)+
      #                     outs[,3] * sawtimber.price/((1+interest)^(final_table[,1])-1)
      
      BLV              <- rep(NA, nrow(final_table))
      
      for(i in iage:e_age){
        if(input$pt_treatment == "Fire"){
          # if age is < thin age OR is in seq of unburned years
          # exclude fire cost
          if(i < thage | i %in% seq(thage + 1, 40, 2)){ 
            BLV[i] <- BLVn[i] / ((1+interest)^(final_table[i,1]) - 1) - anual_perp -
              thinning.cost / ((1+interest)^(final_table[i,1]) - 1) -
              (seedling.cost *(1+interest)^(final_table[i,1]))/((1+interest)^(final_table[i,1]) - 1 ) -
              (planting.cost *(1+interest)^(final_table[i,1]))/((1+interest)^(final_table[i,1]) - 1 ) -
              (establishment.cost *(1+interest)^(final_table[i,1]))/((1+interest)^(final_table[i,1]) - 1 )+
              outs[,1] * pulpwood.price/((1+interest)^(final_table[i,1])-1) +
              outs[,2] * chipnsaw.price/((1+interest)^(final_table[i,1])-1)+
              outs[,3] * sawtimber.price/((1+interest)^(final_table[i,1])-1)
          } else {
            BLV[i] <- BLVn[i] / ((1+interest)^(final_table[i,1]) - 1) - anual_perp -
              thinning.cost / ((1+interest)^(final_table[i,1]) - 1) -
              (seedling.cost *(1+interest)^(final_table[i,1]))/((1+interest)^(final_table[i,1]) - 1 ) -
              (planting.cost *(1+interest)^(final_table[i,1]))/((1+interest)^(final_table[i,1]) - 1 ) -
              (establishment.cost *(1+interest)^(final_table[i,1]))/((1+interest)^(final_table[i,1]) - 1 )  -
              (burn.cost*(1+interest)^(final_table[i,1]))/((1+interest)^(final_table[i,1]) - 1 ) +
              outs[,1] * pulpwood.price/((1+interest)^(final_table[i,1])-1) +
              outs[,2] * chipnsaw.price/((1+interest)^(final_table[i,1])-1)+
              outs[,3] * sawtimber.price/((1+interest)^(final_table[i,1])-1)
          }
        }
        # If treatment is "Mix", account for biannual burns and a one-time
        # herbicide expense in year of thinning.
        if(input$pt_treatment == "Mix"){
          if(i < thage | i %in% seq(thage + 1, 40, 2)){
            
              BLV[i] <- BLVn[i] / ((1+interest)^(final_table[i,1]) - 1) - anual_perp -
                thinning.cost / ((1+interest)^(final_table[i,1]) - 1) -
                (seedling.cost *(1+interest)^(final_table[i,1]))/((1+interest)^(final_table[i,1]) - 1 ) -
                (planting.cost *(1+interest)^(final_table[i,1]))/((1+interest)^(final_table[i,1]) - 1 ) -
                (establishment.cost *(1+interest)^(final_table[i,1]))/((1+interest)^(final_table[i,1]) - 1 )+
                outs[,1] * pulpwood.price/((1+interest)^(final_table[i,1])-1) +
                outs[,2] * chipnsaw.price/((1+interest)^(final_table[i,1])-1)+
                outs[,3] * sawtimber.price/((1+interest)^(final_table[i,1])-1)
              
            } else {
              if(i == thage){
                
                  BLV[i] <- BLVn[i] / ((1+interest)^(final_table[i,1]) - 1) - anual_perp -
                    thinning.cost / ((1+interest)^(final_table[i,1]) - 1) -
                    (seedling.cost *(1+interest)^(final_table[i,1]))/((1+interest)^(final_table[i,1]) - 1 ) -
                    (planting.cost *(1+interest)^(final_table[i,1]))/((1+interest)^(final_table[i,1]) - 1 ) -
                    (establishment.cost *(1+interest)^(final_table[i,1]))/((1+interest)^(final_table[i,1]) - 1 )  -
                    (burn.cost*(1+interest)^(final_table[i,1]))/((1+interest)^(final_table[i,1]) - 1 ) - 
                    (herbicide.cost*(1+interest)^(final_table[i,1]))/((1+interest)^(final_table[i,1]) - 1 ) + 
                    outs[,1] * pulpwood.price/((1+interest)^(final_table[i,1])-1) +
                    outs[,2] * chipnsaw.price/((1+interest)^(final_table[i,1])-1)+
                    outs[,3] * sawtimber.price/((1+interest)^(final_table[i,1])-1)
                  
            } else {
              
              BLV[i] <- BLVn[i] / ((1+interest)^(final_table[i,1]) - 1) - anual_perp -
                thinning.cost / ((1+interest)^(final_table[i,1]) - 1) -
                (seedling.cost *(1+interest)^(final_table[i,1]))/((1+interest)^(final_table[i,1]) - 1 ) -
                (planting.cost *(1+interest)^(final_table[i,1]))/((1+interest)^(final_table[i,1]) - 1 ) -
                (establishment.cost *(1+interest)^(final_table[i,1]))/((1+interest)^(final_table[i,1]) - 1 )  -
                (burn.cost*(1+interest)^(final_table[i,1]))/((1+interest)^(final_table[i,1]) - 1 ) + 
                outs[,1] * pulpwood.price/((1+interest)^(final_table[i,1])-1) +
                outs[,2] * chipnsaw.price/((1+interest)^(final_table[i,1])-1) +
                outs[,3] * sawtimber.price/((1+interest)^(final_table[i,1])-1)
              
            }
          }
        }
        
        if(input$pt_treatment == "Herbicide"){
              if(i != thage){
                
                BLV[i] <- BLVn[i] / ((1+interest)^(final_table[i,1]) - 1) - anual_perp -
                  thinning.cost / ((1+interest)^(final_table[i,1]) - 1) -
                  (seedling.cost *(1+interest)^(final_table[i,1]))/((1+interest)^(final_table[i,1]) - 1 ) -
                  (planting.cost *(1+interest)^(final_table[i,1]))/((1+interest)^(final_table[i,1]) - 1 ) -
                  (establishment.cost *(1+interest)^(final_table[i,1]))/((1+interest)^(final_table[i,1]) - 1 )+
                  outs[,1] * pulpwood.price/((1+interest)^(final_table[i,1])-1) +
                  outs[,2] * chipnsaw.price/((1+interest)^(final_table[i,1])-1)+
                  outs[,3] * sawtimber.price/((1+interest)^(final_table[i,1])-1)
                
              } else {
                BLV[i] <- BLVn[i] / ((1+interest)^(final_table[i,1]) - 1) - anual_perp -
                  thinning.cost / ((1+interest)^(final_table[i,1]) - 1) -
                  (seedling.cost *(1+interest)^(final_table[i,1]))/((1+interest)^(final_table[i,1]) - 1 ) -
                  (planting.cost *(1+interest)^(final_table[i,1]))/((1+interest)^(final_table[i,1]) - 1 ) -
                  (establishment.cost *(1+interest)^(final_table[i,1]))/((1+interest)^(final_table[i,1]) - 1 )  -
                  (herbicide.cost*(1+interest)^(final_table[i,1]))/((1+interest)^(final_table[i,1]) - 1 ) +
                  outs[,1] * pulpwood.price/((1+interest)^(final_table[i,1])-1) +
                  outs[,2] * chipnsaw.price/((1+interest)^(final_table[i,1])-1)+
                  outs[,3] * sawtimber.price/((1+interest)^(final_table[i,1])-1)
              }
        }
        
          if(input$pt_treatment == "None"){
              
              BLV[i] <- BLVn[i] / ((1+interest)^(final_table[i,1]) - 1) - anual_perp -
                thinning.cost / ((1+interest)^(final_table[i,1]) - 1) -
                (seedling.cost *(1+interest)^(final_table[i,1]))/((1+interest)^(final_table[i,1]) - 1 ) -
                (planting.cost *(1+interest)^(final_table[i,1]))/((1+interest)^(final_table[i,1]) - 1 ) -
                (establishment.cost *(1+interest)^(final_table[i,1]))/((1+interest)^(final_table[i,1]) - 1 )+
                outs[,1] * pulpwood.price/((1+interest)^(final_table[i,1])-1) +
                outs[,2] * chipnsaw.price/((1+interest)^(final_table[i,1])-1)+
                outs[,3] * sawtimber.price/((1+interest)^(final_table[i,1])-1)
              
          }
      }
                          
                         
      NPV            <-  incomes - anual
      
      out <- data.frame(Age = final_table[,1], NPV = NPV, BLV = BLV)
      return(out)         
       })  
  
## Volume extraction Calculation ###########
  extraction <-reactive({
  
           my_table    <- calc_table()         
           
           ln_inputs   <- length(my_table$Vpulp)
           ln_initage  <- my_table$A[1]
           ln_thage    <- input$ThAge
  
           Pulp       = c(rep(0, ln_inputs-1), my_table$Vpulp[ln_inputs])
           Chipnsaw   = c(rep(0, ln_inputs-1), my_table$Vchn[ln_inputs])
           Sawtimber  = c(rep(0, ln_inputs-1), my_table$Vsaw[ln_inputs])
           
           Pulprem    = rep(0, ln_inputs)
           Chnsrem    = rep(0, ln_inputs)
           Sawrem     = rep(0, ln_inputs)
           
           # if(input$chk1 == TRUE)
           # {
            Pulprem[ln_thage - ln_initage +1]  <- my_table$Vpulp[ln_thage - ln_initage +1] - my_table$Vpulp[2 + ln_thage - ln_initage] 
            Chnsrem[ln_thage - ln_initage +1]  <- my_table$Vchn[ln_thage - ln_initage + 1] - my_table$Vchn[2 + ln_thage - ln_initage]
            Sawrem[ln_thage - ln_initage +1]   <- my_table$Vsaw[ln_thage - ln_initage + 1] - my_table$Vsaw[2 + ln_thage - ln_initage]
           # }

            my_removals <- data.frame(Pulprem   = Pulprem, 
                                      Chnsrem   = Chnsrem, 
                                      Sawrem    = Sawrem, 
                                      Pulp      = Pulp, 
                                      Chipnsaw  = Chipnsaw, 
                                      Sawtimber = Sawtimber)
          })
  

########### Outputs ############

##### SI PLOT #### 
output$SIPlot <- renderPlotly({        
  my_base <- my_base()
  # plot(c(0,35), c(0,100),
  #      type = "n",
  #      xlim = c(0,35),
  #      ylim = c(0,100),
  #      xlab = "Age (years)",
  #      ylab = "Dom. Height (ft)")
  # 
  # rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],
  #      col = "grey80")
  # 
  # abline(v=seq(0,35,5), col = "white", lty = 2)
  # abline(h=seq(0,100,10), col = "white", lty = 2)
  # 
  # lines(H~A,
  #       data = my_base,
  #       lwd = 1,
  #       lty = 2,
  #       col ="black")

  new_data <- calc_table()

  # lines(H ~A,
  #       data = new_data,
  #       lwd = 3,
  #       col = "red")
  # 
  # SI = round(H2(new_data$H[1], A1 = new_data$A[1], 25),0)
  # 
  # text(5,85.5,paste("Site Index ", SI), cex = 2.5, col = "white")
  # text(5,86,paste("Site Index ", SI), cex = 2.5, col = "navy")
  # 
  # with(new_data,
  #      points(c(A[1], A[length(A)]), 
  #             c(H[1],H[length(H)]),
  #             pch = 21,
  #             col = "red",
  #             bg = "yellow"))
  
  plot_ly() %>%
    add_lines(data = my_base, x = ~A, y = ~H, color = I("black"), line = list(dash = "dash")) %>%
    add_lines(data = my_base, x = ~A, y = ~H, color = I("red")) %>%
    layout(xaxis = list(title = "Age"),
           yaxis = list(title = "Height"))
    })

 ##### Survival Plot #####
 output$SurvivalPlot <- renderPlotly({
   
   my_base <- my_base()
   
  # par(bg = "grey30", fg = "white", col = "white")
   # plot(c(0,35),c(0,1100), type = "n",
   #     xlab = "Age (years)",
   #     ylab = "Survival (trees/acre)")
   # 
   # 
   # rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = 
   #    "grey80")
   #    abline(v=seq(0,35,5), col = "white", lty = 2)
   #    abline(h=seq(0,1000,100), col = "white", lty = 2)
   # 
   # lines(N ~ A,
   #      my_base,
   #      lty = 2,
   #      col ="black")

   new_data <- calc_table() 

   # lines( N ~ A,
   #      new_data,
   #      lwd = 3,
   #      col = "red")
   # 
   # with(new_data, points(c(A[1], A[length(A)]),
   #              c(N[1],N[length(N)]),
   #              pch = 21,
   #              col = "red",
   #              bg = "yellow"))
   
   plot_ly() %>%
     add_lines(data = my_base,
               x = ~A,
               y = ~N,
               color = I("black"),
               line = list(dash = "dash"),
               name = "Base") %>%
     add_lines(data = new_data,
               x = ~A,
               y = ~N,
               color = I("red"),
               name = "Current") %>%
     layout(xaxis = list(title = list(text = "Age")),
            yaxis = list(title = list(text = "N")))
  })
 ##### Basal Area Plot ####### 
 output$BasalArea <- renderPlotly({my_base <- my_base()
                               # plot(c(0,35),c(0,300), type = "n",
                               # xlab = "Age (years)",
                               # ylab = "Basal Area (ft2/acre)")
                               # 
                               #  # Now set the plot region to grey
                               # rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = 
                               #     "grey80")
                               # abline(v=seq(0,35,5), col = "white", lty = 2)
                               # abline(h=seq(0,500,50), col = "white", lty = 2)
                               # 
                               # lines(G ~ A, my_base, lty = 2,
                               #       col ="black")
    
                               newData <- calc_table()
                               # lines(G ~ A, newData, lwd = 3, col = "red")
                               # with(newData, points(c(A[1], A[length(A)]), c(G[1], G[length(G)]), pch = 21, col = "red", bg = "yellow"))
                               
                               plot_ly() %>%
                                 add_lines(data = my_base,
                                           x = ~A,
                                           y = ~G,
                                           color = I("black"),
                                           line = list(dash = "dash"),
                                           name = "Base") %>%
                                 add_lines(data = newData,
                                           x = ~A,
                                           y = ~G,
                                           color = I("red"),
                                           name = "Current") %>%
                                 layout(xaxis = list(title = list(text = "Age (years)")),
                                        yaxis = list(title = list(text = "Basal Area (sq.ft/acre)")))
                               })
 ####### Deer Plot #####
 
 output$Deer <- renderPlotly({
   

   newData <- calc_table() %>%
     mutate(G = round(G, 1))

   newData$G <- round(newData$G, 1)
   newDeer <- merge(newData, deer, by.x = "G", by.y = "BA", all.x = F, all.y = F) %>%
     as_tibble() %>%
     mutate(med = (med/(365*2.47))*100)
   
   min.age  <- min(newDeer$A)
   max.age <- max(newDeer$A)
   
   control.ages <- min.age:input$ThAge
   one.year.ages <- seq(input$ThAge+1, max.age, 2)
   two.year.ages <- seq(input$ThAge+2, max.age, 2)
   
   
     controlData <- newDeer %>% filter(Treatment.Act == "Control") %>%
       arrange(A)%>%
       mutate(plotID = "controlplot")
     
     fireData <- newDeer %>% filter(Treatment.Act == "Control" & A %in% control.ages |
                                      Treatment.Act == "Fire" & A %in% one.year.ages & Recovery == 0 |
                                      Treatment.Act == "Fire" & A %in% two.year.ages & Recovery == 1) %>%
       arrange(A) %>%
       mutate(plotID = "fireplot")
   
     herbData <- newDeer %>% filter(Treatment.Act == "Control" & A %in% control.ages |
                                      Treatment.Act == "Herbicide" & A == input$ThAge+1 & Recovery == 0 |
                                      Treatment.Act == "Herbicide" & A > input$ThAge+1 & Recovery == 1) %>%
       arrange(A)%>%
       mutate(plotID = "herbplot")
   
     mixData <- newDeer %>% filter(Treatment.Act == "Control" & A %in% control.ages |
                                     Treatment.Act == "Mix" & A == input$ThAge+1 & Recovery == 0 |
                                     Treatment.Act == "Mix" & A %in% one.year.ages[-which(one.year.ages == input$ThAge+1)] & Recovery == 0 |
                                     Treatment.Act == "Mix" & A %in% two.year.ages & Recovery == 1) %>%
       arrange(A) %>%
       mutate(plotID = "mixplot")
  

   
   plot_ly() %>%
     add_lines(data=controlData, x = ~A, y = ~med, name = "None", color = I("black"), line = list(dash = "dash")) %>%
     add_lines(data=fireData, x = ~A, y = ~med, name = "Fire", color = I("red")) %>%
     add_lines(data=herbData, x = ~A, y = ~med, name = "Herbicide", color = I("darkgreen")) %>%
     add_lines(data=mixData, x = ~A, y = ~med, name = "Mix", color = I("darkorange")) %>%
     layout(xaxis = list(title = "Age"),
            yaxis = list(title = "Nutritional Carrying Capacity\n(deer/100 acres)",
                         autorange = F,
                         range = list(0,30)),
            legend = list(title = list(text = "Post-thin Treatment")))
       
 })

##### Quail Plot ######
 output$Quail <- renderPlotly({
   newData <- calc_table()
   
   newData$G <- round(newData$G, 1)
  quailData <- merge(newData, quail, by.x = "G", by.y = "BA", all.x = F, all.y = F) %>% 
     as_tibble()
   
   min.age  <- min(quailData$A)
   max.age <- max(quailData$A)
   
   control.ages <- min.age:input$ThAge
   one.year.ages <- seq(input$ThAge+1, max.age, 2)
   two.year.ages <- seq(input$ThAge+2, max.age, 2)
   
   controlData <- quailData %>% filter(Treatment.Act == "Control") %>%
     arrange(A)%>%
     mutate(plotID = "controlplot")
   
   fireData <- quailData %>% filter(Treatment.Act == "Control" & A %in% control.ages |
                                    Treatment.Act == "Fire" & A %in% one.year.ages & Recovery == 0 |
                                    Treatment.Act == "Fire" & A %in% two.year.ages & Recovery == 1) %>%
     arrange(A) %>%
     mutate(plotID = "fireplot")
   
   herbData <- quailData %>% filter(Treatment.Act == "Control" & A %in% control.ages |
                                    Treatment.Act == "Herbicide" & A == input$ThAge+1 & Recovery == 0 |
                                    Treatment.Act == "Herbicide" & A > input$ThAge+1 & Recovery == 1) %>%
     arrange(A)%>%
     mutate(plotID = "herbplot")
   
   mixData <- quailData %>% filter(Treatment.Act == "Control" & A %in% control.ages |
                                   Treatment.Act == "Mix" & A == input$ThAge+1 & Recovery == 0 |
                                   Treatment.Act == "Mix" & A %in% one.year.ages[-which(one.year.ages == input$ThAge+1)] & Recovery == 0 |
                                   Treatment.Act == "Mix" & A %in% two.year.ages & Recovery == 1) %>%
     arrange(A) %>%
     mutate(plotID = "mixplot")
   
   plot_ly() %>%
     add_lines(data=controlData, x = ~A, y = ~med, name = "None", color = I("black"), line = list(dash = "dash")) %>%
     add_lines(data=fireData, x = ~A, y = ~med, name = "Fire", color = I("red")) %>%
     add_lines(data=herbData, x = ~A, y = ~med, name = "Herbicide", color = I("darkgreen")) %>%
     add_lines(data=mixData, x = ~A, y = ~med, name = "Mix", color = I("darkorange")) %>%
     layout(xaxis = list(title = "Age"),
            yaxis = list(title = "Quail Habitat Quality Index",
                         autorange = F,
                         range = list(0,1)),
            legend = list(title = list(text = "Post-thin Treatment")))
   
 })
 
##### Plant Diversity Plot ######
 output$Diversity <- renderPlotly({
   newData <- calc_table()
   
   newData$G <- round(newData$G, 1)
  divData <- merge(newData, diversity, by.x = "G", by.y = "BA", all.x = F, all.y = F) %>% 
     as_tibble()
  
  min.age  <- min(divData$A)
  max.age <- max(divData$A)
   control.ages <- min.age:input$ThAge
   one.year.ages <- seq(input$ThAge+1, max.age, 2)
   two.year.ages <- seq(input$ThAge+2, max.age, 2)
   
   controlData <- divData %>% filter(Treatment.Act == "Control") %>%
     arrange(A)%>%
     mutate(plotID = "controlplot")
   
   fireData <- divData %>% filter(Treatment.Act == "Control" & A %in% control.ages |
                                      Treatment.Act == "Fire" & A %in% one.year.ages & Recovery == 0 |
                                      Treatment.Act == "Fire" & A %in% two.year.ages & Recovery == 1) %>%
     arrange(A) %>%
     mutate(plotID = "fireplot")
   
   herbData <- divData %>% filter(Treatment.Act == "Control" & A %in% control.ages |
                                      Treatment.Act == "Herbicide" & A == input$ThAge+1 & Recovery == 0 |
                                      Treatment.Act == "Herbicide" & A > input$ThAge+1 & Recovery == 1) %>%
     arrange(A)%>%
     mutate(plotID = "herbplot")
   
   mixData <- divData %>% filter(Treatment.Act == "Control" & A %in% control.ages |
                                     Treatment.Act == "Mix" & A == input$ThAge+1 & Recovery == 0 |
                                     Treatment.Act == "Mix" & A %in% one.year.ages[-which(one.year.ages == input$ThAge+1)] & Recovery == 0 |
                                     Treatment.Act == "Mix" & A %in% two.year.ages & Recovery == 1) %>%
     arrange(A) %>%
     mutate(plotID = "mixplot")
   
   plot_ly() %>%
     add_lines(data=controlData, x = ~A, y = ~med, name = "None", color = I("black"), line = list(dash = "dash")) %>%
     add_lines(data=fireData, x = ~A, y = ~med, name = "Fire", color = I("red")) %>%
     add_lines(data=herbData, x = ~A, y = ~med, name = "Herbicide", color = I("darkgreen")) %>%
     add_lines(data=mixData, x = ~A, y = ~med, name = "Mix", color = I("darkorange")) %>%
     layout(xaxis = list(title = "Age"),
            yaxis = list(title = "Plant Diversity\n(species/sq. meter)",
                         autorange = F,
                         range = list(0,20)),
            legend = list(title = list(text = "Post-thin Treatment")))
   
   
 })
 

 ##### Volume Plot #####
 output$Volume <- renderPlotly({
   my_base <- my_base()
       
   new_data <- calc_table()
  
   plot_ly() %>%
    add_lines(data = my_base,line = list(dash = "dash"), x = ~A, y = ~V, name = "Base", color = I("black"), linetype = 2) %>%
    add_lines(data = new_data, x = ~A, y = ~V, name = "Current Total",color = I("red")) %>%
    add_lines(data = new_data, x = ~A, y = ~Vpulp, name = "Pulpwood", color = I("blue")) %>%
    add_lines(data = new_data, x = ~A, y = ~Vchns, name = "Chip & Saw", color = I("green")) %>%
    add_lines(data = new_data, x = ~A, y = ~Vsaw, name = "Sawtimber", color = I("brown")) %>%
    layout(xaxis = list(title = list(text = "Age")),
           yaxis = list(title = "Volume (ft^3/acre)"))
   })

 
##### Dry Weight Plot ######
output$DryWeight <- renderPlotly({my_base <- my_base()
                                 # plot(c(0,35),c(0,100), type = "n",
                                 #      xlab = "Age (years)",
                                 #      ylab = "Dry Weight (Ton/acre)",
                                 #      main = "Total Weight ib.")
                                 #   
                                 # # Now set the plot region to grey
                                 # rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = 
                                 #        "grey80")
                                 # abline(v=seq(0,35,5), col = "white", lty = 2)
                                 # abline(h=seq(0,100,10), col = "white", lty = 2)
                                 #   
                                 new_data <- calc_table()
                                   
                                 # lines(DW ~ A, my_base, lty = 2,
                                 #       col = "black")
                                 # lines(DW ~ A, new_data, col = "red", lwd = 3)
                                 # with(new_data, 
                                 #      points(c(A[1], A[length(A)]), c(DW[1], DW[length(DW)]), pch = 21, col = "red", bg = "yellow"))
                                 # 
                                 plot_ly() %>%
                                   add_lines(data = my_base, 
                                             x = ~A, 
                                             y = ~DW, 
                                             color = I("black"),line = list(dash = "dash"),
                                             name = "Base") %>%
                                   add_lines(data = new_data,
                                             x = ~A, 
                                             y = ~DW, 
                                             color = I("red"),
                                             name = "Current") %>%
                                   layout(xaxis = list(title = list(text = "Age")),
                                          yaxis = list(title = list(text = "Dry Weight (tons/acre)")))
                                 })  

##### QDM Plot #####
 output$QMD <- renderPlotly({
   my_base <- my_base()
   new_data <- calc_table()
   plot_ly() %>%
      add_lines(data = my_base, 
                x = ~A, 
                y = ~Dq, 
                color = I("black"),line = list(dash = "dash"),
                name = "Base") %>%
      add_lines(data = new_data,
                x = ~A, 
                y = ~Dq, 
                color = I("red"),
                name = "Current") %>%
      layout(xaxis = list(title = list(text = "Age")),
             yaxis = list(title = list(text = "Quadratic Mean Diameter (in)")))
    })
 
 ##### Extraction Plot ######
 output$Extraction   <- renderPlotly({
   my_base <- my_base()
                             
   my_cols <- c("Gold", "darkseagreen1", "plum2")
   
   my_table <- calc_table()
   
   outs <- t(matrix(apply(extraction(), 2, diffinv)[length(extraction()[,1])+1,], nrow = 2, ncol = 3, byrow = TRUE))
   
   maxVol <- max(my_table$V[length(my_table[,1])], sum(outs[,1]), sum(outs[,2]))
   
   outs <- as.data.frame(outs)
   colnames(outs) <- c("Thinning Removals","Harvest Removals")  
   outs$Products <- c("Pulpwood", "Chip and Saw", "Sawtimber")
   
   # barplot(outs, col = my_cols,
   #         ylab = "Volume (ft3/acre)",
   #         ylim = c(0,ceiling(maxVol)+200),
   #         border = "grey20")
   # 
   # rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = 
   #        "grey80")
   # 
   # abline(h = seq(0,ceiling(maxVol)+200, 500), lty = 2, col = "white")
   # 
   # barplot(outs, col = my_cols,
   #         ylab = "Volume (ft3/acre)",
   #         ylim = c(0,ceiling(maxVol)+200),
   #         border = "grey20",
   #         #inside = FALSE,
   #         add = TRUE)
   # 
   # box(lwd = 3)
   # 
   # legend("topleft", 
   #        fill = my_cols,
   #        legend = c("Pulp", "Chip and saw", "Sawtimber"),
   #        border = "grey",
   #        density = 100)
   
   new.out <- outs %>% as_tibble() %>% 
     pivot_longer(cols = c(`Thinning Removals`, `Harvest Removals`)) %>%
     mutate(name = factor(name,
                          levels = c("Thinning Removals", "Harvest Removals")))
   
   plot_ly() %>%
     add_bars(data = new.out %>% filter(Products == "Pulpwood"), name = "Pulpwood", 
              x = ~name, y = ~value, color = I("darkgreen")) %>%
     add_bars(data = new.out %>% filter(Products == "Chip and Saw"), name = "Chip & Saw", 
              x = ~name, y = ~value, color = I("brown")) %>%
     add_bars(data = new.out %>% filter(Products == "Sawtimber"), name = "Sawtimber", 
              x = ~name, y = ~value, color = I("grey40")) %>%
     layout(xaxis = list(title = "Product"),
            yaxis = list(title = "Volume (ft^3/acre)",
                         rangemode = "nonnegative"),
            barmode = "stack")
   
   })
 ##### Yield Table #####
 output$Yield.Table <- renderReactable({
   
   Yield.table <- calc_table()
   
   out <- extraction()
   
   Yield.table[,14:16] <- (out[,1:3] + out[,4:6])/128 * 2.78
   
   colnames(Yield.table)<-c("Age (years)", 
                            "Dens. (trees/acre)", 
                            "Height (ft)", 
                            "BArea (ft2/acre)", 
                            "Vol ob (ft3/acre)",
                            "Dry Weight (ton/acre)",
                            "Dq (inches)",
                            "Pulp (ft3/acre)",
                            "Chip-n-Saw (ft3/acre)",
                            "Sawtimber (ft3/acre)",
                            "GW Pulp (ton/acre)",
                            "GW Chip-n-saw (ton/acre)",
                            "GW Sawtimber (ton/acre)",
                            "Extracted Pulp (ton/acre)",
                            "Extracted Chip-nsaw (ton/acre)",
                            "Extracted Sawtimber (ton/acre)")
   
   Yield.table <- Yield.table %>%
     mutate(`Dens. (trees/acre)` = round(`Dens. (trees/acre)`,0)) %>%
     mutate_at(.vars = 3:16, .funs = ~round(., digits = 2))
   
   theme <- reactableTheme(color = "hsl(0, 0%, 90%)",
                           backgroundColor = "hsl(0, 0%, 10%)",
                           borderColor = "hsl(0, 0%, 18%)",
                           stripedColor = "hsl(0, 0%, 13%)",
                           headerStyle = list(`&:hover[aria-sort]` = list(backgroundColor = "hsl(0, 0%, 14%)")),
                           tableBodyStyle = list(color = "hsl(0, 0%, 75%)"),
                           rowHighlightStyle = list(color = "hsl(0, 0%, 90%)",
                                                    backgroundColor = "hsl(0, 0%, 14%)"),
                           selectStyle = list(backgroundColor = "hsl(0, 0%, 20%)"),
                           inputStyle = list(backgroundColor = "hsl(0, 0%, 10%)",
                                             borderColor = "hsl(0, 0%, 21%)",
                                             `&:hover, &:focus` = list(borderColor = "hsl(0, 0%, 30%)")),
                           pageButtonHoverStyle = list(backgroundColor = "hsl(0, 0%, 20%)"),
                           pageButtonActiveStyle = list(backgroundColor = "hsl(0, 0%, 24%)"))

   reactable(Yield.table,
             resizable = TRUE,
             showPageSizeOptions = F,
             outlined = TRUE,
             highlight = TRUE,
             compact = TRUE,
             height = "auto",
             fullWidth = FALSE,
             defaultPageSize = 36,
             # columns = list(Age = colDef(name = "Age", defaultSortOrder = "asc",
             #                             align = "center"),
             #                BLV = colDef(name = "Base Land Value",
             #                             format = colFormat(currency = "USD",
             #                                                digits = 2),
             #                             align = "right"),
             #                NPV = colDef(name = "Net Present Value",
             #                             format = colFormat(currency = "USD",
             #                                                digits = 2),
             #                             align = "right")),
             theme = theme)
   })

##### Financial Plot #####
output$Financial <- renderPlotly({
  financial <- calc_costs()

  mxNPV <- financial$Age[financial$NPV == max(financial$NPV)]
  mxBLV <- financial$Age[financial$BLV == max(financial$BLV)]
  
  maxY <- max(max(financial$NPV),max(financial$BLV))
  minY <- min(min(financial$NPV), min(financial$BLV))
  
  financial %>%
  plot_ly(.,y = ~NPV, x = ~Age) %>%
    add_lines(name = "NPV",color = I("red")) %>%
    add_lines(name = "BLV",y = ~BLV, x =~ Age, color = I("green")) %>%
    layout(yaxis = list(title = list(text = "Value ($/acre)"),
                        zeroline = T,
                        zerolinecolor = I("black"),
                        zerolinewidth = 1.5),
           xaxis = list(range = list(10,35)))
                                
  })


##### Financial Table ######
output$FinanTable <- renderReactable({
  
  financial <- calc_costs()
  
  finanicial <- financial %>% mutate_at(.vars = c(2,3), ~round(.,2))
                                
  theme <- reactableTheme(color = "hsl(0, 0%, 90%)", 
                          backgroundColor = "hsl(0, 0%, 10%)", 
                          borderColor = "hsl(0, 0%, 18%)", 
                          stripedColor = "hsl(0, 0%, 13%)", 
                          headerStyle = list(`&:hover[aria-sort]` = list(backgroundColor = "hsl(0, 0%, 14%)")), 
                          tableBodyStyle = list(color = "hsl(0, 0%, 75%)"), 
                          rowHighlightStyle = list(color = "hsl(0, 0%, 90%)",
                                                   backgroundColor = "hsl(0, 0%, 14%)"), 
                          selectStyle = list(backgroundColor = "hsl(0, 0%, 20%)"), 
                          inputStyle = list(backgroundColor = "hsl(0, 0%, 10%)", 
                                            borderColor = "hsl(0, 0%, 21%)", 
                                            `&:hover, &:focus` = list(borderColor = "hsl(0, 0%, 30%)")), 
                          pageButtonHoverStyle = list(backgroundColor = "hsl(0, 0%, 20%)"), 
                          pageButtonActiveStyle = list(backgroundColor = "hsl(0, 0%, 24%)"))
  
  reactable(financial, 
            resizable = TRUE, 
            showPageSizeOptions = F,
            outlined = TRUE, 
            highlight = TRUE, 
            compact = TRUE, 
            height = "auto",
            fullWidth = FALSE, 
            defaultPageSize = 36,
            columns = list(Age = colDef(name = "Age", defaultSortOrder = "asc",
                                        align = "center"),
                           BLV = colDef(name = "Base Land Value",
                                        format = colFormat(currency = "USD", 
                                                           digits = 2),
                                        align = "right"),
                           NPV = colDef(name = "Net Present Value",
                                        format = colFormat(currency = "USD", 
                                                           digits = 2),
                                        align = "right")),
            theme = theme)
})

##### Distribution Plot ######
output$Distribution <- renderPlot({
  
  Yield.table <- calc_table() 
  
  Age <- input$cstDistributionAge
  
  Yield.table <- Yield.table[Yield.table$A == Age,]
  
  S <- H2(Yield.table$H, 25, Age)
  
  values      <- GetDiameterDistribution(Age = Age,
                                         N   = Yield.table$N,
                                         H   = Yield.table$H,
                                         G   = Yield.table$G,
                                         S   = S)
  
  barplot(values, names = seq(1:20), ylim = c(0,500), ylab = "Class population (trees/acre)", xlab = "Diameter Class (in)")
  
  box()

})

##### Download Handler #####
output$downloadData <- downloadHandler(
  filename = function() {
    paste(input$dataset, ".csv", sep = "")
  },
  content = function(file) {
    write.csv(calc_table(), file, row.names = FALSE)
  }
)

output$home_img <- renderImage({
  
  list(src = "GAMELablogo_white.png",
       width = "60%",
       height = "100%")
  
}, deleteFile = F)

output$planting <- renderPlot({
  num.rows <- round(sqrt(input$N0), 0)
  plot(x = seq(1,num.rows,1)+3,
       seq(1,num.rows,1),
       pch = 16)
})

# Stop running app when web browser window is closed
session$onSessionEnded(stopApp)

}
# )

##### Run App #####
# shinyApp(ui, server, options = list(launch.browser = T))


3/8

