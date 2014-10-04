
library(dplyr)


# Replaced 'input$' with 'input_'
# Note that 'input' does NOT refer to system inputs but UI inputs


# Areas in ha inputs
input_WetlandsArea  <- input$wetlandsArea
input_PondsArea  <- NloadIn1$ponds
input_NatVegArea  <- NloadIn1$NatVeg
input_TurfArea <- NloadIn1$Turf
input_AgArea  <- NloadIn1$Ag + NloadIn1$ActAg

input_ImpervArea  <- NloadIn1$Imperv
input_LawnArea  <- NloadIn1$Lawns
input_GolfArea <- NloadIn1$Golf

# Roof and driveways add up to contribute to impervious surfaces
# input_RoofArea  <- 80
# input_DrivewayArea <- 80
# 
# input_AtmDepRate <- 15.1
# input_NtransNatVeg <- .35
# input_NtransTurf <- .38
# input_NtransAg <- .38
# 	
# input_FertLawns <- 122
# 
# input_PercentHomes <- 
# input_DeNit <- 
# input_FertAg <- 136

# input_Fert <- 
# 
# input_TransTurf <- .61
# input_NtransWetlands <- .22
# input_NtransPonds <- .44
# 
# input_HumanLoad  <- 4.8
# input_HouseSize  <- 
# input_NumbHomesSeptic  <- 
# input_NotLostSpetic
# input_NotLostLeach
# input_NotLostPlume
# input_NotLostAquifer
# input_PropHomesCess <- .53
# input_AvgAnSTPLoad
# input_TotAnFlow

	
#
# Atmospheric Loads =============================================================
#a
AtmNatVeg <- function(){
	return(input_AtmDepRate * input_NatVegArea * input_NtransNatVeg) %>% round(1)
}
#b
AtmTurf <- function(){
	return(input_AtmDepRate * input_TurfArea * input_TransTurf) %>% round(1)
}
#c
AtmAg <- function(){  ## Not working!
	return(input_AtmDepRate * input_AgArea * input_NtransAg) %>% round(1)
}
#d
AtmImperv <- function(){
	return((input_AtmDepRate * (input_RoofArea + input_DrivewayArea) * input_NtransTurf) * (input_AtmDepRate * input_ImpervArea)) %>% round(1) #Need help with this formula
}
#e
AtmWetlands <- function(){
	return(input_AtmDepRate * input_WetlandsArea * input_NtransWetlands) %>% round(1)
}
#f
AtmPonds <- function(){
	return(input_AtmDepRate * input_PondsArea * input_NtransPonds) %>% round(1)
}

## Total N load to estuary sourced from Atmospheric Deposition
TotalLoadAtmospheric <- function(){
	return(AtmNatVeg() + AtmTurf() + AtmAg() + AtmImperv() + AtmWetlands() + AtmPonds()) %>% round(1)
}
# Fertilizer Application Loads ===================================================		

#g
# REWORK
FertTurf <- function(){
	return(input_FertLawns * input_LawnArea * input_PercentHomes * input_DeNit) %>% round(1)
}
#h 
# REWORK
FertAg <- function(){
	return(input_FertAg * input_AgArea * input_DeNit) %>% round(1)
}
#i
# REWORK
FertGolf <- function(){
	return(input_Fert * input_GolfArea * input_Denit) %>% round(1)
}

## Total Fertilixation Load
# REWORK
TotalFertLoad <- function(){
	return(FertTurf() + FertAg() + FertGolf()) %>% round(1)
}
# Surface Loads- Fertilizer and Deposition ======================================
#j
# REWORK
SurfaceLoad <- function(){
	return((TotalLoadAtmospheric() + TotalFertLoad()) * 0.39 * 0.65) %>% round(1)
}
#k
# REWORK
SepticLoad <- function(){
	return(input_HumanLoad * input_HouseSize * input_NumbHomesSeptic * input_NotLostSpetic * input_NotLostLeach * input_NotLostPlume * input_NotLostAquifer) %>% round(1)
}
#l
# REWORK
CesspoolLoad <- function(){
	return(input_HumanLoad * input_HouseSize * input_PropHomesCess * input_NotLostSpetic * input_NotLostPlume * input_NotLostAquifer) %>% round(1)
}
#m
# REWORK
WasteWaterLoad <- function(){
	return(input_AvgAnSTPLoad * input_TotAnFlow) %>% round(1)
}

# Total Nitrogen Loading to Estuary =============================================
NLoadTotal <- function(){
	return(SurfaceLoad() + SepticLoad() + CesspoolLoad() + WasteWaterLoad()) %>% round(1)
}
