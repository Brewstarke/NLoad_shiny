
library(shiny)
library(reshape2)
library(dplyr)
library(rCharts)
library(ggplot2)
library(RColorBrewer)

# Run any data manpluations or function creations here
# N-load backbone formula


shinyServer( # this will be run each time a user changes something.
	function(input, output) {
		#a
		AtmNatVeg <- function(){
			return(input$AtmDepRate * input$NatVegArea * input$NtransNatVeg) %>% round(1)
		}
		#b
		AtmTurf <- function(){
			return(input$AtmDepRate * input$TurfArea * input$TransTurf) %>% round(1)
		}
		#c
		AtmAg <- function(){  ## Not working!
			return(input$AtmDepRate * input$AgArea * input$NtransAg) %>% round(1)
		}
		#d
		AtmImperv <- function(){
			return((input$AtmDepRate * (input$RoofArea + input$DrivewayArea) * input$NtransTurf) * (input$AtmDepRate * input$ImpervArea)) %>% round(1) #Need help with this formula
		}
		#e
		AtmWetlands <- function(){
			return(input$AtmDepRate * input$WetlandsArea) %>% round(1)
		}
		#f
		AtmPonds <- function(){
			return(input$AtmDepRate * input$PondsArea * input$ThroughAquiferPonds) %>% round(1)
		}
	
		## Total N load to estuary sourced from Atmospheric Deposition
		TotalLoadAtmospheric <- function(){
			return(AtmNatVeg() + AtmTurf() + AtmAg() + AtmImperv() + AtmWetlands() + AtmPonds()) %>% round(1)
		}
		
		
		
		##Via fertilizer application
		
		#g
		FertTurf <- function(){
			return(input$FertLawns * input$LawnArea * input$PercentHomes * input$DeNit) %>% round(1)
		}
		#h 	
		FertAg <- function(){
			return(input$FertAg * input$AgArea * input$DeNit) %>% round(1)
		}
		#i
		FertGolf <- function(){
			return(input$Fert * input$GolfArea * input$Denit) %>% round(1)
		}
		
		TotalFertLoad <- function(){
			return(FertTurf() + FertAg() + FertGolf()) %>% round(1)
		}
		
		#j
		SurfaceLoad <- function(){
			return((TotalLoadAtmospheric() + TotalFertLoad()) * 0.39 * 0.65) %>% round(1)
		}
		#k
		SepticLoad <- function(){
			return(input$HumanLoad * input$HouseSize * input$NumbHomesSeptic * input$NotLostSpetic * input$NotLostLeach * input$NotLostPlume * input$NotLostAquifer) %>% round(1)
		}
		#l
		CesspoolLoad <- function(){
			return(input$HumanLoad * input$HouseSize * input$NumbHomesCess * input$NotLostSpetic * input$NotLostPlume * input$NotLostAquifer) %>% round(1)
		}
		#m
		WasteWaterLoad <- function(){
			return(input$AvgAnSTPLoad * input$TotAnFlow) %>% round(1)
		}
		
		#Nitrogen Loading to Estuary
		NLoadTotal <- function(){
			return(SurfaceLoad() + SepticLoad() + CesspoolLoad() + WasteWaterLoad()) %>% round(1)
		}

# 	output$TotalLoadAtmosphericOut <- renderText({
# 		paste("Your total atmospheric load is ", as.character(TotalLoadAtmospheric()) ,"KG N/ha/year")
# 	})
# 	output$TotalFertLoadOut <- renderText({
# 		paste("The total load from fertilizer under this scenario is ", as.character(TotalFertLoad()), "KG N/ha/year")
# 	})
# 	output$TotalWasteWaterLoad <- renderText({
# 		paste("The total Nitrogen load from wastewater is", as.character(WasteWaterLoad()), "KG N/ha/year")
# 	})
# 	ouput$NLoadTotalOut <- renderText({
# 		paste("Nitrogen load to the estuary ", as.character(NLoadTotal()), "kg N/ha/yr")
# 	})
	## Summary plot-
	
	
	output$SummaryStackBar <- renderChart({

		NLoad_names <- names(NLoad_outs) # create vectoir list of names for melt/cast
		
		NLoads.Melt <- NLoad_outs %>%
			select(1:8) %>%	
			arrange(septic_NLoad) %>%
			melt(id.vars = NLoad_names[1:2])
		
		
		HSbar <- dPlot(y = "subwatershed_code", x = "value", data= NLoads.Melt, groups= "variable", type = "bar", height = 700, width= 700)
		HSbar$yAxis(type= "addCategoryAxis", orderRule = 'rev(value)')
		HSbar$xAxis(type= "addMeasureAxis")
		HSbar$legend(
			x = 0, 
			y = 0, 
			width = 500, 
			height = 1500,
			horizontalAlign = "center")
		HSbar$defaultColors(brewer.pal(6, "Set2"))
		return(HSbar)
		
	})
}
)

