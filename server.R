
library(shiny)
library(reshape2)
library(dplyr)
library(rCharts)
library(ggplot2)
library(RColorBrewer)

# Run any data manpluations or function creations here
# N-load backbone formula
NLoads.Melt <- NLoad_outs %>%   # Change to the dataframe output.
	select(1:8) %>%	
	arrange(septic_NLoad) %>%
	melt(id.vars = NLoad_names[1:2])

# Shiny Server ------------------------------------------------------------------
shinyServer( # this will be run each time a user changes something.
	function(input, output) {
# Atmospheric Loads =============================================================
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
# Fertilizer Application Loads ===================================================		
		
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

	## Total Fertilixation Load
		TotalFertLoad <- function(){
			return(FertTurf() + FertAg() + FertGolf()) %>% round(1)
		}
# Surface Loads- Fertilizer and Deposition ======================================
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
		
# Total Nitrogen Loading to Estuary =============================================
		NLoadTotal <- function(){
			return(SurfaceLoad() + SepticLoad() + CesspoolLoad() + WasteWaterLoad()) %>% round(1)
		}


# Shiny Plots -------------------------------------------------------------------
	
	# Stacked bar plot- absolute values- dimple plots =====
	output$HStackBar <- renderChart2({
		# Stacked horizontal plot Total loads descending
		HSbar <- dPlot(y = "subwatershed_code", x = "value", data= NLoads.Melt, groups= "variable", type = "bar", height = 700, width= 700)
		HSbar$yAxis(type= "addCategoryAxis", orderRule = 'rev(value)')
		HSbar$xAxis(type= "addMeasureAxis")
		HSbar$legend(
			x = 0, 
			y = 0, 
			width = 500, 
			height = 1500,
			horizontalAlign = "center")
		HSbar$defaultColors(brewer.pal(6, "Set1"))
		return(HSbar)
	})

	# Stacked horizontal percentage =========================================
	output$HStackPct <- renderChart2({
		HSbarPct <- dPlot(y = "subwatershed_code", x = "value", data= NLoads.Melt, groups= "variable", type = "bar", height = 700, width= 700)
		HSbarPct$yAxis(type= "addCategoryAxis")
		HSbarPct$xAxis(type= "addPctAxis")
		HSbarPct$legend(
			x = 0, 
			y = 0, 
			width = 700, 
			height = 700,
			horizontalAlign = "right")
		HSbarPct$defaultColors(brewer.pal(6, "Set1"))
		return(HSbarPct)
		
	})
	
	output$plot  <- renderChart2({
	
		plot1 <- nPlot(value ~ subwatershed_code,
			       group = "variable",
			       data = NLoads.Melt,
			       type = "multiBarHorizontalChart")
		plot1$params$height = 600
		plot1$params$dom  <-  "plot"
		return(plot1)
	})

})

