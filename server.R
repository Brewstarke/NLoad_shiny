#### Shiny Server
#
# NLM Model running on shiny
# Runs basic Nitrogen loading models from data read in by user
# Sensitivity analysis is possible by manipulating the many parameters 
#
#
####

library(shiny)
library(reshape2)
library(dplyr)
library(rCharts)
library(ggplot2)
library(RColorBrewer)

# Shiny Server ------------------------------------------------------------------
shinyServer( # this will be run each time a user changes something.
	function(input, output, session) {
		
# Datafile load
	filedata <- reactive({
			infile <- input$datafile
			if (is.null(infile)) {
				# User has not uploaded a file yet - from http://bl.ocks.org/psychemedia/9690079
				return( "Load spatial data to get started...")# Output message in html....to ui.R
			}
			read.csv(infile$datapath, header = TRUE)
		})
		
# Data mapping -------------------------------------------------

# From NLM_OysterBay Spreadsheet\
#   ---Parameters--
# 	[Rainfall nitrate]:
# 		[Rainfall ammonia]:
# 		[Rainfall dissolved organic N]:
# 	[TDN]:
# 		Ave Annual Rainfall:
# 		Wet to Total Deposition Factor:
# 	% atmos N transported from wetlands
# 	% atmos N transported from freshwater ponds
# 	% atmos N transported from Nat'l Veg Soils:
# 	% atmos N transported from Turf Soils:
# 	% atmos N transported from Agr. Soils:
# 	Median Home Size:
# 	No of stories/home:
# 	House footprint area:
# 	Average area of roof:
# 	Average area of driveway:
# 	% atmos N transported from Impervious Soils (roof/driveway):
# 	Fertilizer N applied to lawns:
# 	Fertilizer N applied to agriculture:
# 	Fertilizer N applied to rec/golf courses:
# 	Average lawn area:
# 	% of homes that use fertilizer:
# 	% of fertilizer N transported from Turf Soils:
# 	% of fertilizer N transported from Agri Soils:
# 	% of fertilizer N transported from Rec. Soils:
# 	Per capita human N excretion rate:
# 	People per house:
# 	% N transported from septic tank
# 	%N transported through leaching field
# 	% waste transported from septic plumes:
# 	% watershed N transported from vadose zone:
# 	% N transported from aquifer:
# 	# of houses in high density residential areas:
# 	# of houses in medium-high density residential areas:
# 	# of houses in medium density residential areas:
# 	# of houses in medium-low density residential areas:
# 	# of houses in low density residential areas:
# 	percent of onsite wastewater systems that are cesspools

# uiRender commands ####	
#  These functions generate a user input (a select input) 
		# Site
		# input$Site
	output$Sites <- renderUI({  # need to create a site input for plots and other analysis.
			fd <-filedata()
			if (is.null(fd)) return(NULL)
			
			items <- names(fd) 
			names(items) <- items
			updateSelectInput(session, "Site", "Site:" ,items, selected = items[1])
			
		})
		# wetlands
		# input$WetlandsArea
	output$WetlandsAreas <- renderUI({
			fd <-filedata()
			if (is.null(fd)) return(NULL)
			
			items=names(fd)
			names(items)=items
			updateSelectInput(session, "WetlandsArea", "Wetlands Area (ha):", items, selected = items[3]) 
			# inputID links to the /scratchspace.R input list at top.
			
		})
		# ponds
		# input$PondsArea
	output$PondsAreas <- renderUI({
			fd <-filedata()
			if (is.null(fd)) return(NULL)
			items=names(fd)
			names(items)=items
			updateSelectInput(session, "PondsArea", "Ponds Area (ha):", items, selected = items[3]) # inputID links to the /scratchspace.R input list at top.
			
		})
		# natural vegetation
		# input$NatVegArea
	output$NatVegAreas <- renderUI({
			fd <-filedata()
			if (is.null(fd)) return(NULL)
			
			items=names(fd)
			names(items)=items
			updateSelectInput(session, "NatVegArea", "Natural Vegetation Area (ha):", items, selected = items[4]) # inputID links to the /scratchspace.R input list at top.
			
		})
		# turfArea
		# input$TurfArea
	output$TurfAreas <- renderUI({
			fd <-filedata()
			if (is.null(fd)) return(NULL)
			
			items=names(fd)
			names(items)=items
			updateSelectInput(session, "TurfArea", "Turf Area (ha):", items, selected = items[5]) # inputID links to the /scratchspace.R input list at top.
			
		})
		# agArea
		# input$AgArea
	output$AgAreas <- renderUI({
			fd <-filedata()
			if (is.null(fd)) return(NULL)
			
			items=names(fd)
			names(items)=items
			updateSelectInput(session, "AgArea", "Agricultural Area (ha):", items, selected = items[5]) # inputID links to the /scratchspace.R input list at top.
			
		})
		# impervArea
		# input$ImpervArea
	output$ImpervAreas <- renderUI({
			fd <-filedata()
			if (is.null(fd)) return(NULL)
			
			items=names(fd)
			names(items)=items
			updateSelectInput(session, "ImpervArea", "Impervious Surface Area (ha):", items, selected = items[6]) # inputID links to the /scratchspace.R input list at top.
			
		})
		# activeAgArea
		# input$ActiveAgArea
	output$ActiveAgAreas <- renderUI({
			fd <-filedata()
			if (is.null(fd)) return(NULL)
			
			items=names(fd)
			names(items)=items
			updateSelectInput(session, "ActiveAgArea", "Active Agricultral Area (ha):", items, selected = items[6]) # inputID links to the /scratchspace.R input list at top.
			
		})
		# recArea
		# input$RecArea
	output$RecAreas <- renderUI({
			fd <-filedata()
			if (is.null(fd)) return(NULL)
			
			items=names(fd)
			names(items)=items
			updateSelectInput(session, "RecArea", "Recreational Areas (ha):", items, selected = items[6]) # inputID links to the /scratchspace.R input list at top.
			
		})
		# lawnArea
		# input$LawnArea
	output$LawnAreas <- renderUI({
			fd <-filedata()
			if (is.null(fd)) return(NULL)
			
			items=names(fd)
			names(items)=items
			updateSelectInput(session, "LawnArea", "Lawn Area (ha):", items, selected = items[7]) # inputID links to the /scratchspace.R input list at top.
			
		})
		# parkArea
		# input$ParkArea
	output$ParkAreas <- renderUI({
			fd <-filedata()
			if (is.null(fd)) return(NULL)
			
			items=names(fd)
			names(items)=items
			updateSelectInput(session, "ParkArea", "Park Area (ha):", items, selected = items[8]) # inputID links to the /scratchspace.R input list at top.
			
		})
	
# Data Table Outputs ------------------------------
	# Output data table- first page
	output$filetable <- renderDataTable({
			fd <- data.frame(filedata())
			if(is.null(fd)){
				return("Load data to see summary")
			}
			fd
			})

	output$filetable2 <- renderDataTable({
				
			fd <- data.frame(filedata())
			if(is.null(fd)){
				return("Load data to see summary")
			} # If no table loaded into memory then return message- if loaded run below...
			NLM()
			
			
	})
	target <- reactive({toString(input$value)})
# Function definitions for NLM 	-----	
			# 	NLoad <- reactive({	
			
			# From NLM_OysterBay Spreadsheet\
			#
			# 	[Rainfall nitrate]:
			# 		[Rainfall ammonia]:
			# 		[Rainfall dissolved organic N]:
			# 	[TDN]:
			# 		Ave Annual Rainfall:
			# 		Wet to Total Deposition Factor:
			# 	% atmos N transported from wetlands
			# 	% atmos N transported from freshwater ponds
			# 	% atmos N transported from Nat'l Veg Soils:
			# 	% atmos N transported from Turf Soils:
			# 	% atmos N transported from Agr. Soils:
			# 	Median Home Size:
			# 	No of stories/home:
			# 	House footprint area:
			# 	Average area of roof:
			# 	Average area of driveway:
			# 	% atmos N transported from Impervious Soils (roof/driveway):
			# 	Fertilizer N applied to lawns:
			# 	Fertilizer N applied to agriculture:
			# 	Fertilizer N applied to rec/golf courses:
			# 	Average lawn area:
			# 	% of homes that use fertilizer:
			# 	% of fertilizer N transported from Turf Soils:
			# 	% of fertilizer N transported from Agri Soils:
			# 	% of fertilizer N transported from Rec. Soils:
			# 	Per capita human N excretion rate:
			# 	People per house:
			# 	% N transported from septic tank
			# 	%N transported through leaching field
			# 	% waste transported from septic plumes:
			# 	% watershed N transported from vadose zone:
			# 	% N transported from aquifer:
			# 	# of houses in high density residential areas:
			# 	# of houses in medium-high density residential areas:
			# 	# of houses in medium density residential areas:
			# 	# of houses in medium-low density residential areas:
			# 	# of houses in low density residential areas:
			# 	percent of onsite wastewater systems that are cesspools *** Make this a user datasheet loading input ***
			
		
# ----
	NLM <- reactive({
		fd <- data.frame(filedata())
		# Parameter mapping...
		# UI controlled parameters
		TDN <- input$AtmDepRate
		ANTNV <- input$AtmNtransNatVeg
		ATAg <- input$AtmNtransAg
		ATImp <- input$AtmNtransImperv
		TAP <- input$ThroughAquiferPonds
		FAgTran <- input$FertTransAg
		FRecTran <- input$FertTransRec
		TranT <- input$FertTransTurf
		# Fertilizer Loads
		FertL <- input$FertLawns
		FertPerc <- input$PercentHomes
		DNit <- input$DeNit
		FertG <- input$FertRec
		FertAg <- input$FertAg
		# Septic and Cesspools  
		HL <- input$HumanLoad 
		HSize <- 12   # input$HouseSize -- changed during debug
		NHS <- 12     # input$NumbHomesSeptic -- changed during debug
		WTWet <- 12
		# 			input$NotLostSpetic
		# 			input$NotLostLeach
		# 			input$NotLostPlume
		# 			input$NotLostAquifer
		# User loaded spatial paramters (areas) 
		#- Read in on first tab and mapped out with uiOutput-renderOutput functions.
		# fd == dataframe that is loaded in by user
		# input$xxx == the column name of dataframe that is mapped to parameter XX. 
		# The ____$____$ function returns a vector (column) of values from the user loaded and selected dataframe
## These inputs are passing NULLs- need to fix this!!		
	# target <- reactive({toString(input$value)}) suggested by Rhamath V
		ta <- reactive({toString(input$TurfArea)})
		nva <- reactive({toString(input$NatVegArea)})
		ra <- reactive({toString(input$RecArea)})
		la <- reactive({toString(input$LawnArea)})
		pa <- reactive({toString(input$ParkArea)})
		aa <- reactive({toString(input$AgArea)})
		aaa <- reactive({toString(input$ActiveAgArea)})
		ia <- reactive({toString(input$ImpervArea)})
		wa <- reactive({toString(input$WetlandsArea)})

		TArea <- fd$ta()
		NVArea <- fd$nva()
		RArea <- fd$ra()
		LArea <- fd$la()
		PArea <- fd$pa()
		AArea <- fd$aa()
		AAArea <- fd$aaa()
		IArea <- fd$ia()
		WArea <- fd$wa()
		# Create blank object to store output
		NLM <- NULL
			  
# Atmospheric Loads =============================================================
		#a -good-
		AtmNatVeg  <- (TDN * NVArea * ANTNV) %>% round(1)
		#b -good-
		AtmTurfRec <- (TDN * (TArea + RArea + LArea + PArea ) * TranT) %>% round(1)
		#c -good- 
		AtmAg <- (TDN * (AArea + AAArea) * ATAg) %>% round(1)
		#d -good- FOR NOW...
		AtmImperv <- ((TDN * IArea * ATImp) ) %>% round(1) #Need help with this formula....# NLM Oysterbay spreadsheet does NOT use the inpu$AtmNtransImperv input in formula
		#e -good-
		AtmWetlands <- (TDN * WArea * WTWet) %>% round(1)
		#f -good-
		AtmFreshWater <- (TDN * PArea * TAP) %>% round(1)
## Total N load to estuary sourced from Atmospheric Deposition
		NLM$TotalLoadAtmospheric <- (AtmNatVeg + AtmTurfRec + AtmAg + AtmImperv + AtmWetlands + AtmFreshWater) %>% round(1)
		
# Fertilizer Application Loads ===================================================		
		#g
		FertTurf <- (FertL * LArea * FertPerc * TranT) %>% round(1)
		#ActiveAg- new to the NLM_Oyster Bay Spreadsheet
		#h 	Is this Active Ag only? Need to find out and/or add actvie ag.
		FertActiveAg <- (FertAg * FAgTran * AAArea) %>% round(1)
		#i
		FertGolf <- (FertG * RArea * FRecTran) %>% round(1)
		FertParks <- (FertG * FRecTran * PArea) %>% round(1)
# Total Fertilixation Load
		NLM$TotalFertLoad <- (FertTurf + FertActiveAg + FertGolf + FertParks) %>% round(1)
# Surface Loads- Fertilizer and Deposition ------------------------------------
		#j
		NLM$SurfaceLoad <- ((NLM$TotalLoadAtmospheric + NLM$TotalFertLoad) * 0.39 * 0.65) %>% round(1)
		#k
		NLM$SepticLoad <- (input$HumanLoad * input$HouseSize * input$NumbHomesSeptic * input$NotLostSpetic * input$NotLostLeach * input$NotLostPlume * input$NotLostAquifer) %>% round(1)
		#l
		NLM$CesspoolLoad <- (input$HumanLoad * input$HouseSize * input$NumbHomesCess * input$NotLostSpetic * input$NotLostPlume * input$NotLostAquifer) %>% round(1)
		#m
##		NLM$WasteWaterLoad <- (input$AvgAnSTPLoad * input$TotAnFlow) %>% round(1)
# Total Nitrogen Loading to Estuary --------------------------------------------
		NLM$NLoadTotal <- (NLM$SurfaceLoad + NLM$SepticLoad + NLM$CesspoolLoad + NLM$WasteWaterLoad) 

		outNLM <- data.frame(NLM)	
		outNLM
})

# Start of NLoad outputs----


# Data output summaries ----		
# Output data summaries- mainly used for diagnostics for model building at this point.
# 		output$filesummary <- renderTable({
# 			# 				fd2 <- filedata()
# 			# 				if(is.null(fd2)){
# 			# 					return("Load data to see summary")
# 			# 				}
# 			# 				#colMeans(fd[2:length(fd)])
# 			# 				NVArea <- fd2[input$NatVegArea]
# 			# 				NVArea
# 			NLoad()
# 			NLoad.outputs
# 			
# 		})


# Shiny Plots ----
# 	# Stacked bar plot- absolute values- dimple plots 
# 	output$HStackBar <- renderChart2({
# 		# Stacked horizontal plot Total loads descending
# 		HSbar <- dPlot(y = "subwatershed_code", x = "value", data= NLoads.Melt, groups= "variable", type = "bar", height = 700, width= 700)
# 		HSbar$yAxis(type= "addCategoryAxis", orderRule = 'rev(value)')
# 		HSbar$xAxis(type= "addMeasureAxis")
# 		HSbar$legend(
# 			x = 0, 
# 			y = 0, 
# 			width = 500, 
# 			height = 1500,
# 			horizontalAlign = "center")
# 		HSbar$defaultColors(brewer.pal(6, "Set1"))
# 		return(HSbar)
# 	})
# 
# 	# Stacked horizontal percentage 
# 	output$HStackPct <- renderChart2({
# 		HSbarPct <- dPlot(y = "subwatershed_code", x = "value", data= NLoads.Melt, groups= "variable", type = "bar", height = 700, width= 700)
# 		HSbarPct$yAxis(type= "addCategoryAxis")
# 		HSbarPct$xAxis(type= "addPctAxis")
# 		HSbarPct$legend(
# 			x = 0, 
# 			y = 0, 
# 			width = 700, 
# 			height = 700,
# 			horizontalAlign = "right")
# 		HSbarPct$defaultColors(brewer.pal(6, "Set1"))
# 		return(HSbarPct)
# 		
# 	})
# 	
# 	output$plot  <- renderChart2({
# 	
# 		plot1 <- nPlot(value ~ subwatershed_code,
# 			       group = "variable",
# 			       data = NLoads.Melt,
# 			       type = "multiBarHorizontalChart")
# 		plot1$params$height = 600
# 		plot1$params$dom  <-  "plot"
# 		return(plot1)
# 	})


# ----

}
) 
