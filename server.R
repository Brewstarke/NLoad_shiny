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
	function(input, output) {
		
# Datafile load
	filedata <- reactive({
			infile <- input$datafile
			if (is.null(infile)) {
				# User has not uploaded a file yet - from http://bl.ocks.org/psychemedia/9690079
				return(NULL)# Output message in html....to ui.R
			}
			csv <- data.frame(read.csv(infile$datapath, header = TRUE))
			csv # run filedata() should return a dataframe
		})
# Column Names for mapping:
	mappingNames <- reactive({
		if(is.null(filedata())){return(NULL)}
		items <- names(filedata())
		items <- as.list(items)
		return(items)
	})
# Table of data loaded in by user
	output$filetable <- renderDataTable({
		fd1 <- filedata()
		if(is.null(fd1)){
			return("Load data to see summary")
		}
		fd1
	})
# Table of NLM outputs	
	output$filetable2 <- renderDataTable({
		
		fd2 <- filedata()
		if(is.null(fd2)){
			return("Your NLM outputs will appear here")
		} # If no table loaded into memory then return message- if loaded run below...
		NLMout()		
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
		if (is.null(filedata())) return(NULL)
		
			selectInput("Site", "Site:", mappingNames(), selected = mappingNames()[1])
		})
		# wetlands
		# input$WetlandsArea
	output$WetlandsAreas <- renderUI({
		if (is.null(filedata())) return(NULL)	
			
			selectInput("WetlandsArea", "Wetlands Area (ha):", mappingNames(), selected = mappingNames()[3]) 
			# inputID links to the /scratchspace.R input list at top.
			
		})
		# ponds
		# input$PondsArea
	output$PondsAreas <- renderUI({
		if (is.null(filedata())) return(NULL)	
			selectInput("PondsArea", "Ponds Area (ha):", mappingNames(), selected = mappingNames()[4]) # inputID links to the /scratchspace.R input list at top.
			
		})
		# natural vegetation
		# input$NatVegArea
	output$NatVegAreas <- renderUI({
		if (is.null(filedata())) return(NULL)
			selectInput("NatVegArea", "Natural Vegetation Area (ha):", mappingNames(), selected = mappingNames()[5]) # inputID links to the /scratchspace.R input list at top.
			
		})
		# turfArea
		# input$TurfArea
	output$TurfAreas <- renderUI({
		if (is.null(filedata())) return(NULL)
# 		items=names(filedata())
# 			names(items)=items
			selectInput("TurfArea", "Turf Area (ha):", mappingNames(), selected = mappingNames()[6]) # inputID links to the /scratchspace.R input list at top.
			
		})
		# agArea
		# input$AgArea
	output$AgAreas <- renderUI({
		if (is.null(filedata())) return(NULL)
# 			items=names(filedata())
# 			names(items)=items
			selectInput("AgArea", "Agricultural Area (ha):", mappingNames(), selected = mappingNames()[7]) # inputID links to the /scratchspace.R input list at top.
			
		})
		# impervArea
		# input$ImpervArea
	output$ImpervAreas <- renderUI({
		if (is.null(filedata())) return(NULL)
# 			items=names(filedata())
# 			names(items)=items
			selectInput("ImpervArea", "Impervious Surface Area (ha):", mappingNames(), selected = mappingNames()[8]) # inputID links to the /scratchspace.R input list at top.
			
		})
		# activeAgArea
		# input$ActiveAgArea
	output$ActiveAgAreas <- renderUI({
		if (is.null(filedata())) return(NULL)
# 			
# 			items=names(filedata())
# 			names(items)=items
			selectInput("ActiveAgArea", "Active Agricultral Area (ha):", mappingNames(), selected = mappingNames()[9]) # inputID links to the /scratchspace.R input list at top.
			
		})
		# recArea
		# input$RecArea
	output$RecAreas <- renderUI({
		if (is.null(filedata())) return(NULL)
# 			
# 			items=names(filedata())
# 			names(items)=items
			selectInput("RecArea", "Recreational Areas (ha):", mappingNames(), selected = mappingNames()[10]) # inputID links to the /scratchspace.R input list at top.
			
		})
		# lawnArea
		# input$LawnArea
	output$LawnAreas <- renderUI({
		if (is.null(filedata())) return(NULL)
# 			items=names(filedata())
# 			names(items)=items
			selectInput("LawnArea", "Lawn Area (ha):", mappingNames(), selected = mappingNames()[11]) # inputID links to the /scratchspace.R input list at top.
			
		})
		# parkArea
		# input$ParkArea
	output$ParkAreas <- renderUI({
		if (is.null(filedata())) return(NULL)
# 			items=names(filedata())
# 			names(items)=items
			selectInput("ParkArea", "Park Area (ha):", mappingNames(), selected = mappingNames()[12]) # inputID links to the /scratchspace.R input list at top.
			
		})
	
# Data Table Outputs ------------------------------
	# Output data table- first page


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
# User loaded spatial paramters (areas) 
#- Read in on first tab and mapped out with uiOutput-renderOutput functions.
# fd == dataframe that is loaded in by user
# input$xxx == the column name of dataframe that is mapped to parameter XX. 

## These inputs are passing NULLs- need to fix this!!		
# target <- reactive({toString(input$value)}) suggested by Rhamath V
NLMout <- reactive({
	if (is.null(filedata())) return(NULL)
		
	fd <- data.frame(filedata())
# if (!is.null(mytext)){
# 			mytext = input$text
# 			vars <- all.vars(parse(text = mytext))
# 			return(vars)
# 		}	
# 		ta <- input$TurfArea
# 		nva <- as.character(input$NatVegArea)
# 		ra <- as.character(input$RecArea)
# 		la <- as.character(input$LawnArea)
# 		pa <- as.character(input$ParkArea)
# 		aa <- as.character(input$AgArea)
# 		aaa <- as.character(input$ActiveAgArea)
# 		ia <- as.character(input$ImpervArea)
# 		wa <- as.character(input$WetlandsArea)
		SiteNames <- fd[[input$Site]]
		TArea <-  fd[[input$TurfArea]]
		NVArea <-  fd[[input$NatVegArea]]
		RArea <-  fd[[input$RecArea]]
		LArea <-  fd[[input$LawnArea]]
		PArea <-  fd[[input$ParkArea]]
		AArea <-  fd[[input$AgArea]]
		AAArea <-  fd[[input$ActiveAgArea]]
		IArea <-  fd[[input$ImpervArea]]
		WArea <-  fd[[input$WetlandsArea]]

# 		TArea <- as.vector(fd$ta)
# 		NVArea <- as.vector(fd$nva)
# 		RArea <- as.vector(fd$ra)
# 		LArea <- as.vector(fd$la)
# 		PArea <- as.vector(fd$pa)
# 		AArea <- as.vector(fd$aa)
# 		AAArea <- as.vector(fd$aaa)
# 		IArea <- as.vector(fd$ia)
# 		WArea <- as.vector(fd$wa)
# Build the NLM modelusing user loaded spatial data (areas from .csv)

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
		HSize <- input$HouseSize
		NHS <- input$NumbHomesSeptic
		WTWet <- 12
		# 			input$NotLostSpetic
		# 			input$NotLostLeach
		# 			input$NotLostPlume
		# 			input$NotLostAquifer
	
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
#		NLM$SepticLoad <- (input$HumanLoad * input$HouseSize * input$NumbHomesSeptic * input$NotLostSpetic * input$NotLostLeach * input$NotLostPlume * input$NotLostAquifer) %>% round(1)
		#l
#		NLM$CesspoolLoad <- (input$HumanLoad * input$HouseSize * input$NumbHomesCess * input$NotLostSpetic * input$NotLostPlume * input$NotLostAquifer) %>% round(1)
		#m
##		NLM$WasteWaterLoad <- (input$AvgAnSTPLoad * input$TotAnFlow) %>% round(1)
# Total Nitrogen Loading to Estuary --------------------------------------------
#		NLM$NLoadTotal <- (NLM$SurfaceLoad + NLM$SepticLoad + NLM$CesspoolLoad + NLM$WasteWaterLoad) 
		NLM$Sites <- (SiteNames)
		outNLM <- data.frame(NLM)	
		outNLM
})


# TEST #
# for interactivity-
	output$NLMtest <- renderDataTable({
		if(is.null(filedata())){
			return("Load data to see ouputs")
		}
		NLMout()
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
