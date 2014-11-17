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
library(rCharts)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(rjson)


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
			csv # run filedata() should return a dataframe of input areas that feed into NLMout()
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
			selectInput("TurfArea", "Turf Area (ha):", mappingNames(), selected = mappingNames()[6]) # inputID links to the /scratchspace.R input list at top.
		})
	# agArea
	# input$AgArea
	output$AgAreas <- renderUI({
		if (is.null(filedata())) return(NULL)
			selectInput("AgArea", "Agricultural Area (ha):", mappingNames(), selected = mappingNames()[7]) # inputID links to the /scratchspace.R input list at top.
		})
	# impervArea
	# input$ImpervArea
	output$ImpervAreas <- renderUI({
		if (is.null(filedata())) return(NULL)
			selectInput("ImpervArea", "Impervious Surface Area (ha):", mappingNames(), selected = mappingNames()[8]) # inputID links to the /scratchspace.R input list at top.
		})
	# activeAgArea
	# input$ActiveAgArea
	output$ActiveAgAreas <- renderUI({
		if (is.null(filedata())) return(NULL)
			selectInput("ActiveAgArea", "Active Agricultral Area (ha):", mappingNames(), selected = mappingNames()[9]) # inputID links to the /scratchspace.R input list at top.
		})
	# recArea
	# input$RecArea
	output$RecAreas <- renderUI({
		if (is.null(filedata())) return(NULL)
 			selectInput("RecArea", "Recreational Areas (ha):", mappingNames(), selected = mappingNames()[10]) # inputID links to the /scratchspace.R input list at top.
		})
	# lawnArea
	# input$LawnArea
	output$LawnAreas <- renderUI({
		if (is.null(filedata())) return(NULL)
			selectInput("LawnArea", "Lawn Area (ha):", mappingNames(), selected = mappingNames()[11]) # inputID links to the /scratchspace.R input list at top.
		})
	# parkArea
	# input$ParkArea
	output$ParkAreas <- renderUI({
		if (is.null(filedata())) return(NULL)
			selectInput("ParkArea", "Park Area (ha):", mappingNames(), selected = mappingNames()[12]) # inputID links to the /scratchspace.R input list at top.
		})
	# uiOutput("ResdGT200m"),
	output$ResdGT200ms <- renderUI({
		if (is.null(filedata())) return(NULL)
			selectInput("ResdGT200m", "No. of Residences located greater than 200m from the water:", mappingNames(), selected = mappingNames()[13]) # inputID links to the /scratchspace.R input list at top.
	})
	
	# uiOutput("ResdLT200ms"),
	output$ResdLT200ms <- renderUI({
		if (is.null(filedata())) return(NULL)
		selectInput("ResdLT200m", "No. of Residences located less than 200m from the water:", mappingNames(), selected = mappingNames()[14]) # inputID links to the /scratchspace.R input list at top.
	})

	# uiOutput("persperhomes")
	output$persperhomes <- renderUI({
		if (is.null(filedata())) return(NULL)
		selectInput("perperhome", "Average number of people per home:", mappingNames(), selected = mappingNames()[15]) # inputID links to the /scratchspace.R input list at top.
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
# User loaded spatial paramters (areas) and population paramters/estimates
#- Read in on first tab and mapped out with uiOutput-renderOutput functions.
# fd == dataframe that is loaded in by user
# input$xxx == the column name of dataframe that is mapped to parameter XX. 
	NLMout <- reactive({
		if (is.null(filedata())) return(NULL)
			
		fd <- data.frame(filedata())
			# assigns vector from dataframe (column) to parameter which will coerce forumlas to vector outputs and dataframes
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
			ResGT <- fd[[input$ResdGT200m]]
			ResLT <- fd[[input$ResdLT200m]]
			Persons <- fd[[input$perperhome]]
		# Build the NLM modelusing user loaded spatial data (areas from .csv)
		# Parameter mapping...
		# UI controlled parameters
	## Atmospheric Loading Parameters ##-- A22:A35 in spreadsheet- a few unused parameters dropped
		TDN <- input$AtmDepRate
		WTWet <- input$AtmNtransWetlands
		TAP <- input$AtmNtransPonds
		ANTNV <- input$AtmNtransNatVeg
		TAT <- input$AtmNtransTurf
		ATAg <- input$AtmNtransAg
		ATImp <- input$AtmNtransImperv #### NOT USED IN NLM OYSTERBAY_COLDSPRING HARBOR ####
			
	## Fertilizer Loading Parameters ##-- A36:A43- Dropped average lawn area paramter- using Lawn Area (total land coverage area) from user loaded data
		FertL <- input$FertLawns
		FertAg <- input$FertAg
		FertG <- input$FertRec
		FertPerc <- input$PercentHomes
		TranT <- input$FertTransTurf
		FAgTran <- input$FertTransAg
		FRecTran <- input$FertTransRec
		
	# Septic and Cesspools  Loading Parameters ## -- A44:A50
		HL <- input$HumanLoad 
		NTS <- input$NtransFromSpeticTank
		NTL <- input$NTransLeach
		NTP <- input$NtransPlume
		
		NTV <- input$NtransVadose
			DNit <- input$DeNit  # NOT SURE THIS IS USED...
			
		NTA <- input$NtransAquifer
		PercCess <- input$percentCesspools
	# Create blank object to store output
			NLM <- NULL
			NLM$Sites <- (SiteNames)	  
# Atmospheric Loads =============================================================
			AtmWetlands <- (TDN * WArea * WTWet) %>% round()
			AtmFreshWater <- (TDN * PArea * TAP) %>% round()
			AtmNatVeg  <- (TDN * NVArea * ANTNV) %>% round()
			AtmTurfRec <- (TDN * (TArea + RArea + LArea + PArea ) * TAT) %>% round()
			AtmAg <- (TDN * (AArea + AAArea) * ATAg) %>% round()
			AtmImperv <- ((TDN * IArea * ATImp) ) %>% round() #Need help with this formula....# NLM Oysterbay spreadsheet does NOT use the inpu$AtmNtransImperv input in formula
				
	## Total N load to estuary sourced from Atmospheric Deposition
			NLM$TotalLoadAtmospheric <- (AtmWetlands + AtmFreshWater + AtmNatVeg + AtmTurfRec + AtmAg + AtmImperv) * NTV * NTA %>% round()
			
# Fertilizer Application Loads ===================================================		
			#g
			FertTurf <- (FertL * LArea * FertPerc * TranT) %>% round()
			#ActiveAg- new to the NLM_Oyster Bay Spreadsheet
			#h 	Is this Active Ag only? Need to find out and/or add actvie ag.
			FertActiveAg <- (FertAg * FAgTran * AAArea) %>% round()
			#i
			FertGolf <- (FertG * RArea * FRecTran) %>% round()
			FertParks <- (FertG * FRecTran * PArea) %>% round()
# Total Fertilixation Load
			NLM$TotalFertLoad <- (FertTurf + FertActiveAg + FertGolf + FertParks) * NTV * NTA %>% round()
# Surface Loads- Fertilizer and Deposition ------------------------------------
			#j
			NLM$SurfaceLoad <- ((NLM$TotalLoadAtmospheric + NLM$TotalFertLoad) * 0.39 * 0.65) %>% round()
			#k
			NLM$SepticLoad <- ((1-PercCess) * HL * (ResGT * Persons) * NTS * NTL * NTP * NTA) + ((1-PercCess) * HL * (ResLT * Persons) * NTS * NTL * NTP * NTA) %>% round()
			NLM$CesspoolLoad <- (PercCess * HL* (ResGT * Persons) * NTS * NTP * NTA) + (PercCess * HL* (ResLT * Persons) * NTS * NTP * NTA) %>% round()
# Total Nitrogen Loading to Estuary --------------------------------------------
#			NLM$NLoadTotal <- (NLM$SurfaceLoad + NLM$SepticLoad + NLM$CesspoolLoad + NLM$WasteWaterLoad) 
		
			outNLM <- data.frame(NLM)	
			outNLM
	})


# TEST #
# for interactivity-
	output$NLMtest <- renderDataTable({
		if(is.null(filedata())){
			return("Load data to see ouputs")
		}
		NLMAtm <- NLMout()
		NLMAtm[,1:2]
	})
# Fertilizer and wastewater load table for UI paramter page
	output$NLMwwfertloads <- renderDataTable({
		if(is.null(filedata())){
			return("Load data to see ouputs")
		}
		NLMwwfert <- NLMout()
		NLMwwfert[,c(1,3,5,6)]
	})

# Start of NLoad outputs----

# Data output summaries ----		

# Shiny Plots ----
	NLM.plot.data <- reactive({
		NLoad_names <- names(NLMout()) # create vectoir list of names for melt/cast
		NLoads.Melt <- NLMout() %>%
			# Can add arrange command to 'sort' the 
			melt(id.vars = NLoad_names[1])
		NLoads.Melt
	})
	
		
		
	# Stacked bar plot- absolute values- dimple plots 
	output$HStackBar <- renderChart2({
		# Stacked horizontal plot Total loads descending
		HSbar <- dPlot(y = names(NLM.plot.data())[1], x = "value", data= NLM.plot.data(), groups= "variable", type = "bar", height = 700, width= 700)
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

	# Stacked horizontal percentage 
	output$HStackPct <- renderChart2({
		HSbarPct <- dPlot(y = names(NLM.plot.data())[1], x = "value", data= NLM.plot.data(), groups= "variable", type = "bar", height = 700, width= 700)
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
