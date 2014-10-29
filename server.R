
library(shiny)
library(reshape2)
library(dplyr)
library(rCharts)
library(ggplot2)
library(RColorBrewer)

#---- scratch space ----


# Run any data manpluations or function creations here
# N-load backbone formula
# NLoads.Melt <- NLoad_outs %>%   # Change to the dataframe output.
# 	select(1:8) %>%	
# 	
# 	melt(id.vars = NLoad_names[1:2])

# ("Site") Not used as an input for N load functions but for plotting and possibly geocoding shapefiles
# ("WetlandsArea"),
# ("PondsArea"),
# ("NatVegArea"),
# ("TurfArea"),
# ("AgArea"),
# ("ImpervArea"),
# ("ActiveAgArea"),
# ("RecArea"),
# ("LawnArea"),
# ("ParkArea")

# Shiny Server ------------------------------------------------------------------
shinyServer( # this will be run each time a user changes something.
	function(input, output) {
	
		# Datafile input: ----
		filedata <- reactive({
			infile <- input$datafile
			if (is.null(infile)) {
				# User has not uploaded a file yet - from http://bl.ocks.org/psychemedia/9690079
				return(NULL)# Output message in html....to ui.R
			}
			read.csv(infile$datapath, header = TRUE)
			
		})
		
# Data mapping -------------------------------------------------


# uiRender commands ----	
#  These functions generate a user input (a select input) 
		# Site
		# input$Site
		output$Site <- renderUI({  # need to create a site input for plots and other analysis.
			df <-filedata()
			if (is.null(df)) return(NULL)
			
			items <- names(df) 
			names(items) <- items
			selectInput("Site", "Site:" ,items, selected = items[1])
			
		})
		# wetlands
		# input$WetlandsArea
		output$WetlandsArea <- renderUI({
			df <-filedata()
			if (is.null(df)) return(NULL)
			
			items=names(df)
			names(items)=items
			selectInput("WetlandsArea", "Wetlands Area (ha):", items, selected = items[3]) 
			# inputID links to the /scratchspace.R input list at top.
			
		})
		# ponds
		# input$PondsArea
		output$PondsArea <- renderUI({
			df <-filedata()
			if (is.null(df)) return(NULL)
			items=names(df)
			names(items)=items
			selectInput("PondsArea", "Ponds Area (ha):", items, selected = items[4]) # inputID links to the /scratchspace.R input list at top.
			
		})
		# natural vegetation
		# input$NatVegArea
		output$NatVegArea <- renderUI({
			df <-filedata()
			if (is.null(df)) return(NULL)
			
			items=names(df)
			names(items)=items
			selectInput("NatVegArea", "Natural Vegetation Area (ha):", items, selected = items[5]) # inputID links to the /scratchspace.R input list at top.
			
		})
		# turfArea
		# input$TurfArea
		output$TurfArea <- renderUI({
			df <-filedata()
			if (is.null(df)) return(NULL)
			
			items=names(df)
			names(items)=items
			selectInput("TurfArea", "Turf Area (ha):", items, selected = items[6]) # inputID links to the /scratchspace.R input list at top.
			
		})
		# agArea
		# input$AgArea
		output$AgArea <- renderUI({
			df <-filedata()
			if (is.null(df)) return(NULL)
			
			items=names(df)
			names(items)=items
			selectInput("AgArea", "Agricultural Area (ha):", items, selected = items[7]) # inputID links to the /scratchspace.R input list at top.
			
		})
		# impervArea
		# input$ImpervArea
		output$ImpervArea <- renderUI({
			df <-filedata()
			if (is.null(df)) return(NULL)
			
			items=names(df)
			names(items)=items
			selectInput("ImpervArea", "Impervious Surface Area (ha):", items, selected = items[8]) # inputID links to the /scratchspace.R input list at top.
			
		})
		# activeAgArea
		# input$ActiveAgArea
		output$ActiveAgArea <- renderUI({
			df <-filedata()
			if (is.null(df)) return(NULL)
			
			items=names(df)
			names(items)=items
			selectInput("ActiveAgArea", "Active Agricultral Area (ha):", items, selected = items[9]) # inputID links to the /scratchspace.R input list at top.
			
		})
		# recArea
		# input$RecArea
		output$RecArea <- renderUI({
			df <-filedata()
			if (is.null(df)) return(NULL)
			
			items=names(df)
			names(items)=items
			selectInput("RecArea", "Recreational Areas (ha):", items, selected = items[10]) # inputID links to the /scratchspace.R input list at top.
			
		})
		# lawnArea
		# input$LawnArea
		output$LawnArea <- renderUI({
			df <-filedata()
			if (is.null(df)) return(NULL)
			
			items=names(df)
			names(items)=items
			selectInput("LawnArea", "Lawn Area (ha):", items, selected = items[11]) # inputID links to the /scratchspace.R input list at top.
			
		})
		# parkArea
		# input$ParkArea
		output$ParkArea <- renderUI({
			df <-filedata()
			if (is.null(df)) return(NULL)
			
			items=names(df)
			names(items)=items
			selectInput("ParkArea", "Park Area (ha):", items, selected = items[12]) # inputID links to the /scratchspace.R input list at top.
			
		})
	
# Data Table Output ------------------------------
		# Output data table
		output$filetable <- renderDataTable({
			df <- filedata()
			if(is.null(df)){
				return("Load data to see summary")
			}
			as.data.frame(df)
		})
		
# Make N Load output dataframe
	output$Nloads <- renderDataTable({
			fd <- filedata()
			if(is.null(fd)){
				return("Load data to see summary")
			}
			NLM()
	})

	NLM <- reactive({
			# Function definitions for NLM 	-----	
			
			# Parameter mapping...
			# From NLM_OysterBay Spreadsheet\
			#
			fd <- filedata() # Bring in the loaded dataframe to map user specified parameters (columns)
			
			TDN <- input$AtmDepRate			# 	[TDN]:  Taken from NADP Data
								# 		Ave Annual Rainfall:
								# 		Wet to Total Deposition Factor:
								# 	% atmos N transported from wetlands
			TAP <- input$ThroughAquiferPonds 	# 	% atmos N transported from freshwater ponds
			ANTNV <- input$AtmNtransNatVeg  	# 	% atmos N transported from Nat'l Veg Soils:
			ANTT <- 12					# 	% atmos N transported from Turf Soils:
			ATAg <- input$AtmNtransAg 		# 	% atmos N transported from Agr. Soils:
			ATImp <- input$AtmNtransImperv ### Not used in NLM-OysterBay
			HSize <- 2		# 	Median Home Size: ##### Make this a input read in with the areas
								# 	No of stories/home:
								# 	House footprint area:
								# 	Average area of roof:
								# 	Average area of driveway:
								# 	% atmos N transported from Impervious Soils (roof/driveway):
			FertL <- input$FertLawns 		# 	Fertilizer N applied to lawns:
			FertAg <- input$FertAg     		# 	Fertilizer N applied to agriculture:
			FertG <- input$FertRec  		# 	Fertilizer N applied to rec/golf courses:
			# NOT USED TOTAL LAWN AREA USED 	# 	Average lawn area:
			FertPerc <- input$PercentHomes  	# 	% of homes that use fertilizer:
			TranT <- input$FertTransTurf    	# 	% of fertilizer N transported from Turf Soils:
			FAgTran <- input$FertTransAg    	# 	% of fertilizer N transported from Agri Soils:
			FRecTran <- input$FertTransRec  	# 	% of fertilizer N transported from Rec. Soils:
			HL <- input$HumanLoad 			# 	Per capita human N excretion rate:
								# 	People per house:
								# 	% N transported from septic tank
								# 	% N transported through leaching field
								# 	% waste transported from septic plumes:
								# 	% watershed N transported from vadose zone:
								# 	% N transported from aquifer:
								# 	# of houses in high density residential areas:
								# 	# of houses in medium-high density residential areas:
								# 	# of houses in medium density residential areas:
								# 	# of houses in medium-low density residential areas:
								# 	# of houses in low density residential areas:
								# 	percent of onsite wastewater systems that are cesspools *** Make this a user datasheet loading input ***
												
			# Septic and Cesspools  
			NHS <- 122 #input$NumbHomesSeptic
								# 			input$NotLostSpetic
								# 			input$NotLostLeach
								# 			input$NotLostPlume
								# 			input$NotLostAquifer
		
			# User loaded areas - Read in on first tab and mapped out with uiOutput-renderOutput functions.
			# fd == dataframe that is loaded in by user
			# input$xxx == the column name of dataframe that is mapped to parameter XX. The [[ ]] function returns a vector 
			TArea <- fd[[input$TurfArea]]
			NVArea <- fd[[input$NatVegArea]]
			RArea <- fd[[input$RecArea]]
			LArea <- fd[[input$LawnArea]]
			PArea <- fd[[input$ParkArea]]
			AArea <- fd[[input$AgArea]]
			AAArea <- fd[[input$ActiveAgArea]]
			IArea <- fd[[input$ImpervArea]]
			WArea <- fd[[input$WetlandsArea]]
			WTWet <- fd[[input$NtransWetlands]]
			PArea <- fd[[input$PondsArea]]
			GArea <- fd[[input$GolfArea]]
				
		NLoadOuts <- NULL # Create an empty object to insert columns below
		
# Atmospheric Loads =============================================================
		#a -good-
		NLoadOuts$AtmNatVeg  <- (TDN * NVArea * ANTNV) %>% round(1)
		
		#b -good-
		NLoadOuts$AtmTurfRec <- (TDN * (TArea + RArea + LArea + PArea ) * TranT) %>% round(1)
		
		#c -good- 
		NLoadOuts$AtmAg <- (TDN * (AArea + AAArea) * ATAg) %>% round(1)
		
		#d -good- FOR NOW...
		NLoadOuts$AtmImperv <- ((TDN * IArea * ATImp)) %>% round(1) #Need help with this formula....# NLM Oysterbay spreadsheet does NOT use the inpu$AtmNtransImperv input in formula
		
		#e -good-
		NLoadOuts$AtmWetlands <- (TDN * WArea * WTWet) %>% round(1)
		
		#f -good-
		NLoadOuts$AtmFreshWater <- (TDN * PArea * TAP) %>% round(1)
		
	
	## Total N load to estuary sourced from Atmospheric Deposition
		NLoadOuts$TotalLoadAtmospheric <- ((AtmNatVeg() + AtmTurfRec() + AtmAg() + AtmImperv() + AtmWetlands() + AtmFreshWater())) %>% round(1)
		
# Fertilizer Application Loads ===================================================		
		
		#g
		NLoadOuts$FertTurf <- (FertL * LArea * FertPerc * TranT) %>% round(1)
		
		#ActiveAg- new to the NLM_Oyster Bay Spreadsheet
		#h 	Is this Active Ag only? Need to find out and/or add actvie ag.
		NLoadOuts$FertActiveAg <- (FertAg * FAgTran * AAArea) %>% round(1)
		
		#i
		NLoadOuts$FertGolf <- (FertG * GArea * FRecTran) %>% round(1)
		
		NLoadOuts$FertParks <- (FertG * FRecTran * PArea) %>% round(1)
		

	## Total Fertilixation Load
		NLoadOuts$TotalFertLoad <- (FertTurf() + FertActiveAg() + FertGolf() + FertParks()) %>% round(1)
		
# Surface Loads- Fertilizer and Deposition ------------------------------------
		#j
		NLoadOuts$SurfaceLoad <- ((TotalLoadAtmospheric() + TotalFertLoad()) * 0.39 * 0.65) %>% round(1)
		
		#k
		NLoadOuts$SepticLoad <- (input$HumanLoad * input$HouseSize * input$NumbHomesSeptic * input$NotLostSpetic * input$NotLostLeach * input$NotLostPlume * input$NotLostAquifer) %>% round(1)
		
		#l
		NLoadOuts$CesspoolLoad <- (input$HumanLoad * input$HouseSize * input$NumbHomesCess * input$NotLostSpetic * input$NotLostPlume * input$NotLostAquifer) %>% round(1)
		
		#m
		NLoadOuts$WasteWaterLoad <- (input$AvgAnSTPLoad * input$TotAnFlow) %>% round(1)
		
		
# Total Nitrogen Loading to Estuary --------------------------------------------
		NLoadOuts$NLoadTotal <- ((SurfaceLoad() + SepticLoad() + CesspoolLoad() + WasteWaterLoad()) %>% round(1))
		

	NLoad.outputs <- data.frame(TotalLoadAtmospheric(), TotalFertLoad())
	NLoad.outputs
	
			
	
})

# Start of NLoad outputs----


# Data output summaries ----		
# Output data summaries- mainly used for diagnostics for model building at this point.
# 		output$filesummary <- renderTable({
# 			# 				df2 <- filedata()
# 			# 				if(is.null(df2)){
# 			# 					return("Load data to see summary")
# 			# 				}
# 			# 				#colMeans(df[2:length(df)])
# 			# 				NVArea <- df2[input$NatVegArea]
# 			# 				NVArea
# 			NLoad()
# 			NLoad.outputs
# 			
# 		})


# # Shiny Plots ----
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
# # 	# Stacked horizontal percentage 
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

 
 
})

