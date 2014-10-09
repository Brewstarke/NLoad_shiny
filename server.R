
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
# Map renderUI inputs to variable names in df
# 		isolate({
# 			df=filedata()
# 			site= input$Site
# 			wetlands= df[[input$WetlandsArea]]
# 			
# 			#create the dataframe of outputs using the mapped variable names.
# 			NLoad_outs= data.frame(place=unique(c(as.vector(dummy[[fr]]),
# 							     as.vector(dummy[[to]]))),stringsAsFactors=F)      
# 			cbind(locs, t(sapply(locs$place,geocode, USE.NAMES=F))) 
# 		})
		# output$____ identifies the uiOutput created in server.R and laid out in ui.R
		# these uioutputs create the list of the

# uiRender commands ----		
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
			selectInput("PondsArea", "Ponds Area (ha):", items, selected = items[3]) # inputID links to the /scratchspace.R input list at top.
			
		})
		# natural vegetation
		# input$NatVegArea
		output$NatVegArea <- renderUI({
			df <-filedata()
			if (is.null(df)) return(NULL)
			
			items=names(df)
			names(items)=items
			selectInput("NatVegArea", "Natural Vegetation Area (ha):", items, selected = items[4]) # inputID links to the /scratchspace.R input list at top.
			
		})
		# turfArea
		# input$TurfArea
		output$TurfArea <- renderUI({
			df <-filedata()
			if (is.null(df)) return(NULL)
			
			items=names(df)
			names(items)=items
			selectInput("TurfArea", "Turf Area (ha):", items, selected = items[5]) # inputID links to the /scratchspace.R input list at top.
			
		})
		# agArea
		# input$AgArea
		output$AgArea <- renderUI({
			df <-filedata()
			if (is.null(df)) return(NULL)
			
			items=names(df)
			names(items)=items
			selectInput("AgArea", "Agricultural Area (ha):", items, selected = items[5]) # inputID links to the /scratchspace.R input list at top.
			
		})
		# impervArea
		# input$ImpervArea
		output$ImpervArea <- renderUI({
			df <-filedata()
			if (is.null(df)) return(NULL)
			
			items=names(df)
			names(items)=items
			selectInput("ImpervArea", "Impervious Surface Area (ha):", items, selected = items[6]) # inputID links to the /scratchspace.R input list at top.
			
		})
		# activeAgArea
		# input$ActiveAgArea
		output$ActiveAgArea <- renderUI({
			df <-filedata()
			if (is.null(df)) return(NULL)
			
			items=names(df)
			names(items)=items
			selectInput("ActiveAgArea", "Active Agricultral Area (ha):", items, selected = items[6]) # inputID links to the /scratchspace.R input list at top.
			
		})
		# recArea
		# input$RecArea
		output$RecArea <- renderUI({
			df <-filedata()
			if (is.null(df)) return(NULL)
			
			items=names(df)
			names(items)=items
			selectInput("RecArea", "Recreational Areas (ha):", items, selected = items[6]) # inputID links to the /scratchspace.R input list at top.
			
		})
		# lawnArea
		# input$LawnArea
		output$LawnArea <- renderUI({
			df <-filedata()
			if (is.null(df)) return(NULL)
			
			items=names(df)
			names(items)=items
			selectInput("LawnArea", "Lawn Area (ha):", items, selected = items[6]) # inputID links to the /scratchspace.R input list at top.
			
		})
		# parkArea
		# input$ParkArea
		output$ParkArea <- renderUI({
			df <-filedata()
			if (is.null(df)) return(NULL)
			
			items=names(df)
			names(items)=items
			selectInput("ParkArea", "Park Area (ha):", items, selected = items[6]) # inputID links to the /scratchspace.R input list at top.
			
		})
	

		# Output data table
		output$filetable <- renderTable({
			df <- filedata()
			if(is.null(df)){
				return("Load data to see summary")
			}
			df
		})
# ----		
		# Output data summaries- mainly used for diagnostics for model building.
		output$filesummary <- renderPrint({
				df2 <- filedata()
				if(is.null(df2)){
					return("Load data to see summary")
				}
				#colMeans(df[2:length(df)])
				inAtmNatVeg <- df2[input$NatVegArea]
				inAtmNatVeg
		 	})

	


		
	reactive({}	
# Atmospheric Loads =============================================================
		#a -good-
		AtmNatVeg <- function(){
			return(input$AtmDepRate * inAtmNatVeg * input$AtmNtransNatVeg) %>% round(1)
		}
		#b -good-
		AtmTurfRec <- function(){
			return(input$AtmDepRate * (input$TurfArea + input$RecArea + input$LawnArea + input$ParkArea) * input$TransTurf) %>% round(1)
		}
		#c -good- 
		AtmAg <- function(){  
			return(input$AtmDepRate * (input$AgArea + input$ActiveAgArea) * input$AtmNtransAg) %>% round(1)
		}
		#d -good- FOR NOW...
		AtmImperv <- function(){ # NLM Oysterbay spreadsheet does NOT use the inpu$AtmNtransImperv input in formula
			return((input$AtmDepRate * input$ImpervArea * input$AtmNtransImperv) ) %>% round(1) #Need help with this formula....
		}
		#e -good-
		AtmWetlands <- function(){
			return(input$AtmDepRate * input$WetlandsArea * input$NtransWetlands) %>% round(1)
		}
		#f -good-
		AtmFreshWater <- function(){
			return(input$AtmDepRate * input$PondsArea * input$ThroughAquiferPonds) %>% round(1)
		}
	
	## Total N load to estuary sourced from Atmospheric Deposition
		TotalLoadAtmospheric <- function(){
			return((AtmNatVeg() + AtmTurfRec() + AtmAg() + AtmImperv() + AtmWetlands() + AtmFreshWater())  ) %>% round(1)
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
# Surface Loads- Fertilizer and Deposition ------------------------------------
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
		
# Total Nitrogen Loading to Estuary --------------------------------------------
		NLoadTotal <- function(){
			return((SurfaceLoad() + SepticLoad() + CesspoolLoad() + WasteWaterLoad()) %>% round(1))
		}
})

# Start of NLoad outputs----
		output$NloadDF <- renderTable({
			# FertGolf and # FertAg
			return(AtmFreshWater())
		})


# Shiny Plots ----
	# Stacked bar plot- absolute values- dimple plots 
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

	# Stacked horizontal percentage 
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

