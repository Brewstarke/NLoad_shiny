
library(shiny)
library(reshape2)
library(dplyr)
library(rCharts)
library(ggplot2)
library(RColorBrewer)

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
# ("ActiveagArea"),
# ("RecArea"),
# ("LawnArea"),
# ("ParkArea")

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
		#d - CHECK FORMALA
		AtmImperv <- function(){
			return((input$AtmDepRate * (input$ImpervArea) * input$NtransTurf) * (input$AtmDepRate * input$ImpervArea)) %>% round(1) #Need help with this formula
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
			return(SurfaceLoad() + SepticLoad() + CesspoolLoad() + WasteWaterLoad()) %>% round(1)
		}


# Render Text Outputs ----
output$test1 <- renderText({ 
	input$HumanLoad
})

output$test2 <- renderPrint({
	input$AgArea
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

# Datafile input: ----
filedata <- reactive({
	infile <- input$datafile
	if (is.null(infile)) {
		# User has not uploaded a file yet - from http://bl.ocks.org/psychemedia/9690079
		return(NULL)# Output message in html....to ui.R
	}
	read.csv(infile$datapath)
})
# Data mapping -------------------------------------------------
# output$____ identifies the uiOutput created in server.R and laid out in ui.R
# these uioutputs create the list of the

# sites
output$Site <- renderUI({  # need to create a site input for plots and other analysis.
	df <-filedata()
	if (is.null(df)) return(NULL)
	
	items <- names(df) 
	names(items) <- items
	selectInput("Site", "Site:" ,items, selected = NULL)
	
})
# wetlands
output$WetlandsArea <- renderUI({
	df <-filedata()
	if (is.null(df)) return(NULL)
	
	items=names(df)
	names(items)=items
	selectInput("WetlandsArea", "Wetlands Area (ha):", items, selected = NULL) 
	# inputID links to the /scratchspace.R input list at top.
	
})
# ponds
output$PondsArea <- renderUI({
	df <-filedata()
	if (is.null(df)) return(NULL)
	items=names(df)
	names(items)=items
	selectInput("PondsArea", "Ponds Area (ha):", items, selected = NULL) # inputID links to the /scratchspace.R input list at top.
	
})
# natural vegetation
output$NatVegArea <- renderUI({
	df <-filedata()
	if (is.null(df)) return(NULL)
	
	items=names(df)
	names(items)=items
	selectInput("NatVegArea", "Natural Vegetation Area (ha):", items, selected = NULL) # inputID links to the /scratchspace.R input list at top.
	
})
# turfArea
output$TurfArea <- renderUI({
	df <-filedata()
	if (is.null(df)) return(NULL)
	
	items=names(df)
	names(items)=items
	selectInput("TurfArea", "Turf Area (ha):", items, selected = NULL) # inputID links to the /scratchspace.R input list at top.
	
})
# agArea
output$AgArea <- renderUI({
	df <-filedata()
	if (is.null(df)) return(NULL)
	
	items=names(df)
	names(items)=items
	selectInput("AgArea", "Agricultural Area (ha):", items, selected = NULL) # inputID links to the /scratchspace.R input list at top.
	
})
# impervArea
output$ImpervArea <- renderUI({
	df <-filedata()
	if (is.null(df)) return(NULL)
	
	items=names(df)
	names(items)=items
	selectInput("ImpervArea", "Impervious Surface Area (ha):", items, selected = NULL) # inputID links to the /scratchspace.R input list at top.
	
})
# activeagArea
output$ActiveagArea <- renderUI({
	df <-filedata()
	if (is.null(df)) return(NULL)
	
	items=names(df)
	names(items)=items
	selectInput("ActiveagArea", "Active Agricultral Area (ha):", items, selected = NULL) # inputID links to the /scratchspace.R input list at top.
	
})
# recArea
output$RecArea <- renderUI({
	df <-filedata()
	if (is.null(df)) return(NULL)
	
	items=names(df)
	names(items)=items
	selectInput("RecArea", "Recreational Areas (ha):", items, selected = NULL) # inputID links to the /scratchspace.R input list at top.
	
})
# lawnArea
output$LawnArea <- renderUI({
	df <-filedata()
	if (is.null(df)) return(NULL)
	
	items=names(df)
	names(items)=items
	selectInput("LawnArea", "Lawn Area (ha):", items, selected = NULL) # inputID links to the /scratchspace.R input list at top.
	
})
# parkArea
output$ParkArea <- renderUI({
	df <-filedata()
	if (is.null(df)) return(NULL)
	
	items=names(df)
	names(items)=items
	selectInput("ParkArea", "Park Area (ha):", items, selected = NULL) # inputID links to the /scratchspace.R input list at top.
	
})
output$filetable <- renderTable({
	filedata()
})

})

