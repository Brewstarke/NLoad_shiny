####
#
#  ui.R Shiny App
#
# NLM model 
#
####

library(shiny)
library(reshape2)
library(rCharts)
library(dplyr)
library(ggplot2)
library(RColorBrewer)


shinyUI(navbarPage("N-Load",
# Data Loading Tab ---- 
	tabPanel("Data Loading", 
	    fluidRow(
	    	h4("MODEL RESULTS NOT ACCURATE - MODIFICATION NEEDED"),
	    	column(2,
	    	       h5("Load in land use and population data as a .csv file here:"),
	    	       fileInput('datafile', 'Choose CSV file',
	    	       	  accept=c('text/csv', 'text/comma-separated-values,text/plain')),
	    	       uiOutput("Sites"),
	    	       uiOutput("WetlandsAreas"),
	    	       uiOutput("PondsAreas"),
	    	       uiOutput("NatVegAreas"),
	    	       uiOutput("TurfAreas"),
	    	       uiOutput("AgAreas"),
	    	       uiOutput("ImpervAreas"),
	    	       uiOutput("ActiveAgAreas"),
	    	       uiOutput("RecAreas"),
	    	       uiOutput("LawnAreas"),
	    	       uiOutput("ParkAreas"),
	    	       uiOutput("ResdGT200ms"),
	    	       uiOutput("ResdLT200ms"),
	    	       uiOutput("persperhomes")
	    	       
	    	), ## Can add 'conditionPanel()' to allow for extra data/parameter mapping options or NLoad options.
	    	column(9,
	    	       h5("User provided paramters used as NLM model inputs"),
	    	       dataTableOutput("filetable"),
	    	       h5("NLM Outputs as kgN/ha"),
	    	       dataTableOutput("filetable2")
	    	       )
		    )
		),
# Wastewater Parameters ----
	tabPanel("Direct Human Inputs",
		 fluidRow(
		 	column(4,
		 	   h5("Direct Application of fertilizers"),
			 	   sliderInput("PercentHomes", # good 49%
			 	       	    "% of homes that use fertilizer",
			 	       	    min = 0.00, max = 1.00, value = 0.49, step = 0.05, ticks = FALSE),
			 	   sliderInput("FertLawns", # good- 122 - was 105
			 	   	    "Nitrogen from fertilizer applied to lawns (kg N/ha):",
			 	   	    min= 0, max= 250, value= 122, ticks= FALSE),
			 	   sliderInput("FertAg", # good- 136 
			 	   	    "Nitrogen from fertilizer applied to agricultural lands (kg N/ha):",
			 	   	    min= 0, max= 250, value= 136, ticks= TRUE),
#	 	 	 	   sliderInput("FertVineyards", 
# 		 	 	   	    "Nitrogen from fertilizer applied to vineyards (kg N/ha):",
# 	 		 	   	    min= 0, max= 20, value= 8.41, ticks= FALSE),
			 	   sliderInput("FertRec", # good 146  -- was 115
			 	   	    "Nitrogen from fertilizer applied to golf courses and recreational lands (kg N/ha):",
			 	   	    min= 0, max= 250, value= 146, ticks= FALSE),
		 	   h5("Septic System & Cesspool Efficiencies"),
				sliderInput("percentCesspools", # good 53%  -- was 32%
					     "Precent of onsite wastewater systems that are cesspools",
					     min = 0.00, max = 1.00, value= .53),
		 	  	sliderInput("HumanLoad", # good 4.8  
		 	   	 	    "Human N released per year (kg N/pp/yr)", 
		 	   	  	   min = 0.0, max = 10.0, value = 4.8, round = FALSE, step = 0.1),
		 	      	sliderInput("NtransFromSpeticTank",  # good- 93% was 94%
		 	       	  	  "% Nitrogen transported from septic tank",
		 	       	  	  min= 0.00, max = 1.00, value = 0.93, round = FALSE, step = 0.01),
		 	       	sliderInput("NTransLeach", # good- 95% was 65%
		 	       	  	  "% Nitrogen transported through leaching fields",
		 	       	  	  min = 0.00, max = 1.00, value = 0.95, round = FALSE, step = 0.01),
		 	       	sliderInput("NtransPlume",  #good 95% was 65%
		 	       	  	  "% waste transported from septic plume",
		 	       	   	 min = 0.00, max = 1.00, value = 0.95, round = FALSE, step = 0.01),
				sliderInput("NtransAquifer",  # good 85%
					    "% Watershed Nitrogen transported from the aquifer",
					    min = 0.00, max = 1.00, value = 0.85, round = FALSE, step = 0.01),
		 	   h5("Sewage Treatment Plant Efficiencies"),
		 	       sliderInput("AvgAnSTPLoad",  # Need to get an idea of range-- Maybe add another control for modifying the 'efficiency' of a STP for running scenarios.
		 	       		    "Average annual wastewater N concentration (kg N/L)",
		 	       	 	   min = 0, max = 1000, value = 100),
		 	       sliderInput("TotAnFlow",   # need to reformat- numbers too large
		 	       		    "Total average annual flow (L)",
		 	       	 	   min = 0, max = 1000000000, value = 500000, step = 1000)
		 	       ),
		 	column(8,
		 	       h4("Insert a plot of fertilizer loadings and septic? Again, facet on subestuary or scenario"),
		 	       dataTableOutput(outputId = "NLMwwfertloads")
		 	       )
		 	)
		  ),
# Less Active Parameters ----
navbarMenu("Additional Model Parameters", 
	   tabPanel("Atmospheric Loading Parameters",
	   	 fluidRow(
	   	 	column(3,
	   	 	       h4("Atmospheric Loading Parameters"),
	   	 	       h6("...insert some commentary on what these parameters mean and do..."),
    	       # 	% atmos N transported from wetlands
	   	 	       sliderInput("AtmNtransWetlands", #good
	   	 	       	    "% Atmospheric Nitrogen transported from wetlands",
	   	 	       	    min = 0.00, max = 1.00, value= 0.22, round = FALSE, step = 0.01),
	       # 	% atmos N transported from freshwater ponds
	   	 	       sliderInput("AtmNtransPonds", # good
	   	 	       	    "% atmos N transported from freshwater ponds:",
	   	 	       	    min = 0.00, max = 1.00, value = 0.44, round = FALSE, step = 0.01),
		# 	% atmos N transported from Nat'l Veg Soils:
	   	 	       sliderInput("AtmNtransNatVeg", #good
	   	 	       	    "% atmos N transported from Nat'l Veg Soils:",
	   	 	       	    min = 0.00, max = 1.00, value = 0.35, round = FALSE, step = 0.01),
		# 	% atmos N transported from Turf Soils:
	   	 	       sliderInput("AtmNtransTurf",  #good
	   	 	       	    "% atmos N transported from Turf Soils::",
	   	 	       	    min = 0.00, max = 1.00, value = 0.38, round = FALSE, step = 0.01),
	   	 # 	% atmos N transported from Agr. Soils:
	   	 	       sliderInput("AtmNtransAg",  #good
	   	 	       	    "% atmos N transported from Agr. Soils:",
	   	 	       	    min = 0.00, max = 1.00, value = 0.38, round = FALSE, step = 0.01),
		# 	% atmos N transported from Impervious Soils (roof/driveway): *********   ### NLM oysterbay spreadsheet does not use this...
	   	 	       sliderInput("AtmNtransImperv",  # good though not used...
	   	 	       	    "% atmos N transported from Impervious Soils (roof/driveway):",
	   	 	       	    min = 0.00, max = 1.00, value = 0.38, round = FALSE, step = 0.01)
#				h5("Throughput to the aquifer"),
# 					   	 	       
# 	   	 	       sliderInput("DeNit",
# 	   	 	       	    "% Not lost as gases -- Denitrification?",  ## used one input for g-h-i
# 	   	 	       	    min = 0.00, max = 1.00, value = 0.61)
	   	 		),
	   	 	column(9,
	   	 	      h4("Should add a graphic with the atmospheric loads for each subwater shed (OR MANIPULATION/SCENARIO OF A WATERSHED"),
	   	 	      dataTableOutput(outputId = "NLMtest")
	   	 	       )
	   		 )
	   	),
# ---> Transport Parameters ----
	   tabPanel("Fertilizer Loading and Transportportation",
	   	 fluidRow(
	   	 	column(3, 
	   	 	       # transportation parameter UI inputs
	   	 	       
		   	h4("Fertilizer Transport"),
		   	h6("Should add some commentary here too, Not sure what these really mean or do..."),
	   	 	       sliderInput("FertTransTurf",  #good-61%  was 49%
	   	 	       	    "% N fertilizer transported from turf soils",
	   	 	       	    min = 0.00, max = 1.00, value = 0.61, round = FALSE, step = 0.01),
	   	 	       sliderInput("FertTransAg", # good- 61% was 49%
	   	 	       	    "% fertilizer N transported from Agriculture",
	   	 	       	    min = 0.00, max = 1.00, value = 0.61, round = FALSE, step = 0.01),
	   	 	       sliderInput("FertTransRec", #good 61% was 49%
	   	 	       	    "% fertilizer N transported from recreational soils",
	   	 	       	    min = 0.00, max = 1.00, value = 0.61, round = FALSE, step = 0.01)
	   	 	   # THE BELOW SEPTIC-FERTILIZER PARAMETERS ARE NOT USED IN NLM-OYSTERBAY SHEET
# 		   		sliderInput("FertTransSeptic",
# 	   	 	       	    "% fertilizer N transported from septic tanks",
# 	   	 	       	    min = 0.00, max = 1.00, value = 0.49, round = FALSE, step = 0.01),
# 	   	 	       sliderInput("FertTransLeach",
# 	   	 	       	    "% fertilizer N transported from leaching fields",
# 	   	 	       	    min = 0.00, max = 1.00, value = 0.49, round = FALSE, step = 0.01),
# 	   	 	       sliderInput("FertTransSepticPlume",
# 	   	 	       	    "% fertilizer N transported from septic plumes",
# 	   	 	       	    min = 0.00, max = 1.00, value = 0.49, round = FALSE, step = 0.01),
# 	   	 	       sliderInput("FertTransVadose",
# 	   	 	       	    "% fertilizer N transported from vadose zone",   ### UNUSED IN NLM OYSTERBAY SPREADSHEET
# 	   	 	       	    min = 0.00, max = 1.00, value = 0.49, round = FALSE, step = 0.01),
# 	   	 	       sliderInput("FertTransAquifer",
# 	   	 	       	    "% fertilizer N transported from aquifer",
# 	   	 	       	    min = 0.00, max = 1.00, value = 0.61, round = FALSE, step = 0.01)
	   	 	
	   	 		),
	   	 	column(9,
	   	 	       h4("Not sure what to put here. Graphic of how this transportation works??")
	   	 	       )
	   	 	)
	   	),
# ---> Physical loading parameters ----
	   tabPanel("Physical Loading Parameters", 
		   fluidRow(
		   	column(3, 
		   	       h4("Physical Loading Parameters"),
		   	       sliderInput("AtmDepRate",  # NEED TO CONFIRM THIS NUMBER- USED THE wet and dry combined initially- 15.1kgN/ha/yr
		   	       	    label= "Atmospheric Deposition Rate (kg N/ha/yr):", 
		   	       	    min = 5, max = 20, value = 15.0892, step= .01), 
# 		   	       sliderInput("HumanExcretion", # NOT USED
# 		   	       	    "Per capita human N excretion rate (kg N/pp/yr):",
# 		   	       	    min= 0.0, max= 10.0, value= 4.8, round= FALSE, step= 0.1),
		   	       sliderInput("NtransVadose", #good - 39%
		   	       	    "% Watershed Nitrogen transported from the vadose zone",
		   	       	    min = 0.00, max = 1.00, value = .39, round= FALSE, ticks= FALSE, step = 0.01)		   	       
	   			),
		   	column(9,
		   	       h4("might not be worth keeping this tab around....")
		   	       )
	   		)
		)
	),
	   
# Output Summary Tab ----
tabPanel("Distribution of Loads",
	 fluidRow(
	 	column(6,
	 	       h4("Distribution of N loading by sub-watershed"),
 	 	       showOutput("HStackBar", "dimple")
	 	       ),
	 	column(6,
	 	       h4("Proportions by source-"),
	 	       h5("plot goes here..."),
 	 	       showOutput("HStackPct", "dimple")
	 	       )
	 	)
	),
# # Output Summary Tab #2 ----
tabPanel("Download Outputs",
	fluidRow(
		column(12,
		       h4("To save results click 'Down Load' button and browse to folder for saving"),
		       downloadButton('downloadOutput', label = "Download NLM Outputs")
		)
		)
	)
#----
)
)




	 