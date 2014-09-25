
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(leaflet)
library(rCharts)
library(rjson)

shinyUI(navbarPage("N-Load",
		   
  # Application title
  #tags$h2("N-Load Model v.1.0"),
  
#   p("Version adapted to Shiny"),
#   hr(),

  # Sidebar with a slider input for number of bins
  tabPanel("Physical Loading Parameters",
#Physical Loading----
  	fluidRow(
  	column(3, 
  		h3("Physical Loading Parameters"),
  		
  		sliderInput("AtmDepRate",
  		     	    label= "Atmospheric Deposition Rate (kg N/ha/yr):", 
  		     	    min = 12.0, max = 17.0, value = 15.1, round= FALSE, step= 0.1), 
  		sliderInput("FertLawns",
  			    "Nitrogen from fertilizer applied to lawns (kg N/ha):",
  			    min= 50, max= 150, value= 104, ticks= FALSE),
  		sliderInput("FertAg", 
  			    "Nitrogen from fertilizer applied to agricultural lands (kg N/ha):",
  			    min= 50, max= 150, value= 136, ticks= FALSE),
  		sliderInput("FertVineyards", 
  			    "Nitrogen from fertilizer applied to vineyards (kg N/ha):",
  			    min= 0, max= 20, value= 8.41, ticks= FALSE),
  		sliderInput("FertRec", 
  			    "Nitrogen from fertilizer applied to golf courses and recreational lands (kg N/ha):",
  			    min= 0, max= 200, value= 115, ticks= FALSE),
  		sliderInput("HumanExcretion",
  			    "Per capita human N excretion rate (kg N/pp/yr):",
  			    min= 0.0, max= 10.0, value= 4.8, round= FALSE, step= 0.1)
  		)
  	)
),
# Retention Parameters ----
tabPanel("Retention Parameters",
	 fluidRow(
	 	column(3,
  	       h3("Retention Parameters"),
  	       
  		sliderInput("NtransNatVeg",
  			    "% Atmospheric Deposition NOT retained in natural vegetation",
  			    min = 0.00, max = 1.00, value = 0.35),
  		sliderInput("NtransTurf",
  			    "% Atmospheric Deposition NOT retained in turf soils:",
  			    min = 0.00, max = 1.00, value = 0.38),
  		sliderInput("NtransAg",
  			    "% Atmospheric Deposition NOT retained in agriculture soils:",
  			    min = 0.00, max = 1.00, value = 0.38),
    	       	sliderInput("NtransWetlands",
  			    "% Atmospheric Deposition NOT retained in wetlands:",
  			    min = 0.00, max = 1.00, value = 0.38),
  		sliderInput("NtransPonds",
  			    "% Atmospheric Deposition NOT retained in ponds:",
  			    min = 0.00, max = 1.00, value = 0.38),
  		sliderInput("NtransImperv",
  			    "% Atmospheric Deposition NOT retained in impervious surfaces:",
  			    min = 0.00, max = 1.00, value = 0.38),
  		sliderInput("DeNit",
  			    "% Not lost as gases -- Denitrification?",  ## used one input for g-h-i
  			    min = 0.00, max = 1.00, value = 0.61)
  		)
	 )
),
# Transport Parameters----
tabPanel("Transport Parameters",
	 fluidRow(
	 	column(3, 
  		h3("Transportation Parameters"),
  		
  		sliderInput("PercentHomes",
  			    "% of homes that use fertilizer",
  			    min = 0.00, max = 1.00, value = 0.49),
  		sliderInput("TransTurf",
  			    "% N fertilizer transported from turf soils",
  			    min = 0.00, max = 1.00, value = 0.49),
  		sliderInput("TransAg",
  			    "% fertilizer N transported from Agriculture",
  			    min = 0.00, max = 1.00, value = 0.49),
  		sliderInput("TransRec",
  			    "% fertilizer N transported from recreational soils",
  			    min = 0.00, max = 1.00, value = 0.49),
  		sliderInput("TransSeptic",
  			    "% fertilizer N transported from septic tanks",
  			    min = 0.00, max = 1.00, value = 0.49),
  		sliderInput("TransLeach",
  			    "% fertilizer N transported from leaching fields",
  			    min = 0.00, max = 1.00, value = 0.49),
  		sliderInput("TransSepticPlume",
  			    "% fertilizer N transported from septic plumes",
  			    min = 0.00, max = 1.00, value = 0.49),
  		sliderInput("TransVandose",
  			    "% fertilizer N transported from vandose zone",
  			    min = 0.00, max = 1.00, value = 0.49),
  		sliderInput("TransAquifer",
  			    "% fertilizer N transported from aquifer",
  			    min = 0.00, max = 1.00, value = 0.61),
  		sliderInput("ThroughAquiferPonds",
  			    "Throughput to the aquifer from ponds",
  			    min = 0.00, max = 1.00, value = 0.44),
  		sliderInput("ThroughAquiferWetlands",
  			    "Throughput to the aquifer from wetlands",
  			    min = 0.00, max = 1.00, value= 0.22)
  		)
	 )
),
# Wastewater Parameters ----
tabPanel("Wastewater Parameters",
	 fluidRow(
	 	tags$h5("Wastewater Parameters- From septic, cesspool, STP's"),
	 	column(4,
	 	       h6("Human Factors"),
	 	       
	 	       numericInput("HumanLoad",
	 	       	     "Human N released per year (kg??)", #Need to confirm units
	 	       	     min = 0.00, max = 10.00, value = 4.80),
	 	       sliderInput("HouseSize",
	 	       	     "Average Household Size (# people)",
	 	       	     min= 0.00, max = 10.00, value = 2.10, round = FALSE, step = 0.05),
	 	       numericInput("NumbHomesSeptic",
	 	       	     "Number of homes with Septic Systems",
	 	       	     min= 0, max = NA, value = NA),
	 	       numericInput("NumbHomesCess",
	 	       	     "Number of homes with Cesspool",
	 	       	     min= 0, max = NA, value = NA),
	 	       
	 	       #k l and m
	 	       h6("Septic System & Cesspool Efficiencies"),
	 	       
	 	       sliderInput("NotLostSpetic",
	 	       	    "% NOT lost in septic tank",
	 	       	    min= 0.00, max = 1.00, value = 0.94),
	 	       sliderInput("NotLostLeach",
	 	       	    "% NOT lost in leaching fields",
	 	       	    min = 0.00, max = 1.00, value = 0.65),
	 	       sliderInput("NotLostPlume",
	 	       	    "% NOT lost in septic plume",
	 	       	    min = 0.00, max = 1.00, value = 0.66),
	 	       sliderInput("NotLostAquifer",
	 	       	    "% NOT lost in aquifer",
	 	       	    min = 0.00, max = 1.00, value = 0.65),
	 	       sliderInput("AvgAnSTPLoad",  # Need to get an idea of range
	 	       	    "Average annual wastewater N concentration (kg N/L)",
	 	       	    min = 0, max = 1000, value = 100),
	 	       sliderInput("TotAnFlow",   # need to reformat- numbers too large
	 	       	    "Total average annual flow (L)",
	 	       	    min = 0, max = 1000000000, value = 500000, step = 1000)
	 	       )
	 	)
	 
	 ),
# Geographic Parameters ----
tabPanel("Geographic Parameters - Results", 
	 fluidRow(
	 	tags$h3("MODEL RESULTS NOT ACCURATE - MODIFICATION NEEDED"),
	 	column(3,
	 	       h3("Geographic Parameters"),
  	       
  		numericInput("LawnArea",
  			    "Average lawn size (hectares):",
  			    min= 0.00, max= 20, value= 0.05),
  		numericInput("NatVegArea",
  			    "area of naturally vegetated land",
  			    value= NA, min = 0.0, max = NA),
  		numericInput("TurfArea",
  			     "Turf Area- needs better description",
  			     value= NA, min = 0.0, max = NA),
  		numericInput("AgArea", 
  			     "Agricultural area- needs better description",
  			     value= NA, min = 0.0, max = NA),
  		numericInput("GolfArea",
  			     "Golf course area (ha)",
  			     value= NA, min = 0.0, max = NA),
  		numericInput("RoofArea",
  			     "Total area of roofs",
  			     value= NA, min = 0.0, max = NA),
  		numericInput("DrivewayArea",
  			     "Total area of driveways",
  			     value= NA, min = 0.0, max = NA),
  		numericInput("ImpervArea",
  			     "Total area of impervious surfaces such as roads/parking lots/runways (ha)",
  			     value = NA, min = 0.0, max = NA),
  		numericInput("PondsArea",
  			     "Total area of freshwater ponds (ha)",
  			     value= NA, min = 0.0, max = NA),
  		numericInput("WetlandsArea",
  			     "Total area of wetlands",
  			     value = NA, min = 0.0, max = NA)
  		),
	 	column(6, 
	 	       h4(textOutput("TotalLoadAtmosphericOut"))
# 	 	       h4(textOutput("TotalFertLoadOut")),
# 	 	       h4(textOutput("TotalWasteWaterLoad")),
# 	 	       h4(textOutput("NLoadTotalOut"))
	 	)
	 )
),
# Output Summary Tab ----
tabPanel("Summary of Loads- Peconics",
	 fluidRow(
	 	column(6,
	 	       h6("Distribution of N loading by sub-watershed"),
	 	       showOutput("HStackBar", "dimple")),
	 	column(6,
	 	       h6("Proportions by source"),
	 	       showOutput("HStackPct", "dimple"))
	 	)
),
# Output Summary Tab #2 ----
tabPanel("Summary of Loads- Peconics- NVD3 plot",
	fluidRow(
		column(12,
		       h6("Distribution of N loads to Peconics- Interactive...."),
		       showOutput("plot", "nvd3"))
		)
))
)

	 