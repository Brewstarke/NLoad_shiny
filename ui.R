
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(leaflet)

shinyUI(navbarPage("N-Load",

  # Application title
  #tags$h2("N-Load Model v.1.0"),
  
#   p("Version adapted to Shiny"),
#   hr(),

  # Sidebar with a slider input for number of bins
  tabPanel("Physical Loading Parameters",
#----
  	fluidRow(
  	column(3, 
  		h3("Physical Loading Parameters"),
  		
  		sliderInput("AtmDepRate",
  		     	    label= "Atmospheric Deposition Rate (kg N/ha/yr):", 
  		     	    min = 12.0, max = 17.0, value = 15.1, round= FALSE, step= 0.01), 
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
  			    min= 0, max= 10, value= 4.8, round= FALSE, step= NULL)
  		)
  	)
),
tabPanel("Retention Parameters",
	 fluidRow(
	 	column(3,
  	       h3("Retention Parameters"),
  	       
  		sliderInput("NtransNatVeg",
  			    "% Atmospheric Deposition NOT retained in natural vegetation",
  			    min = 0, max = 100, value = 35),
  		sliderInput("NtransTurf",
  			    "% Atmospheric Deposition NOT retained in turf soils:",
  			    min = 0, max = 100, value = 38),
  		sliderInput("NtransAg",
  			    "% Atmospheric Deposition NOT retained in agriculture soils:",
  			    min = 0, max = 100, value = 38),
    	       	sliderInput("NtransWetlands",
  			    "% Atmospheric Deposition NOT retained in wetlands:",
  			    min = 0, max = 100, value = 38),
  		sliderInput("NtransPonds",
  			    "% Atmospheric Deposition NOT retained in ponds:",
  			    min = 0, max = 100, value = 38),
  		sliderInput("NtransImperv",
  			    "% Atmospheric Deposition NOT retained in impervious surfaces:",
  			    min = 0, max = 100, value = 38),
  		sliderInput("DeNit",
  			    "% Not lost as gases -- Denitrification?",  ## used one input for g-h-i
  			    min = 0, max = 100, value = 61)
  		)
	 )
),
# ----
tabPanel("Transport Parameters",
	 fluidRow(
	 	column(3, 
  		h3("Transportation Parameters"),
  		
  		sliderInput("PercentHomes",
  			    "% of homes that use fertilizer",
  			    min = 0, max = 100, value = 49),
  		sliderInput("TransTurf",
  			    "% N fertilizer transported from turf soils",
  			    min = 0, max = 100, value = 49),
  		sliderInput("TransAg",
  			    "% fertilizer N transported from Agriculture",
  			    min = 0, max = 100, value = 49),
  		sliderInput("TransRec",
  			    "% fertilizer N transported from recreational soils",
  			    min = 0, max = 100, value = 49),
  		sliderInput("TransSeptic",
  			    "% fertilizer N transported from septic tanks",
  			    min = 0, max = 100, value = 49),
  		sliderInput("TransLeach",
  			    "% fertilizer N transported from leaching fields",
  			    min = 0, max = 100, value = 49),
  		sliderInput("TransSepticPlume",
  			    "% fertilizer N transported from septic plumes",
  			    min = 0, max = 100, value = 49),
  		sliderInput("TransVandose",
  			    "% fertilizer N transported from vandose zone",
  			    min = 0, max = 100, value = 49),
  		sliderInput("TransAquifer",
  			    "% fertilizer N transported from aquifer",
  			    min = 0, max = 100, value = 61),
  		sliderInput("ThroughAquiferPonds",
  			    "Throughput to the aquifer from ponds",
  			    min = 0, max = 100, value = 44),
  		sliderInput("ThroughAquiferWetlands",
  			    "Throughput to the aquifer from wetlands",
  			    min = 0, max = 100, value= 22)
  		)
	 )
),
#----
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
  			    value= NA, min = 0, max = NA),
  		numericInput("TurfArea",
  			     "Turf Area- needs better description",
  			     value= NA, min = 0, max = NA),
  		numericInput("AgArea", 
  			     "Agricultural area- needs better description",
  			     value= NA, min = 0, max = NA),
  		numericInput("GolfArea",
  			     "Golf course area (ha)",
  			     value= NA, min = 0, max = NA),
  		numericInput("RoofArea",
  			     "Total area of roofs",
  			     value= NA, min = 0, max = NA),
  		numericInput("DrivewayArea",
  			     "Total area of driveways",
  			     value= NA, min = 0, max = NA),
  		numericInput("ImpervArea",
  			     "Total area of impervious surfaces such as roads/parking lots/runways (ha)",
  			     value = NA, min = 0, max = NA),
  		numericInput("PondsArea",
  			     "Total area of freshwater ponds (ha)",
  			     value= NA, min = 0, max = NA),
  		numericInput("WetlandsArea",
  			     "Total area of wetlands",
  			     value = NA, min = 0, max = NA)
  		),
	 	column(9, 
	 	       h1(textOutput("Load1")))
	 )
	 )
	 
)
)
