#### Shiny Server
#
# NLM Model running on shiny
# Runs basic Nitrogen loading models from data read in by user
# Sensitivity analysis is possible by manipulating the many parameters 
#
#
####

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
# 	percent of onsite wastewater systems that are cesspools

shinyUI(navbarPage("N-Load",
		   #theme("bootstrap.css",
# Data Loading Tab ---- 
tabPanel("Data Loading", 
    fluidRow(
    	tags$h3("MODEL RESULTS NOT ACCURATE - MODIFICATION NEEDED"),
    	column(2,
    	       h3("Load in data file here:"),
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
    	       uiOutput("ParkAreas")
    	), ## Can add 'conditionPanel()' to allow for extra data/parameter mapping options or NLoad options.
    	column(8,
    	       h4("Geographic Paramters read in by user"),
    	       tableOutput("filetable"),
    	       tableOutput("filetable2")
    	       )
	    )
	),
# # Trial Runs Tab ----
tabPanel("Trial Runs",
	 fluidRow(
	 	tags$h6("This is a test of the emergency broadcast system..."),
	 	column(6,
	 	       h5("holy shit if this works...")),
	 	column(6,
	 	       h4("Put out puts here in text form..."))
		)
	 ),
# Wastewater Parameters ----
tabPanel("Direct Human Inputs",
	 fluidRow(
	 	column(3,
	 	   h4("Direct Application of fertilizers"),
	 	       sliderInput("PercentHomes",
	 	       	    "% of homes that use fertilizer",
	 	       	    min = 0.00, max = 1.00, value = 0.49),
	 	   sliderInput("FertLawns",
	 	   	    "Nitrogen from fertilizer applied to lawns (kg N/ha):",
	 	   	    min= 50, max= 150, value= 104, ticks= FALSE),
	 	   sliderInput("FertAg", 
	 	   	    "Nitrogen from fertilizer applied to agricultural lands (kg N/ha):",
	 	   	    min= 50, max= 150, value= 136, ticks= FALSE),
# 	 	   sliderInput("FertVineyards", 
# 	 	   	    "Nitrogen from fertilizer applied to vineyards (kg N/ha):",
# 	 	   	    min= 0, max= 20, value= 8.41, ticks= FALSE),
	 	   sliderInput("FertRec", 
	 	   	    "Nitrogen from fertilizer applied to golf courses and recreational lands (kg N/ha):",
	 	   	    min= 0, max= 200, value= 115, ticks= FALSE),
	 	   h4("Septic System & Cesspool Efficiencies"),
	 	  	numericInput("HumanLoad",
	 	   	     "Human N released per year (kg??)", #Need to confirm units
	 	   	     min = 0.00, max = 10.00, value = 4.80),
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
	 	   h4("Sewage Treatment Plant Efficiencies"),
	 	       sliderInput("AvgAnSTPLoad",  # Need to get an idea of range-- Maybe add another control for modifying the 'efficiency' of a STP for running scenarios.
	 	       	    "Average annual wastewater N concentration (kg N/L)",
	 	       	    min = 0, max = 1000, value = 100),
	 	       sliderInput("TotAnFlow",   # need to reformat- numbers too large
	 	       	    "Total average annual flow (L)",
	 	       	    min = 0, max = 1000000000, value = 500000, step = 1000)
	 	       ),
	 	column(9,
	 	       h4("Insert a plot of fertilizer loadings and septic? Again, facet on subestuary or scenario"))
	 	)
	  ),
# Less Active Parameters ----
navbarMenu("Additional Model Parameters", 
	   tabPanel("Atmospheric Loading Parameters",
	   	 fluidRow(
	   	 	column(3,
	   	 	       h4("Atmospheric Loading Parameters"),
	   	 	       h6("...insert some commentary on what these parameters mean and do..."),
	   	 	       sliderInput("AtmNtransNatVeg",
	   	 	       	    "% atmos N transported from Nat'l Veg Soils:",
	   	 	       	    min = 0.00, max = 1.00, value = 0.35),
	   	 	       sliderInput("AtmNtransTurf",
	   	 	       	    "% atmos N transported from Turf Soils::",
	   	 	       	    min = 0.00, max = 1.00, value = 0.38),
	   	 	       sliderInput("AtmNtransAg",
	   	 	       	    "% atmos N transported from Agr. Soils:",
	   	 	       	    min = 0.00, max = 1.00, value = 0.38),
	   	 	       sliderInput("NtransWetlands",
	   	 	       	    "% atmos N transported from wetlands:",
	   	 	       	    min = 0.00, max = 1.00, value = 0.38),
	   	 	       sliderInput("NtransPonds",
	   	 	       	    "% atmos N transported from freshwater ponds:",
	   	 	       	    min = 0.00, max = 1.00, value = 0.38),
	   	 	       sliderInput("AtmNtransImperv", # NLM oysterbay spreadsheet does not use this...
	   	 	       	    "% atmos N transported from Impervious Soils (roof/driveway):",
	   	 	       	    min = 0.00, max = 1.00, value = 0.38),
	   	 	       
	   	 	       sliderInput("DeNit",
	   	 	       	    "% Not lost as gases -- Denitrification?",  ## used one input for g-h-i
	   	 	       	    min = 0.00, max = 1.00, value = 0.61)
	   	 		),
	   	 	column(9,
	   	 	       h3("holy smokes"),
	   	 	       h4("Should add a graphic with the atmospheric loads for each subwater shed (OR MANIPULATION/SCENARIO OF A WATERSHED")
	   	 		)
	   		 )
	   	),
# ---> Transport Parameters ----
	   tabPanel("Fertilizer Loading and Transportportation",
	   	 fluidRow(
	   	 	column(3, 
	   	 	       # transportation parameter UI inputs
	   	 	       
		   	 	h4("Fertilizer Transportation and biochemical processes resulting in denitriciation or loss"),
	   	 	       sliderInput("FertTransTurf",
	   	 	       	    "% N fertilizer transported from turf soils",
	   	 	       	    min = 0.00, max = 1.00, value = 0.49),
	   	 	       sliderInput("FertTransAg",
	   	 	       	    "% fertilizer N transported from Agriculture",
	   	 	       	    min = 0.00, max = 1.00, value = 0.49),
	   	 	       sliderInput("FertTransRec",
	   	 	       	    "% fertilizer N transported from recreational soils",
	   	 	       	    min = 0.00, max = 1.00, value = 0.49),
	   	 	       sliderInput("FertTransSeptic",
	   	 	       	    "% fertilizer N transported from septic tanks",
	   	 	       	    min = 0.00, max = 1.00, value = 0.49),
	   	 	       sliderInput("FertTransLeach",
	   	 	       	    "% fertilizer N transported from leaching fields",
	   	 	       	    min = 0.00, max = 1.00, value = 0.49),
	   	 	       sliderInput("FertTransSepticPlume",
	   	 	       	    "% fertilizer N transported from septic plumes",
	   	 	       	    min = 0.00, max = 1.00, value = 0.49),
	   	 	       sliderInput("FertTransVandose",
	   	 	       	    "% fertilizer N transported from vandose zone",
	   	 	       	    min = 0.00, max = 1.00, value = 0.49),
	   	 	       sliderInput("FertTransAquifer",
	   	 	       	    "% fertilizer N transported from aquifer",
	   	 	       	    min = 0.00, max = 1.00, value = 0.61),
	   	 	    h5("Throughput to the aquifer"),
		   	 	sliderInput("ThroughAquiferPonds",
	   	 	       	    "Throughput to the aquifer from ponds",
	   	 	       	    min = 0.00, max = 1.00, value = 0.44),
	   	 	       sliderInput("ThroughAquiferWetlands",
	   	 	       	    "Throughput to the aquifer from wetlands",
	   	 	       	    min = 0.00, max = 1.00, value= 0.22)
	   	 		),
	   	 	column(9,
	   	 	       h4("Not sure what to put here. Graphic of how this transportation works??"))
	   	 	)
	   	),
# ---> Physical loading parameters ----
	   tabPanel("Physical Loading Parameters", 
		   fluidRow(
		   	column(3, 
		   	       h4("Physical Loading Parameters"),
		   	       sliderInput("AtmDepRate",
		   	       	    label= "Atmospheric Deposition Rate (kg N/ha/yr):", 
		   	       	    min = 12.0, max = 17.0, value = 15.1, round= FALSE, step= 0.1), 
		   	       
		   	       sliderInput("HumanExcretion",
		   	       	    "Per capita human N excretion rate (kg N/pp/yr):",
		   	       	    min= 0.0, max= 10.0, value= 4.8, round= FALSE, step= 0.1)
	   			),
		   	column(9,
		   	       h4("might not be worth keeping this tab around...."))
	   		)
		)
	
	) ,
	   
# Output Summary Tab ----
tabPanel("Loading Sources",
	 fluidRow(
	 	column(6,
	 	       h4("Distribution of N loading by sub-watershed"),
	 	       showOutput("HStackBar", "dimple")),
	 	column(6,
	 	       h4("Proportions by source"),
	 	       showOutput("HStackPct", "dimple"))
	 	)
	),
# # Output Summary Tab #2 ----
tabPanel("Distribution of Loads",
	fluidRow(
		column(12,
		       h4("Distribution of N loads to Peconics- Interactive...."),
		       showOutput("plot", "nvd3"))
		)
	)
#----
)
)




	 