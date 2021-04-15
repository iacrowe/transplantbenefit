# TransplantBenefit.org
# Ewen Harrison
# Surgical Inforamatics
# University of Edinburgh
# Started November 2018

library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(ggridges)
library(viridis)

# Define UI
ui <- dashboardPage(
	
	title = "TBS comparison",
	skin="black",
	dashboardHeader(
		disable = TRUE,
	  tags$li(class = "dropdown", # Hack to get this to work. 
						tags$head(
							tags$link(rel="shortcut icon", href="https://argonaut.is.ed.ac.uk/public/favicon.ico"),
							
							# Google analytics tracker info pasted into root file named as below. Not included in git. 
							# includeHTML("analytics.html")
						)
		),
		
		# Retina quality images via JS plugin, see end body
		title = HTML("<a href = 'https://transplantbenefit.org'>
									<img src='/public/tb.png' data-rjs = '3' />
										</a>"
		),
		
		titleWidth = 320
	),
	
	dashboardSidebar(disable = FALSE, collapsed = TRUE,
									 sidebarMenu(
									 	menuItem("Transplant benefit score", tabName = "tbs"),
									 	menuItem("Acute liver failure", tabName = "acute_liver"),
									 	menuItem("HCC", tabName = "hcc"),
									 	menuItem("Variant syndromes", tabName = "variants"))
	),
	dashboardBody(
		tabItems(
			# Transplant benefit score ----
			tabItem("tbs",
							fluidRow(
								# Input
								column(2,
											 fluidRow(source(file.path("ui", "ui_input1_header.R"))$value),
											 fluidRow(source(file.path("ui", "ui_input1_1.R"))$value)
								),
								column(2,
											 source(file.path("ui", "ui_input1_2.R"))$value
								),
								column(2,
											 source(file.path("ui", "ui_input1_3.R"))$value,
											 source(file.path("ui", "ui_input1_4.R"))$value,
											 
								),
								column(3,
											 fluidRow(
											   box(title = "Need (M1) comparison", status = "info", width=12, 
											       collapsible = TRUE, collapsed = TRUE,
											       
											       p("The distribution and point estimate of the current
											         and updated M1 values are shown (lower equals greater need)")
											   ),
											  box(plotOutput("m1_comparison", height = 300), width = 12),
											  br(),
											  box(title = "Current Models", status = "info", width=12),
										    infoBoxOutput("tbsBox_current", width=12),
											 	infoBoxOutput("m1Box_current", width=12),
											 	infoBoxOutput("m2Box_current", width=12)
											 ),
											 source(file.path("ui", "ui_input1_5.R"))$value
											 
								),
								column(3,
								       fluidRow(
								         box(title = "TBS comparison", status = "info", width=12, 
								             collapsible = TRUE, collapsed = TRUE,
								             
								             p("The distribution and point estimate of the current
											         and updated TBS (higher equals greater benefit")
								         ),
								       box(plotOutput("tbs_comparison", height = 300), width = 12),
								       br(),
								       box(title = "Updated Models", status = "info", width=12),
								       infoBoxOutput("tbsBox_new", width=12),
								       infoBoxOutput("m1Box_new", width=12),
								       infoBoxOutput("m2Box_new", width=12),
								       br(),
								       source(file.path("ui", "ui_1_6_selection_criteria.R"))$value,
								       source(file.path("ui", "ui_1_7_cld_criteria.R"))$value,
								       source(file.path("ui", "ui_footnote.R"))$value
								      )

											 # Plots to return when distribution data available
											 # box(
											 # 	plotOutput("hist_m1", height=200),
											 # 	plotOutput("hist_m2", height=200),
											 # 	plotOutput("hist_tbs", height=200), 
											 # 	title = "Population comparison",
											 # 	width=12,
											 # 	collapsible = TRUE)
								)
							),
							
							
							# Retina images
							tags$script(type="text/javascript", src="retina.min.js")
			),
			
			# Acute liver failure ----
			tabItem("acute_liver",
							fluidRow(
								column(width = 4,
											 source(file.path("ui", "ui_input2_1_acute_liver.R"))$value
								),
								column(width = 4,
											 infoBoxOutput("uk_alfBox", width=12)
								),
								column(width = 4,
											 source(file.path("ui", "ui2_2_ald_criteria.R"))$value,
											 source(file.path("ui", "ui_footnote.R"))$value
								)
							)
			),
			
			# Hepatocellular carcinoma ----
			tabItem("hcc", 
							fluidRow(
								column(width = 4,
											 source(file.path("ui", "ui_input3_1_hcc.R"))$value
								),
								column(width = 4,
											 infoBoxOutput("uk_hccBox", width=12),
											 infoBoxOutput("milan_hccBox", width=12),
											 infoBoxOutput("ucsf_hccBox", width=12)
								),
								column(width = 4,
											 infoBoxOutput("duvoux_hccBox", width=12),
											 source(file.path("ui", "ui_hcc_criteria.R"))$value,
											 source(file.path("ui", "ui_footnote.R"))$value
								)
							)
			),
			
			# Variant syndromes ----
			tabItem("variants",
							source(file.path("ui", "ui_input4_1_var.R"))$value
							
			)
		)
	)
)

# Server
server <- function(input, output, session) {
	
	# Include the logic (server) for each tab
	source(file.path("server", "server0_fn_comp.R"),  local = TRUE)$value
	
	# TBS server ----
	source(file.path("server", "server1_1.R"),  local = TRUE)$value
	source(file.path("server", "server1_2.R"),  local = TRUE)$value
	source(file.path("server", "server1_3.R"),  local = TRUE)$value
	# source(file.path("server", "server1_4.R"),  local = TRUE)$value
	source(file.path("server", "server1_5_download.R"),  local = TRUE)$value
	
	# ALF server ----
	source(file.path("server", "server2_1_alf.R"),  local = TRUE)$value
	
	# HCC server ----
	source(file.path("server", "server3_1_hcc.R"),  local = TRUE)$value
	
	# Variant server ----
	# Intentionally blank. 
}

# Run the application 
shinyApp(ui = ui, server = server)
