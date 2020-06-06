# UI - Mapping Overlaps Gadget Education


ui <- dashboardPage(title = paste("Mapping Overlaps Gadget", sep = ""), 
                    skin = "black",
                    

# Header ------------------------------------------------------------------

  
  dashboardHeader(
    title = span("MOG", style = "color: #0CAE99")
  ),
  

# Sidebar -----------------------------------------------------------------

  dashboardSidebar(
    sidebarMenu(
      menuItem("Welcome to MOG", tabName = "welc", icon = icon('info'), selected = TRUE),
      menuItem("Univariate Maps", tabName = "uni_maps", icon = icon('map-marked-alt')),
      menuItem("Overlap Maps", tabName = "biv_maps", icon = icon('layer-group')),
      menuItem("Correlations", tabName = "corrs", icon = icon('chart-line')),
      menuItem("ANOVA", tabName = "anova", icon = icon('equals')),
      menuItem("Regression", tabName = "regress", icon = icon('project-diagram'))
    )
  ),
  

# Body --------------------------------------------------------------------


  dashboardBody(
    
    use_waiter(),
    show_waiter_on_load(),

    
    tabItems(
      

# Welcome/info tab --------------------------------------------------------
      
      tabItem(tabName = "welc", # Welcome/Info tab 
              
              fluidRow(
                column( # Buffer for Centre
                  2
                ),
                column(
                  8,
                  includeHTML("mog_welcome.html")
                ),
                column( # Buffer for centre
                  2
                )
              )
              
      ), # Close tab
      

# Univariate Maps tab -----------------------------------------------------


      tabItem(tabName = "uni_maps",
               fluidRow(
                 column(
                   12,
                   titlePanel("Univariate Maps")
                 )
               ),
              
              fluidRow( # Intro text
                column(
                  12,
                  helpText(div(HTML("This page allows you to look at the levels of certain variables across neighbourhoods in a local authority, or multiple local authorities. <br>
                           Choose your local authority, or list of local authorities; a variable; and the number of quantiles you would like to split the variable into below (quantiles are ranges on the variable's score with equal numbers of neighbourhoods in). 
                           For example, if I selected Index of Multiple Deprivation Score split into 10 quantiles I would get a plot back where there are an equal number of neighbourhoods in each colour break in the legend (for example, if there were 100 neighbourhoods in total there would be 10 neighbourhoods in each quantile). The quantiles are ordered from low scores to high scores. Higher scores, which ususally mean worse social, economic, or health outcomes (but not always, depending on the variable) are closer to red, lower scores are closer to light orange.<br>
                           If the plot returns fewer quantiles than you requested this is because there are not enough neighbourhoods, or not enough variation in the scores, to create that number of quantiles. In this case it will return the highest number of quantiles possible. You can try adding additional surrounding local authorities to increase the number of quantiles it is possible to create.")))
                )
              ),
              
              fluidRow(
                column(
                  12,
                  selectizeInput("la_names_univ", 
                              "Select one or more local authorities:",
                              choices = NULL,
                              options = list(multiple = TRUE, maxOptions = 1000),
                              multiple = TRUE, 
                              width = "95%"
                  )
                )
              ),
              
              fluidRow( # New row for variable selection and bins slider
                # Select a variable for univariate map
                column(
                  8,
                  selectizeInput("x_univ", 
                              "Select neighbourhood variable:",
                              choices = NULL,
                              options = list(multiple = TRUE, maxOptions = 1000),
                              width = "100%")
                ),
                column(
                  4,
                  sliderInput("bins_slider",
                              "Select the number of quantiles (default = 5)",
                              min = 3, 
                              max = 10,
                              value = 5,
                              round = TRUE,
                              step = 1,
                              animate = FALSE,
                              width = "80%")
                )
              ), # End of Row
              
              fluidRow( # Legend row
                column(12,
                       plotOutput("univ_legend",
                                  height = "60px"),
                       align = "center"
                       ) 
              ),
              
              fluidRow( # Univariate map render
                column(
                  11,
                  plotlyOutput("univ_map", height = "700px") %>% withSpinner(color="#0dc5c1")
                ),
                column(
                  1
                )
                
              )
              
              
              ), # Close tab


# Bivariate maps tab ------------------------------------------------------


      
      tabItem(tabName = "biv_maps", # Open tab - bivariate maps
              fluidRow(
                # Title
                column(
                  12,
                  titlePanel("Overlap (Bivariate) Maps")
                ),
                # Description Placeholder
                column(
                  12,
                  helpText(div(HTML("Pick any combination of two variables to break them into tertiles and look at their relationship on the map when their colour scales are overlapping. 
                            You can pick as many or as few local authorities as you like, but do keep in mind that the more local authorities you select the slower the map will load. 
                            The left hand side variable shows up in shades of red, and the right hand side shows up in variations of blue. 
                            The key for difference combinations is shown in the bottom-right corner of the page.<br>
                           If a map doesn't appear, that usually means that the data for this variable is not available for that specific local authority. This is the case for many of the Welsh authorities.")))
                ),
                column(
                  12, 
                  verbatimTextOutput("click")
                ),
                # Select LAs: Users can enter one or more LAs to map
                # their administrative data
                column(
                  12,
                  selectizeInput("la_names", 
                              "Select one or more local authorities:",
                              choices = NULL,
                              options = list(multiple = TRUE, maxOptions = 1000),
                              multiple = TRUE, 
                              width = "100%"
                              # selected = sample(lsoa_data_spatial$LA_name, 1))
                  )
                )
              ), 
            # End of row
              fluidRow( # New row for variable selection
                # Selectize two variables to use to plot onto the map
                column(
                  6,
                  selectizeInput("x", 
                              "Select Red Variable:",
                              choices = NULL,
                              options = list(multiple = TRUE, maxOptions = 1000),
                              width = "100%")
                ),
                column(
                  6,
                  selectizeInput("y", 
                              "Select Blue Variable:",
                              choices = NULL,
                              options = list(multiple = TRUE, maxOptions = 1000),
                              width = "100%")
                )
              ), # End of Row
            fluidRow( # New row for tertile legends
              column(
                6,
                plotOutput("red_scale", height = "60px")
              ),
              column(
                6,
                plotOutput("blue_scale", height = "60px")
              )
            ), 
            fluidRow( # New row
              column(9,
                     plotlyOutput("bivar_map", height = "700px") %>% withSpinner(color="#0dc5c1")
                     ),
              column(3,
                     helpText(HTML("<br><br>Use the key below to show only areas with that combination of scores. Click once on the legend to activate it, then left click on one of the circles inside the key to redraw the map for just the combination, click on a different circle to change the selected colour. You can add more than one combination by holding down the <em>Shift</em> key and left clicking on additional circles. To reset the map, double-click either on the map itself or anywhere outside of the circles on the key.")),
                     plotlyOutput("legend")
                     )
              
            )
          ),
            

# Correlations tab --------------------------------------------------------


            
        tabItem(tabName = "corrs", # Open tab - correlations
                fluidRow(
                  # Title
                  column(
                    12,
                    titlePanel("Correlations")
                  ),
                  # Description Placeholder
                  column(
                    12,
                    helpText(div(HTML("This page allows you to correlate two variables and shows their relationship on a scatterplot.")))
                  ),
                  # Select LAs: Users can enter one or more LAs to map
                  # their administrative data
                  column(
                    12,
                    selectizeInput("la_names_corrs", 
                                   "Select one or more local authorities:",
                                   choices = NULL,
                                   options = list(multiple = TRUE, maxOptions = 1000),
                                   multiple = TRUE, 
                                   width = "100%"
                                   # selected = sample(lsoa_data_spatial$LA_name, 1))
                    )
                  )
                ), 
                # End of row
                fluidRow( # New row for variable selection
                  # Selectize two variables to use to plot onto the map
                  column(
                    6,
                    selectizeInput("x_corrs", 
                                   "Select X variable:",
                                   choices = NULL,
                                   options = list(multiple = TRUE, maxOptions = 1000),
                                   width = "100%")
                  ),
                  column(
                    6,
                    selectizeInput("y_corrs", 
                                   "Select Y Variable:",
                                   choices = NULL,
                                   options = list(multiple = TRUE, maxOptions = 1000),
                                   width = "100%")
                  )
                ), # End of Row
                fluidRow( # New row
                  column(8,
                         plotOutput("corrplot", height = "700px", width = "100%") 
                         %>% withSpinner(color="#0dc5c1")
                  ),
                  column(4,
                         h4("Pearson's R Correlation Results"),
                         htmlOutput(HTML("corr_result"))
                         )
                  
                )
                ),
      

# ANOVA tab ---------------------------------------------------------------


      tabItem(tabName = "anova", # Open tab - correlations
              fluidRow(
                # Title
                column(
                  12,
                  titlePanel("ANOVA")
                ),
                # Description Placeholder
                column(
                  12,
                  helpText(div(HTML("To compare whether two local authorities have significantly different mean values of a chosen variable add all local authorities you wish to compare to the box below.")))
                ),
                # Select LAs: Users can enter one or more LAs to map
                # their administrative data
                column(
                  12,
                  selectizeInput("la_names_anova", 
                                 "Select one or more local authorities:",
                                 choices = NULL,
                                 options = list(multiple = TRUE, maxOptions = 1000),
                                 multiple = TRUE, 
                                 width = "100%"
                  )
                )
              ), 
              # End of row
              fluidRow( # New row for variable selection
                # Selectize two variables to use to plot onto the map
                column(
                  6,
                  selectizeInput("x_anova", 
                                 "Select variable:",
                                 choices = NULL,
                                 options = list(multiple = TRUE, maxOptions = 1000),
                                 width = "100%")
                )
              ), # End of Row
              fluidRow( # New row
                column(6,
                       plotOutput("anovaplot", height = "700px", width = "100%") 
                       %>% withSpinner(color="#0dc5c1")
                ),
                column(6,
                       h4("Means for each Local Authority"),
                       dataTableOutput("meantable"),
                       h4("Comparison of means and Tukey HSD Significance"),
                       dataTableOutput("tukeytable")
                       )
                
              )
      ),


# Regression tab ----------------------------------------------------------

                
                tabItem(tabName = "regress", # Open tab - regress
                        fluidRow(
                          # Title
                          column(
                            12,
                            titlePanel("Regression & Multiple Regression")
                          ),
                          # Description Placeholder
                          column(
                            12,
                            helpText(div(HTML("This page allows you to build multiple linear regression models using LSOA-level data. You can add multiple independent variables using the left drop down box, and a dependent variable on the right hand drop down box.<br>
                                              ")))
                          ),
                          # Select LAs: Users can enter one or more LAs to map
                          # their administrative data
                          column(
                            12,
                            selectizeInput("la_names_regress", 
                                           "Select one or more local authorities:",
                                           choices = NULL,
                                           options = list(multiple = TRUE, maxOptions = 1000),
                                           multiple = TRUE, 
                                           width = "100%"
                            )
                          )
                        ), 
                        # End of row
                        fluidRow( # New row for variable selection
                          # Selectize two variables to use to plot onto the map
                          column(
                            6,
                            selectizeInput("x_regress", 
                                           "Select X variables:",
                                           choices = NULL,
                                           options = list(multiple = TRUE, maxOptions = 1000),
                                           multiple = TRUE,
                                           width = "100%")
                          ),
                          column(
                            6,
                            selectizeInput("y_regress", 
                                           "Select Y Variable:",
                                           choices = NULL,
                                           options = list(multiple = TRUE, maxOptions = 1000),
                                           width = "100%")
                          )
                        ), # End of Row
                        fluidRow( # New row
                          column(12,
                                 h4("Regression Results"),
                                 dataTableOutput("regress_table")
                                 %>% withSpinner(color="#0dc5c1")
                          )

                        )
                        , # End of Row
                        fluidRow( # New row
                          column(12,
                                 h4("Predictions"),
                                 plotOutput("partial_plots", height = "800px")
                                 %>% withSpinner(color="#0dc5c1")
                          )
                          
                        )
                )
                
        
              
      ), # Close tabs
 # Close tab items
    

# Style tags --------------------------------------------------------------


    # Style
    tags$head(tags$style(HTML('
                          
                          /* body */
                          .content-wrapper, .right-side {
                          background-color: #ffffff;
                          }
                          
                          '))),
    
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    )
    
    

  ) # Close dash body
  
  
)