

server <- shinyServer(function(input, output, session){


# Waiter ------------------------------------------------------------------



  # give time for wait screen to show
  Sys.sleep(1)
  hide_waiter()
  show_waiter(spinner)
  Sys.sleep(2) # give time for wait screen to show
  hide_waiter()


# Quick loading reactive select inputs ------------------------------------
  
# Reactive select inputs
  updateSelectizeInput(session, 
                    "la_names_univ",
                    choices = sort(unique(lsoa_data_spatial$LA_name)),
                    options = list(maxOptions = 1000),
                    selected = "Sheffield",
                    server = TRUE)
  
  updateSelectizeInput(session, 
                       "x_univ",
                       choices = labels_lsoa_data$label[4:nrow(labels_lsoa_data_univ)],
                       options = list(maxOptions = 1000),
                       # Select a random variable to plot from a list of variables shared between England and Wales
                       selected = labels_lsoa_data$label[labels_lsoa_data$var_name == "ChldAYPSDS"],
                       server = TRUE)
  
  updateSelectizeInput(session, 
                       "la_names",
                       choices = sort(unique(lsoa_data_spatial$LA_name)),
                       options = list(maxOptions = 1000),
                       selected = "Sheffield",
                       server = TRUE)
  
  updateSelectizeInput(session, 
                       "la_names_corrs",
                       choices = sort(unique(lsoa_data_spatial$LA_name)),
                       options = list(maxOptions = 1000),
                       selected = "Sheffield",
                       server = TRUE)
  
  updateSelectizeInput(session, 
                       "la_names_anova",
                       choices = sort(unique(lsoa_data_spatial$LA_name)),
                       options = list(maxOptions = 1000),
                       selected = "Sheffield",
                       server = TRUE)
  
  updateSelectizeInput(session, 
                       "la_names_regress",
                       choices = sort(unique(lsoa_data_spatial$LA_name)),
                       options = list(maxOptions = 1000),
                       selected = "Sheffield",
                       server = TRUE)
  
  
  updateSelectizeInput(session, 
                       "x",
                       choices = labels_lsoa_data$label[4:nrow(labels_lsoa_data)],
                       options = list(maxOptions = 1000),
                       # Select a random variable to plot from a list of variables shared between England and Wales
                       selected = labels_lsoa_data$label[labels_lsoa_data$var_name == "IncmDpACIS"],
                       server = TRUE)
  
  updateSelectizeInput(session, 
                       "y",
                       choices = labels_lsoa_data$label[4:nrow(labels_lsoa_data)],
                       options = list(maxOptions = 1000),
                       # Select a random variable to plot from a list of variables shared between England and Wales
                       selected = labels_lsoa_data$label[labels_lsoa_data$var_name == "ChldAYPSDS"],
                       server = TRUE)
  
  updateSelectizeInput(session, 
                       "x_corrs",
                       choices = labels_lsoa_data$label[4:nrow(labels_lsoa_data)],
                       options = list(maxOptions = 1000),
                       # Select a random variable to plot from a list of variables shared between England and Wales
                       selected = labels_lsoa_data$label[labels_lsoa_data$var_name == "IncmDpACIS"],
                       server = TRUE)
  
  updateSelectizeInput(session, 
                       "y_corrs",
                       choices = labels_lsoa_data$label[4:nrow(labels_lsoa_data)],
                       options = list(maxOptions = 1000),
                       # Select a random variable to plot from a list of variables shared between England and Wales
                       selected = labels_lsoa_data$label[labels_lsoa_data$var_name == "ChldAYPSDS"],
                       server = TRUE)
  
  updateSelectizeInput(session, 
                       "x_regress",
                       choices = labels_lsoa_data$label[4:nrow(labels_lsoa_data)],
                       options = list(maxOptions = 1000),
                       # Select a random variable to plot from a list of variables shared between England and Wales
                       selected = labels_lsoa_data$label[labels_lsoa_data$var_name == "IncmDpACIS"],
                       server = TRUE)
  
  updateSelectizeInput(session, 
                       "y_regress",
                       choices = labels_lsoa_data$label[4:nrow(labels_lsoa_data)],
                       options = list(maxOptions = 1000),
                       # Select a random variable to plot from a list of variables shared between England and Wales
                       selected = labels_lsoa_data$label[labels_lsoa_data$var_name == "ChldAYPSDS"],
                       server = TRUE)
  
  updateSelectizeInput(session, 
                       "x_anova",
                       choices = labels_lsoa_data$label[4:nrow(labels_lsoa_data)],
                       options = list(maxOptions = 1000),
                       # Select a random variable to plot from a list of variables shared between England and Wales
                       selected = labels_lsoa_data$label[labels_lsoa_data$var_name == "ChldAYPSDS"],
                       server = TRUE)
  

# Univariate maps  --------------------------------------------------------
  
  output$univ_map <- renderPlotly({
    
    xvar <- sym(labels_lsoa_data_univ$var_name[match(input$x_univ, labels_lsoa_data_univ$label)])
    
    draw_univariate_map(lsoa_data_spatial, !!xvar, input$bins_slider, input$la_names_univ)
    
  })
  
  output$univ_legend <- renderPlot({
    
    xvar <- sym(labels_lsoa_data_univ$var_name[match(input$x_univ, labels_lsoa_data_univ$label)])
    
    draw_univariate_legend(lsoa_data_spatial, !!xvar, nbins = input$bins_slider, input$la_names_univ)
    
  })
  
  output$legend <- renderPlotly({
    
    # Look up variable name string from pretty names in lookup table
    # Paste the matching variable name (ugly variable name)
    # Convert lookups from selectInput from strings to symbols that can be
    # passed using !!
    xvar <- sym(labels_lsoa_data$var_name[match(input$x, labels_lsoa_data$label)])
    yvar <- sym(labels_lsoa_data_2$var_name[match(input$y, labels_lsoa_data_2$label)])
    
    draw_bivar_legend_plotly_coupled(!!xvar, !!yvar) %>%
      event_register('plotly_selected') %>%
      event_register('plotly_deselect')
    
  })
  

# Bivariate Maps ----------------------------------------------------------
  
  combinations <- reactiveVal()
  
  # On click, the key field of the event data contains the fill
  # Add that name to the set of all "selected" cars
  observeEvent(event_data("plotly_selected"), {
    combinations(NULL)
    fill_selected <- event_data("plotly_selected")$key
    combinations_old_new <- c(combinations(), fill_selected)
    combinations(unique(combinations_old_new))
  })
  
  #clear the set of combinations when a double-click occurs
  observeEvent(event_data("plotly_doubleclick"), {
    combinations(NULL)
  })
  
  observeEvent(event_data("plotly_deselect"), {
    combinations(NULL)
  })
  
  plotdata <- reactive({
    xvar <- sym(labels_lsoa_data$var_name[match(input$x, labels_lsoa_data$label)])
    yvar <- sym(labels_lsoa_data_2$var_name[match(input$y, labels_lsoa_data_2$label)])
    plotdata <- bivar_cuts(lsoa_data_spatial, !!xvar, !!yvar, input$la_names)
    plotdata %>% filter(fill %in% combinations())
  })
  
  
  output$bivar_map <- renderPlotly({
    
    
    # Look up variable name string from pretty names in lookup table
    # Paste the matching variable name (ugly variable name)
    # Convert lookups from selectInput from strings to symbols that can be
    # passed using !!
    xvar <- sym(labels_lsoa_data$var_name[match(input$x, labels_lsoa_data$label)])
    yvar <- sym(labels_lsoa_data_2$var_name[match(input$y, labels_lsoa_data_2$label)])
    

    
    plotdata_static <- plotdata()

    if (nrow(plotdata_static) == 0) {
    draw_bivariate_map(lsoa_data_spatial,
                               !!xvar,
                               !!yvar,
                               input$la_names)
    }
    else {
    draw_bivariate_map_coupled(plotdata_static,
                       !!xvar,
                       !!yvar,
                       input$la_names)
    }
      
     
})
  

  
  output$red_scale <- renderPlot({
    
    xvar <- sym(labels_lsoa_data$var_name[match(input$x, labels_lsoa_data$label)])
    
    draw_red_scale(lsoa_data_spatial, !!xvar, input$la_names) 
    
  })
  
  output$blue_scale <- renderPlot({
    
    yvar <- sym(labels_lsoa_data$var_name[match(input$y, labels_lsoa_data$label)])
    
    draw_blue_scale(lsoa_data_spatial, !!yvar, input$la_names) 
    
  })
  

# Correlations  -----------------------------------------------------------
  
  ### CORRELATIONS
  
  output$corrplot <- renderPlot({
    
    xvar <- sym(labels_lsoa_data$var_name[match(input$x_corrs, labels_lsoa_data$label)])
    yvar <- sym(labels_lsoa_data_2$var_name[match(input$y_corrs, labels_lsoa_data_2$label)])
    
    plot_corr(lsoa_data_spatial, !!xvar, !!yvar, input$la_names_corrs)
    
  })
  
  output$corr_result <- renderText({
    
    xvar <- sym(labels_lsoa_data$var_name[match(input$x_corrs, labels_lsoa_data$label)])
    yvar <- sym(labels_lsoa_data_2$var_name[match(input$y_corrs, labels_lsoa_data_2$label)])
    
    corr_test(lsoa_data_spatial, !!xvar, !!yvar, input$la_names_corrs)
    
  })
  

# Anova -------------------------------------------------------------------

  
  output$anovaplot <- renderPlot({
    
    xvar <- sym(labels_lsoa_data$var_name[match(input$x_anova, labels_lsoa_data$label)])
    
    plot_anova(lsoa_data_spatial, !!xvar, input$la_names_anova)
    
  })
  
  output$meantable <- renderDataTable({
    
    xvar <- sym(labels_lsoa_data$var_name[match(input$x_anova, labels_lsoa_data$label)])
    
    datatable(means_table(lsoa_data_spatial, !!xvar, input$la_names_anova))
    
  })
  
  output$tukeytable <- renderDataTable({
    
    xvar <- sym(labels_lsoa_data$var_name[match(input$x_anova, labels_lsoa_data$label)])
    
    datatable(anova_tidy(lsoa_data_spatial, !!xvar, input$la_names_anova))
    
  })
  
  

# Regression --------------------------------------------------------------


  
  output$regress_table <- renderDataTable({
    
    xvars <- labels_lsoa_data %>% filter(label %in% input$x_regress)
    
    yvar <- sym(labels_lsoa_data_2$var_name[match(input$y_regress, labels_lsoa_data_2$label)])
    
    datatable(
      regress_tidy(lsoa_data_spatial, y = !!yvar, x = xvars$var_name, input$la_names_regress) %>% 
        add_column(R.Squared = "") %>% 
        mutate(R.Squared = ifelse(Term == "(Intercept)", 
                                  tidy_rsq(lsoa_data_spatial, 
                                           y = !!yvar, 
                                           x = xvars$var_name, 
                                           input$la_names_regress), R.Squared))
    )
                                           
    
    
  })
  
  
  output$partial_plots <- renderPlot({
    
    xvars <- labels_lsoa_data %>% filter(label %in% input$x_regress)
    
    yvar <- sym(labels_lsoa_data_2$var_name[match(input$y_regress, labels_lsoa_data_2$label)])
    
    partial_regress(lsoa_data_spatial, y = !!yvar, x = xvars$var_name, input$la_names_regress)
    
    
  })
  

})
