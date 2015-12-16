library("shiny")
library("h2o")

setwd("C:/Users/PedroHenrique/OneDrive/Financial Engineering/Fannie Mae/fannie_mae/www")
source("graphs_functions.R")

pred_banks_data <- getdata_banks_list(1,14)
pred_zip_data <- getdata_zip_list(1,14)

shinyServer(function(input, output) {
  
  #configure the bs histogram plot
  output$bcs_hist <- renderPlot({
    
    year_lower <- switch (as.character(input$year_slide[1]),
      "2000" = 1,
      "2001" = 2,
      "2002" = 3,
      "2003" = 4,
      "2004" = 5,
      "2005" = 6,
      "2006" = 7,
      "2007" = 8,
      "2008" = 9,
      "2009" = 10,
      "2010" = 11,
      "2011" = 12,
      "2012" = 13,
      "2013" = 14,
      "2014" = 15
    )
    
    year_upper <- switch (as.character(input$year_slide[2]),
                          "2000" = 1,
                          "2001" = 2,
                          "2002" = 3,
                          "2003" = 4,
                          "2004" = 5,
                          "2005" = 6,
                          "2006" = 7,
                          "2007" = 8,
                          "2008" = 9,
                          "2009" = 10,
                          "2010" = 11,
                          "2011" = 12,
                          "2012" = 13,
                          "2013" = 14,
                          "2014" = 15
    )
    
    bcs_lower <- input$bcs_slider[1]
    bcs_upper <- input$bcs_slider[2]
    
    oir_lower <- input$oir_slider[1]
    oir_upper <- input$oir_slider[2]
    
    olv_lower <- input$olv_slider[1]
    olv_upper <- input$olv_slider[2]
    
    dir_lower <- input$dir_slider[1]
    dir_upper <- input$dir_slider[2]
    
    bank_name <- as.character(input$select_banks)
    
    all_banks <- input$checkbox_banks
    
    generate_bcs_hist(year_lower, year_upper,bcs_lower,bcs_upper,oir_lower,oir_upper,olv_lower,olv_upper,dir_lower,dir_upper,bank_name,all_banks)
  })
  
  
  output$olv_hist <- renderPlot({
    
    year_lower <- switch (as.character(input$year_slide[1]),
                          "2000" = 1,
                          "2001" = 2,
                          "2002" = 3,
                          "2003" = 4,
                          "2004" = 5,
                          "2005" = 6,
                          "2006" = 7,
                          "2007" = 8,
                          "2008" = 9,
                          "2009" = 10,
                          "2010" = 11,
                          "2011" = 12,
                          "2012" = 13,
                          "2013" = 14,
                          "2014" = 15
    )
    
    year_upper <- switch (as.character(input$year_slide[2]),
                          "2000" = 1,
                          "2001" = 2,
                          "2002" = 3,
                          "2003" = 4,
                          "2004" = 5,
                          "2005" = 6,
                          "2006" = 7,
                          "2007" = 8,
                          "2008" = 9,
                          "2009" = 10,
                          "2010" = 11,
                          "2011" = 12,
                          "2012" = 13,
                          "2013" = 14,
                          "2014" = 15
    )
    
    bcs_lower <- input$bcs_slider[1]
    bcs_upper <- input$bcs_slider[2]
    
    oir_lower <- input$oir_slider[1]
    oir_upper <- input$oir_slider[2]
    
    olv_lower <- input$olv_slider[1]
    olv_upper <- input$olv_slider[2]
    
    dir_lower <- input$dir_slider[1]
    dir_upper <- input$dir_slider[2]
    
    bank_name <- as.character(input$select_banks)
    
    all_banks <- input$checkbox_banks
    
    generate_olv_hist(year_lower, year_upper,bcs_lower,bcs_upper,oir_lower,oir_upper,olv_lower,olv_upper,dir_lower,dir_upper,bank_name,all_banks)
  })
  
  output$dir_hist <- renderPlot({
    
    year_lower <- switch (as.character(input$year_slide[1]),
                          "2000" = 1,
                          "2001" = 2,
                          "2002" = 3,
                          "2003" = 4,
                          "2004" = 5,
                          "2005" = 6,
                          "2006" = 7,
                          "2007" = 8,
                          "2008" = 9,
                          "2009" = 10,
                          "2010" = 11,
                          "2011" = 12,
                          "2012" = 13,
                          "2013" = 14,
                          "2014" = 15
    )
    
    year_upper <- switch (as.character(input$year_slide[2]),
                          "2000" = 1,
                          "2001" = 2,
                          "2002" = 3,
                          "2003" = 4,
                          "2004" = 5,
                          "2005" = 6,
                          "2006" = 7,
                          "2007" = 8,
                          "2008" = 9,
                          "2009" = 10,
                          "2010" = 11,
                          "2011" = 12,
                          "2012" = 13,
                          "2013" = 14,
                          "2014" = 15
    )
    
    bcs_lower <- input$bcs_slider[1]
    bcs_upper <- input$bcs_slider[2]
    
    oir_lower <- input$oir_slider[1]
    oir_upper <- input$oir_slider[2]
    
    olv_lower <- input$olv_slider[1]
    olv_upper <- input$olv_slider[2]
    
    dir_lower <- input$dir_slider[1]
    dir_upper <- input$dir_slider[2]
    
    bank_name <- as.character(input$select_banks)
    
    all_banks <- input$checkbox_banks
    
    generate_dir_hist(year_lower, year_upper,bcs_lower,bcs_upper,oir_lower,oir_upper,olv_lower,olv_upper,dir_lower,dir_upper,bank_name,all_banks)
  })
  
  
  output$oir_hist <- renderPlot({
    
    year_lower <- switch (as.character(input$year_slide[1]),
                          "2000" = 1,
                          "2001" = 2,
                          "2002" = 3,
                          "2003" = 4,
                          "2004" = 5,
                          "2005" = 6,
                          "2006" = 7,
                          "2007" = 8,
                          "2008" = 9,
                          "2009" = 10,
                          "2010" = 11,
                          "2011" = 12,
                          "2012" = 13,
                          "2013" = 14,
                          "2014" = 15
    )
    
    year_upper <- switch (as.character(input$year_slide[2]),
                          "2000" = 1,
                          "2001" = 2,
                          "2002" = 3,
                          "2003" = 4,
                          "2004" = 5,
                          "2005" = 6,
                          "2006" = 7,
                          "2007" = 8,
                          "2008" = 9,
                          "2009" = 10,
                          "2010" = 11,
                          "2011" = 12,
                          "2012" = 13,
                          "2013" = 14,
                          "2014" = 15
    )
    
    bcs_lower <- input$bcs_slider[1]
    bcs_upper <- input$bcs_slider[2]
    
    oir_lower <- input$oir_slider[1]
    oir_upper <- input$oir_slider[2]
    
    olv_lower <- input$olv_slider[1]
    olv_upper <- input$olv_slider[2]
    
    dir_lower <- input$dir_slider[1]
    dir_upper <- input$dir_slider[2]
    
    bank_name <- as.character(input$select_banks)
    
    all_banks <- input$checkbox_banks
    
    generate_oir_hist(year_lower, year_upper,bcs_lower,bcs_upper,oir_lower,oir_upper,olv_lower,olv_upper,dir_lower,dir_upper,bank_name,all_banks)
  })
  
  
  output$bank_vertical <- renderPlot({
    
    year_lower <- switch (as.character(input$year_slide[1]),
                          "2000" = 1,
                          "2001" = 2,
                          "2002" = 3,
                          "2003" = 4,
                          "2004" = 5,
                          "2005" = 6,
                          "2006" = 7,
                          "2007" = 8,
                          "2008" = 9,
                          "2009" = 10,
                          "2010" = 11,
                          "2011" = 12,
                          "2012" = 13,
                          "2013" = 14,
                          "2014" = 15
    )
    
    year_upper <- switch (as.character(input$year_slide[2]),
                          "2000" = 1,
                          "2001" = 2,
                          "2002" = 3,
                          "2003" = 4,
                          "2004" = 5,
                          "2005" = 6,
                          "2006" = 7,
                          "2007" = 8,
                          "2008" = 9,
                          "2009" = 10,
                          "2010" = 11,
                          "2011" = 12,
                          "2012" = 13,
                          "2013" = 14,
                          "2014" = 15
    )
    
    bcs_lower <- input$bcs_slider[1]
    bcs_upper <- input$bcs_slider[2]
    
    oir_lower <- input$oir_slider[1]
    oir_upper <- input$oir_slider[2]
    
    olv_lower <- input$olv_slider[1]
    olv_upper <- input$olv_slider[2]
    
    dir_lower <- input$dir_slider[1]
    dir_upper <- input$dir_slider[2]
    
    bank_name <- as.character(input$select_banks)
    
    all_banks <- input$checkbox_banks
    
    generate_banks(year_lower, year_upper,bcs_lower,bcs_upper,oir_lower,oir_upper,olv_lower,olv_upper,dir_lower,dir_upper,bank_name,all_banks)
  })
  
  
  output$oir_hist <- renderPlot({
    
    year_lower <- switch (as.character(input$year_slide[1]),
                          "2000" = 1,
                          "2001" = 2,
                          "2002" = 3,
                          "2003" = 4,
                          "2004" = 5,
                          "2005" = 6,
                          "2006" = 7,
                          "2007" = 8,
                          "2008" = 9,
                          "2009" = 10,
                          "2010" = 11,
                          "2011" = 12,
                          "2012" = 13,
                          "2013" = 14,
                          "2014" = 15
    )
    
    year_upper <- switch (as.character(input$year_slide[2]),
                          "2000" = 1,
                          "2001" = 2,
                          "2002" = 3,
                          "2003" = 4,
                          "2004" = 5,
                          "2005" = 6,
                          "2006" = 7,
                          "2007" = 8,
                          "2008" = 9,
                          "2009" = 10,
                          "2010" = 11,
                          "2011" = 12,
                          "2012" = 13,
                          "2013" = 14,
                          "2014" = 15
    )
    
    bcs_lower <- input$bcs_slider[1]
    bcs_upper <- input$bcs_slider[2]
    
    oir_lower <- input$oir_slider[1]
    oir_upper <- input$oir_slider[2]
    
    olv_lower <- input$olv_slider[1]
    olv_upper <- input$olv_slider[2]
    
    dir_lower <- input$dir_slider[1]
    dir_upper <- input$dir_slider[2]
    
    bank_name <- as.character(input$select_banks)
    
    all_banks <- input$checkbox_banks
    
    generate_oir_hist(year_lower, year_upper,bcs_lower,bcs_upper,oir_lower,oir_upper,olv_lower,olv_upper,dir_lower,dir_upper,bank_name,all_banks)
  })
  
  
  output$map <- renderPlot({
    
    year_lower <- switch (as.character(input$year_slide[1]),
                          "2000" = 1,
                          "2001" = 2,
                          "2002" = 3,
                          "2003" = 4,
                          "2004" = 5,
                          "2005" = 6,
                          "2006" = 7,
                          "2007" = 8,
                          "2008" = 9,
                          "2009" = 10,
                          "2010" = 11,
                          "2011" = 12,
                          "2012" = 13,
                          "2013" = 14,
                          "2014" = 15
    )
    
    year_upper <- switch (as.character(input$year_slide[2]),
                          "2000" = 1,
                          "2001" = 2,
                          "2002" = 3,
                          "2003" = 4,
                          "2004" = 5,
                          "2005" = 6,
                          "2006" = 7,
                          "2007" = 8,
                          "2008" = 9,
                          "2009" = 10,
                          "2010" = 11,
                          "2011" = 12,
                          "2012" = 13,
                          "2013" = 14,
                          "2014" = 15
    )
    
    bcs_lower <- input$bcs_slider[1]
    bcs_upper <- input$bcs_slider[2]
    
    oir_lower <- input$oir_slider[1]
    oir_upper <- input$oir_slider[2]
    
    olv_lower <- input$olv_slider[1]
    olv_upper <- input$olv_slider[2]
    
    dir_lower <- input$dir_slider[1]
    dir_upper <- input$dir_slider[2]
    
    bank_name <- as.character(input$select_banks)
    
    all_banks <- input$checkbox_banks
    
    generatemap_county(year_lower, year_upper,bcs_lower,bcs_upper,oir_lower,oir_upper,olv_lower,olv_upper,dir_lower,dir_upper,bank_name,all_banks)
  })
  
  output$banks <- renderUI({
    
    year_lower <- switch (as.character(input$year_slide[1]),
                          "2000" = 1,
                          "2001" = 2,
                          "2002" = 3,
                          "2003" = 4,
                          "2004" = 5,
                          "2005" = 6,
                          "2006" = 7,
                          "2007" = 8,
                          "2008" = 9,
                          "2009" = 10,
                          "2010" = 11,
                          "2011" = 12,
                          "2012" = 13,
                          "2013" = 14,
                          "2014" = 15
    )
    
    year_upper <- switch (as.character(input$year_slide[2]),
                          "2000" = 1,
                          "2001" = 2,
                          "2002" = 3,
                          "2003" = 4,
                          "2004" = 5,
                          "2005" = 6,
                          "2006" = 7,
                          "2007" = 8,
                          "2008" = 9,
                          "2009" = 10,
                          "2010" = 11,
                          "2011" = 12,
                          "2012" = 13,
                          "2013" = 14,
                          "2014" = 15
    )
    
    banks_data <- getdata_banks_list(year_lower,year_upper)
    
    "select_banks" = selectInput("select_banks",label="banks",
                          choices = banks_data,
                          selected = 1
      
    )
    
    
  })
  
  output$yearly <- renderPlot({
    generate_yearly()
  })
  
  output$model <- renderPlot({
    #test a different set on the pred formula usign the model
    
    sn_entry <- "OTHER"
    bcs_entry <- input$pred_bcs
    olv_entry <- input$pred_olv
    dir_entry <- input$pred_dir
    oir_entry <- input$pred_oir
    z_entry <- input$
    entry <- data.frame("sn"=sn_entry,"olv"=olv_entry,"bcs"=bcs_entry,"dir"=dir_entry,"oir"=oir_entry,"z"=z_entry)
    entry$sn <- as.factor(entry$sn)
    entry$z <- as.factor(entry$z)
    entry <- as.h2o(entry)
    pred_entry <- predict(fit_2, entry)
    pred_entry
  })
  
})