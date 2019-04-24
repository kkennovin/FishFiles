
function(input,output, session){
  
  #leaflet plot
  output$mymap <- renderLeaflet({
    leaflet() %>%
      setView(lng = -81, lat = 26, zoom = 7) %>% 
      addProviderTiles(providers$Esri.OceanBasemap)
  })
  
  filterRegion <- reactive({
    fish %>%
      filter(region_desc == input$region) %>% 
      select(., longitude, latitude) %>% 
      distinct(.)
  })
  
  observe({
    leafletProxy("mymap", data = filterRegion) %>%
      clearShapes() %>%
      addCircles(lng = filterRegion()$longitude, lat = filterRegion()$latitude,
                 radius = .1,
                 color = 'purple',
                 fillOpacity = 0.5)
  })
  
  
  
  # total number of fish seen each year
  output$fishY = renderPlot(
    fish %>% group_by(., year) %>% 
      summarise(., total_seen = sum(number_seen/1000)) %>% 
      ggplot(., aes(x = as.character(year), y = total_seen,
                    group = factor(year), fill = factor(year))) +
      geom_col(position = 'dodge') +
      labs(title = 'Total Number of Fish Seen by Year', x = 'Year', y= 'Total Number Seen (Thousands)') +
      scale_fill_brewer(palette = 'Set3', name = 'Year')
    
  )
  # ratio of fish seen per observation
  obs = fish %>% count(as.character(year))
  colnames(obs) = c('year', 'n')
  obs$year = as.numeric(obs$year)
  total = fish %>% group_by(., year) %>% 
    summarise(., total_seen = sum(number_seen))
  
  obs_total = inner_join(obs, total, by = 'year')
  obs_total = obs_total %>% mutate(., ratio = total_seen/n)
  
  output$fishR = renderPlot(
  obs_total %>% 
    ggplot(., aes(x = as.character(year), y = ratio,
                  group = factor(year), fill = factor(year))) +
    geom_col(position = 'dodge') +
    labs(title = 'Ratio of Fish Seen per Sample by Year', x = 'Year', y= 'Ratio of Fish Seen per Sample') +
    scale_fill_brewer(palette = 'Set3', name = 'Year')
  )
  
  # find top 12 most common fish per region
  fish_cnt = fish %>% 
    group_by(., region_desc, common_name) %>% 
    summarise(., cnt = sum(number_seen)) %>%
    top_n(12, cnt)
  
  # sort most common fish greatest to least
  fish_cnt_sort = fish_cnt %>% 
    group_by(., region_desc) %>% 
    arrange(., region_desc, desc(cnt))
  
  
  # reactive filter by region
  fish_filtered = reactive({
    fish_cnt_sort %>%
      filter(., region_desc == input$region)
  })
  
  #update selection of most common fish by region
  observe({
    commonFish = fish_filtered() %>% 
      select(., common_name)
    updateSelectizeInput(
      session = session,
      inputId = 'top_fish',
      choices = commonFish,
      selected = commonFish[1]
    )
    
  })
  
  #fish specs
  observeEvent(input$getSpecs, {
    fish_specs = fish %>% 
      filter(., region_desc == input$region & common_name == input$top_fish) %>% 
      summarise(., 
                Average_Length = mean(length_fish),
                Average_Number_Seen = mean(number_seen),
                Most_Common_Depth = median(depth),
                Most_Common_Habitat = names(which.max(table(habitat_type)))
                )
    
    output$fishSpecs = renderTable({
      df = data.frame(t(as.matrix(fish_specs)))
      df$field = colnames(fish_specs)
      # df$values = t(as.matrix(fish_specs))
      df = df[,2:1]
      colnames(df) = c('field','value')
      df
    })
      
    
  })
  
  
  
  
  # plot most common fish per region
  output$fish_by_region = renderPlot(
    fish_filtered() %>% 
      ggplot(.) +
      geom_bar(aes(x=reorder(common_name, cnt), y = cnt, 
                   fill = common_name), 
               position = 'dodge', stat = 'identity') +
      coord_flip() +
      labs(title = paste('12 Most Common Species in', input$region, 'Region', sep = ' '), 
           x = 'Species', y = 'Number Seen') +
      theme(legend.position = "none")
  )
  
  
    
  # change in number of most common fish seen by year
  output$top_by_year = renderPlot(
    fish %>% 
      filter(., region_desc == input$region & (common_name %in% fish_filtered()$common_name)) %>% 
      group_by(., common_name, year) %>% 
      summarise(., cnt = sum(number_seen)) %>% 
      ggplot(.) +
      geom_bar(aes(x=reorder(common_name, cnt), y = cnt, 
                   fill = as.character(year)), 
               position = position_dodge(width=-0.95), stat = 'identity') +
      coord_flip() +
      labs(title = paste('12 Most Common Species in', input$region, 'Region by Year', sep = ' '), x = 'Species', y = 'Number Seen') +
      scale_fill_brewer(palette = 'Set1', name = 'Year')
    
  )
  
  # ratio of obs at each depth vs totals obs per year
  depth_ratios = fish %>% 
    group_by(., year, as.integer(round(depth))) %>% 
    count(.) %>% 
    ungroup(.) %>%
    group_by(., year) %>%
    mutate(., ratio = n/sum(n))
  
  colnames(depth_ratios) = c('Year','depth','n','ratio')
  depth_ratios$Year = as.character(depth_ratios$Year)
  
  
  output$depthR_by_year = renderPlot(
  depth_ratios %>%
    ggplot(., aes(x = depth, y = ratio)) +
    geom_smooth(aes(color = Year), se = F) +
    labs(title = 'Ratio of Observations Taken at Each Depth vs. Total Observations per Year',
         x = 'Depth', 
         y = 'Ratio')
  
  )
  
  #number of fish seen by habitat per year
  output$hab_by_year = renderPlot(
  fish %>% group_by(., habitat_type, year) %>% 
    summarise(., total_seen = sum(number_seen/1000)) %>% 
    ggplot(., aes(x = habitat_type, y = total_seen, fill = as.character(year))) +
    geom_col(position = position_dodge(width=-0.95)) +
    coord_flip() +
    scale_fill_brewer(palette = 'Set1', name = 'Year') +
    labs(title = 'Number of Fish Seen by Habitat and Year',
         y = 'Total Number of Fish Seen (in Thousands)', x = 'Habitat Type')
  )
  
  
  
  
  
  
  
  
  
  
}



