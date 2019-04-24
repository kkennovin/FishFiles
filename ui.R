
fluidPage(
  titlePanel("Assessment of coral reef fish communities in the Florida Reef Tract 2014 & 2016"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        inputId = 'region',
        label = 'Region',
        choices = sort(unique(fish$region_desc))
      ),
      selectizeInput(
        inputId = 'top_fish',
        label = 'Most Common Fish',
        choices = unique(fish$common_name)
      ),
      actionButton(
        inputId = 'getSpecs',
        label = 'Get Fish Specs'
        
      ),
      tableOutput('fishSpecs')

    ),
    mainPanel(tabsetPanel(
      tabPanel('Intro and Regions',
               leafletOutput('mymap'
               ),
               helpText('This dataset contains observations taken in 2014 and 2016 of coral reef fish communities located in the Florida Keys Reef Tract. 
                        Each observation records many attributes of the fish seen and the habitat where it was found. Measurement stations were divided into  
                        3 main regions: Dry Tortugas, Key West to Miami, and Miami to North Martin County. Within these regions, 20 different coral habitat  
                        types and 406 different fish species were observed. I have chosen a few attributes of this dataset to explore, including region, habitat 
                        type, depth of the observation, and number of fish seen during the observation. My goal was to find changes in these attributes from 2014 
                        to 2016.')
               
               # img(
               #   src = 'region.png',
               #   height = 600,
               #   width = 600
               #)
      ),
     tabPanel('Initial Observations', fluidRow(
              column(6, plotOutput('fishY')),
              column(6, plotOutput('fishR'))
              ),
              helpText('When I initially saw the decrease in number of fish seen from 2014 to 2016 I was extremely concerned. Only 62% of the number of fish
                       seen in 2014 were seen in 2016. However, further investigation revealed there were many less observations made in 2016 than in 2014. The 
                       ratios of number of fish seen per observation each year are much closer, though still show a decrease from 2014 to 2016 (0.666 and 0.590, 
                       respectively). In the future, I would like to determine if this is a significant decrease.')),
     tabPanel('Most Common Fish',
              plotOutput('fish_by_region'),
              plotOutput('top_by_year'),
              helpText('These graphs show the number seen of the 12 most common fish in each region, and then by year. A few interesting things are seen here. 
                       In the Dry Tortugas region, the number of masked goby seen is 150,000 more than any of the other most common fish. This same trend does not 
                       occur in the other two regions, and we also see a very drastic drop in number of masked goby seen from 2014 to 2016. I assumed at first 
                       that this must have been an input error in the data, and I would perhaps find one or two observations with an extremely high number seen in 
                       comparison to the other masked goby observations. However, this was not the case and even the highest number seen for one observation seemed
                       to be recorded accurately. In further work I would like to research this more, as just the decrease in masked goby from 2014 to 2016 accounts 
                       for a very large portion of the total decrease in fish seen.')
     ),
     tabPanel('By Year Comparisons',
              plotOutput('hab_by_year'),
              plotOutput('depthR_by_year'),
              helpText('These graphs are really interesting to me. From 2014 to 2016, there was an increase in observations taken at lesser depths and a decrease in 
                       observations taken at mid-range depths. This seems to correspond with a decrease in the number of fish seen for a handful of habitat types. 
                       It is unclear from the data or assessment description why there was such a drastic change in the depths or habitat types that observations 
                       were taken at. I had thought I would possibly see an increase in fish seen at mid-range and greater depths, due to possible increases in 
                       water temperature, but it is not possible to make an accurate assesment given the change in data collection patterns.')
              )
  )
)
)
)
