library(shiny)
library(shinyWidgets)
library(shinythemes)
library(tidyverse)
library(ggthemes)
library(ggrepel)
library(plotly)
library(DT)
library(leaflet)
library(rgdal)
library(htmltools)
library(RColorBrewer)



##### Import Data and Data Manipulation
dat <- read_csv("data/gapminder1.csv")
dat <- drop_na(dat)
emissions <- read_csv("data/emissions.csv")
emissions <- drop_na(emissions)
continents <- c("Africa", "Americas", "Asia", "Europe")
worldborders <- readOGR("world_borders_shp/TM_WORLD_BORDERS-0.3.shp")
country_centroids <- read_csv("data/country_centroids_google.csv")
country_centroids <- arrange(country_centroids, name)


#### World Population Map
##### Deal with country names for country centroids
data_country_names <- unique(sort(dat$country))
country_centroids_names <- sort(country_centroids$name)
non_mathching_names_vector <- as.vector(which(is.element(data_country_names, country_centroids_names) == "FALSE"))
non_matching_names <- data_country_names[non_mathching_names_vector]
non_matching_names <- non_matching_names[-c(4, 13)]

country_centroids$name[country_centroids$name == "Congo [DRC]"] <- "Congo, Dem. Rep."
country_centroids$name[country_centroids$name == "Congo [Republic]"] <- "Congo, Rep."
country_centroids$name[country_centroids$name == "Côte d'Ivoire"] <- "Cote d'Ivoire"
country_centroids$name[country_centroids$name == "Kyrgyzstan"] <- "Kyrgyz Republic"
country_centroids$name[country_centroids$name == "Laos"] <- "Lao"
country_centroids$name[country_centroids$name == "Micronesia"] <- "Micronesia, Fed. Sts."
country_centroids$name[country_centroids$name == "Myanmar [Burma]"] <- "Myanmar"
country_centroids$name[country_centroids$name == "Macedonia [FYROM]"] <- "North Macedonia"
country_centroids$name[country_centroids$name == "Palestinian Territories"] <- "Palestine"
country_centroids$name[country_centroids$name == "São Tomé and Príncipe"] <- "Sao Tome and Principe"
country_centroids$name[country_centroids$name == "Slovakia"] <- "Slovak Republic"
country_centroids$name[country_centroids$name == "Saint Lucia"] <- "St. Lucia"
country_centroids$name[country_centroids$name == "Saint Vincent and the Grenadines"] <- "St. Vincent and the Grenadines"

final_country_centroids <- country_centroids %>%
  dplyr::select("centroid_lat" = latitude, 
                "centroid_long" = longitude,
                "country" = name)

# is.element(data_country_names, final_country_centroids$country)

map_dataset <- left_join(dat, final_country_centroids, by = "country")
map_dataset <- drop_na(map_dataset)
population_year <- c(1800, 1850, 1900, 1950, 2000, 2020)
map_dataset <- map_dataset %>%
  filter(year %in% population_year) %>%
  mutate(pop = pop/1000000)

map_dataset$label <- paste("Country:", map_dataset$country, "<br>", 
                           "Population:", map_dataset$pop, "Million")


base_map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  #setView(lng = 120.9605, lat = 23.6978, zoom = 8)
  fitBounds(-100,-60, 60,70) %>%
  addPolygons(data = worldborders,
              weight = 1,
              color = "#045A8D", 
              opacity = 0.8,
              smoothFactor = 0.2) 

#### Life Expectancy Dumbbell Chart
lifeExpdata <- dat %>%
  filter(year %in% c(1946, 2020)) %>%
  dplyr::select(country, year, lifeExp, continent)

data_wide <- spread(lifeExpdata, year, lifeExp)
names(data_wide) <- c("country", "continent", "y1946", "y2020")
dumbbell_data <- data_wide %>%
  mutate(lifeExp_diff = y2020-y1946) %>%
  dplyr::arrange(desc(lifeExp_diff)) %>%
  top_n(20, wt = lifeExp_diff)

#### GDP Per Capita Bar Plot
gdp_bar <- dat %>%
  filter(year == 2020) %>%
  dplyr::select(country, gdpPercap, continent) %>%
  group_by(continent) %>%
  arrange(desc(gdpPercap), .by_group = TRUE) %>%
  top_n(5, wt = gdpPercap) %>%
  ungroup() %>%
  mutate(gdpPercap = gdpPercap/1000)

### Customized Color Palette
africa_countries <- c("Seychelles", "Mauritius", "Equatorial Guinea", "Botswana", 
                     "Gabon")
africa_color <- c("#4CB5F5", "#B7B8B6", "#34675C", "#B3C100", "#7DA3A1")
american_countries <- c("United States", "Canada", "Trinidad and Tobago", 
                        "Bahamas", "Antigua and Barbuda")
american_color <- c("#375E97", "#FB6542", "#FFBB00", "#3F681C", "#505160")
asian_countries <- c("Qatar", "Singapore", "Brunei", "United Arab Emirates", "Kuwait")
asian_color <- c("#CF3721", "#31A9B8", "#F5BE41", "#1E656D", "#063852")
europe_countries <- c("Luxembourg", "Ireland", "Norway", "Switzerland", "Netherlands")
europe_color <- c("#257985", "#5EA8A7", "#F4EBDB", "#FF4447", "#ED8C72")
countries_combined <- c(africa_countries, american_countries, asian_countries, europe_countries)
customized_palette <-c(africa_color, american_color, asian_color, europe_color)

#### GDP Per Capita Distribution
gdp_box <- dat %>%
  filter(year == 2020) %>%
  dplyr::select(country, gdpPercap, continent) %>%
  mutate(gdpPercap = gdpPercap/1000)

##### Make your app

##### SHINY UI #####

ui <- navbarPage(
  theme = shinytheme("yeti"),
  title = "Gapminder Visualization", 
  tabPanel(
    title = "World Population Map",
    div(class = "outer", 
        leafletOutput("map", height = 1000)
    ),
    
    absolutePanel(bottom = 95,
                  left = 55, 
                  width = 250,
                  fixed = TRUE,
                  draggable = TRUE,
                  height = "auto", 
                  
                  span(tags$i(h5("Explore How World Population has Increased since 1800")), 
                       style = "color:#4D004B"),
                  h3("Total Population"),
                  h3(textOutput("world_total_population_count"), aligh = "right"),
                  sliderInput(inputId = "populationyear", 
                              label = "Select Year",
                              min = 1800,
                              max = 2020,
                              value = 1800,
                              step = 50
                  )),

  ),
  
  tabPanel(title = "Bubble Chart", 
           sidebarLayout(
             
             sidebarPanel(
               sliderInput(inputId = "year", 
                           label = "Select Year",
                           min = 1800,
                           max = 2020,
                           value = 1800,
                           step = 1,
                           animate = animationOptions(interval = 1000,
                                                      loop = FALSE)
                 
               ), 
               
               checkboxGroupInput(inputId = "choosecontinent", 
                                  label = "Which Continent Do you Want to Display?", 
                                  choices = continents,
                                  selected = continents),
               
               shinyWidgets::pickerInput(inputId = "countrytick",
                                         label = "Which Country Do you Want to Display Text Label?",
                                         choices = sort(unique(dat$country)), 
                                         options = list(
                                           "live-search" = TRUE,
                                           "selected-text-format" = "count > 3",
                                           "max-options" = 5,
                                           "max-options-text" = "No more than 5 countries.",
                                           "none-Selected-Text" = "No Countries Selected."
                                         ), 
                                         multiple = TRUE)
              
             ),
             
             mainPanel(
               plotOutput(outputId = "bubble")
             )
           ),
           
           tags$div(
             tags$h5(strong("Take a close look on Life Expectancy and GDP Per Capita"))
           ), 
           
           fluidRow(
             column(width = 5, 
                    "The Dumbbell chart on the right shows the Top 20 countries that have the largest increase in 
                    Life Expectancy after the World War II. Within 20 countries on the list, 15 of them are 
                    Asian countries. Picking China as an example, it is hard to imagine that China as a country
                    with more than 1.4 billion population size, its life expectancy has increased from 38 years 
                    old on average to 78 years old with 70 years. We cannot deny that Asia has witnessed a 
                    profound transformation in terms of the economic progress of its nations and the living 
                    conditions of its people. But, it is worth to point out that  the economic prosperity does
                    bring drawbacks as well, specifically, most of Asian countries are facing the aging 
                    population problem.",
                    tags$br(), tags$br(), 
                    "The Bar Plot shows the top 5 wealthiest countries of each continent (based on 
                    2020 data). However, to better understand the distributions of GDP Per Capita data across 
                    each continent, we should use box plot. Based on the boxplot results, Europe has the highest 
                    mean GDP per capita among all four continents, while Africa has the lowest. Moreover, the distribution 
                    of Europe is fairly symmetric as the mean is located near the center of the box. For both Asian and 
                    African countries, the distribution is right-skewd.
                    "
                    ), 
             column(width = 7,
                    tabsetPanel(
                      tabPanel(title = "Dumbbell Chart", 
                               plotOutput(outputId = "lifeExp_dumbbell")),
                      tabPanel(title = "Bar Plot", 
                               plotOutput(outputId = "gdpbar")),
                      tabPanel(title = "Box Plot", 
                               plotOutput(outputId = "gdpboxplot"))
                    ))
           )
    
  ), 
  
  tabPanel(
    title = "CO2 Emissions",
    sidebarLayout(
      
      sidebarPanel(
        shinyWidgets::pickerInput(inputId = "countryemission",
                                  label = "Select Country You Want to Display:",
                                  choices = sort(unique(emissions$country)),
                                  options = list(
                                    "actions-box" = TRUE,
                                    "live-search" = TRUE,
                                    "selected-text-format" = "count > 5",
                                    "max-options" = 8,
                                    "max-options-text" = "No more than 8 countries to show colors.",
                                    "none-Selected-Text" = "Please make a selection."
                                  ), 
                                  selected = c("United States", "China", "Russia", "United Kingdom", "France"),
                                  multiple = TRUE),
        
        sliderInput(inputId = "emissionyear", 
                    label = "Minimum year:",
                    min = 1900,
                    max = 2018,
                    value = 1900,
                    step = 10),
        
        h6("Select countries (up to 8) from drop-down menue, and plotting start date to update plots."),
        

      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel(title = "Total Emissions", plotlyOutput(outputId = "totalemissions")),
          tabPanel(title = "Emissions Per Person", plotlyOutput(outputId = "emissionsperperson"))
        ),
        
      )
    )
  ),
  
  tabPanel(
    title = "Data",
    sidebarLayout(
      
      sidebarPanel(
        shinyWidgets::pickerInput(inputId = "countrytable", 
                                  label = "Which country Do you Want to Access Data?",
                                  choices = sort(unique(dat$country)), 
                                  options = list(
                                    "actions-box" = TRUE, 
                                    "live-search" = TRUE, 
                                    "selected-text-format" = "count > 3",
                                    "none-Selected-Text" = "No Countries Selected."
                                  ), 
                                  multiple = TRUE), 
        
        sliderInput(inputId = "yeartable", 
                    label = p("Select Range of of Data you Want to Access"),
                    min = 1800, 
                    max = 2020,
                    value = c(1800, 2020))
      ), 
      
      mainPanel(
        dataTableOutput(outputId = "data1", 
                        height = 500)
      )
    )
  ), 
  
  tabPanel(
    title = "About this site",
    tags$div(
      tags$h4(strong("Last Update")),
      tags$h6(strong("Jan 24, 2021")),
      "This site is a recreation of Gapminder's project using R Shiny Application, which includes 
      World Population Map, Prof. Hans Rosling's famous bubble chart (GDP per capita vs. Life Expectancy),
      and Carbon Dioxide Emissions.", 
      tags$br(), tags$br(), tags$h4(strong("Background")),
      "There are some major challenges involving in this project. First, the data used for this project is directly
      downolad from",
      tags$a(href = "https://www.gapminder.org/data/", "the Gapminder World website."),
      "It is worth to notice that there exists a R package on Github and CRAN",
      tags$a(href = "https://github.com/jennybc/gapminder", "gapminder"),
      ", which excepts from the Gapminder data, and include data going from 1952 to 2007 in increments of 5 years.
      I didn't use this dataset because I want to expand my data range from 1800 to 2020.",
      tags$br(), tags$br(),
      "Therefore, I download the population, life Expectancy, and GDP per Capita data seprately from the Gapminder 
      website and practice some data manipulation techniques to combine those three datasets together. 
      Additionally, To match country and its continent, I also download the geographics dataset provided by the 
      Gapminder Foundation as well (which is in Excel format). I used 4 continents principle 
      (Asia, Africa, Americas, Europe) to code each country. You can filter data under the Data page, or you 
      could download original data, as well as analytically friendly data from",
      tags$a(href = "https://github.com/yifeitung/data_viz_review", "Github repo."),
      tags$br(), tags$br(),
      "Please notice that the dataset and this site exists for the purposes of education and making Shiny App
      examples. I do not update the dataset. If you want to do actual research, please refer to the Gapminder
      website and review the way it takes to estimate those statistics. All errors are my own.",
      tags$br(), tags$br(), tags$h4(strong("Code")),
      "Code and Input data used to generate this Shiny App are available on Github.",
      tags$br(), tags$br(), tags$h4(strong("Author")),
      "Yifei Dong, Columbia University"
      
  )
)
)


server <- function(input, output){
  
  df <- reactive({
    data <- dat %>% 
      filter(year == input$year & continent %in% input$choosecontinent) %>% 
      dplyr::select(-year)
  })
  
  output$bubble <- renderPlot(
    df() %>%
      dplyr::arrange(desc(pop)) %>%
      ggplot(aes(x = gdpPercap, 
                 y = lifeExp, 
                 size = pop, 
                 col = continent))+
      geom_point(alpha = 0.5)+
      geom_text_repel(data = filter(df(), country %in% input$countrytick), 
                aes(x = gdpPercap, y = lifeExp, label = country), 
                col = "gray50", 
                size = 4, 
                min.segment.length = 0,
                nudge_y = 12)+
      scale_size_continuous(range = c(0.3, 30))+
      scale_color_manual(values = c("brown1", "deeppink", "chartreuse2", "cyan3"), 
                         breaks = c("Asia", "Africa", "Americas", "Europe"))+
      scale_x_continuous("GDP per capita (log scale)", 
                         trans = "log10", 
                         breaks = c(200, 1000, 3000, 10000, 30000, 80000, 160000), 
                         limits = c(200, 200000))+
      scale_y_continuous("Life Expectancy (years)", 
                         limits = c(0, 90), 
                         breaks = seq(10, 90, by = 10))+
      theme_classic(base_family = "Avenir")+
      ggtitle(paste("GDP per Capita vs. Life Expectancy", input$year))+
      guides(size = FALSE,
             color = guide_legend(title = "Continent"))+
      theme(legend.position = "top",
            legend.title = element_text(size = 12),
            legend.text = element_text(size = 12))
  )
  
  emissions_df <- reactive({
    emissions %>% filter(year >= input$emissionyear & year <= 2018 & country %in% input$countryemission)
  })
  
  output$totalemissions <- renderPlotly({
    plot_ly(emissions_df(), x = ~as.factor(year), y = ~co2total,
            color = ~country,
            hoverinfo = "text",
            text = ~paste("Country:", country, "<br>", 
                          "Year:", year, "<br>",
                          "Emissions:", paste0(co2total/10^6, "M"))) %>%
      add_trace(type = "scatter", mode = "markers+lines") %>%
      layout(xaxis = list(title = "", showgrid = FALSE),
             yaxis = list(title = "Total CO2 Emissions (1000 tonnes)"),
             legend = list(orientation = "v"))
  })
  
  output$emissionsperperson <- renderPlotly({
    plot_ly(emissions_df(), x = ~as.factor(year), y = ~co2perperson,
            color = ~country,
            hoverinfo = "text",
            text = ~paste("Country:", country, "<br>", 
                          "Year:", year, "<br>",
                          "Emissions:", co2perperson)) %>%
      add_trace(type = "scatter", mode = "markers+lines") %>%
      layout(xaxis = list(title = "", showgrid = FALSE),
             yaxis = list(title = "CO2 Emissions (tonnes per person)"),
             legend = list(orientation = "v"))
  })
    
  output$data1 <- renderDataTable({
    filter(dat, 
           country %in% input$countrytable & year >= input$yeartable[1] & year <= input$yeartable[2])
  })
  
  map_df <- reactive({
    map_dataset %>%
      filter(year == input$populationyear)
  })
  
  output$map <- renderLeaflet({
    base_map %>%
      addCircleMarkers(lng = map_df()$centroid_long,
                       lat = map_df()$centroid_lat,
                       radius = (map_df()$pop)^(0.6),
                       weight = 1, 
                       color = "#CC4C02", 
                       label = lapply(map_df()$label, HTML), 
                       labelOptions = list(textsize = "13px"))
  })
  
  output$world_total_population_count <- renderText({
    paste(prettyNum(sum(map_df()$pop)/1000), "Billion")
  })
  
  output$lifeExp_dumbbell <- renderPlot({
    ggplot(dumbbell_data)+
      geom_segment(aes(x = y1946, xend = y2020,
                       y = reorder(country, lifeExp_diff), 
                       yend = country,
                       col = continent), size = 1.5)+
      geom_point(aes(x = y1946, y = country), size = 2, col = "#EDAE52")+
      geom_point(aes(x = y2020, y = country), size = 2, col = "#9FB059")+
      geom_text(aes(x  = y1946-0.6, y = country, label = round(y1946)), 
                col = "black", hjust = "right", fontface = "bold", 
                size = 3.5)+
      geom_text(aes(x = y2020+0.6, y = country, label = paste(round(y2020), country)), 
                col = "black", hjust = "left", fontface = "bold",
                 size = 3.5)+
      scale_x_continuous(limits = c(20, 86), breaks = seq(20, 86, 10), 
                         expand = c(0, 10))+
      scale_color_manual(values = rev(brewer.pal(4, "Dark2")))+
      labs(title = "Top 20 Countries that Have Increases in Life Expectancy", 
           subtitle = "Between 1946 and 2020", 
           caption = "Source: Gapminder Foundation")+
      theme_fivethirtyeight()+
      theme(legend.position = "top",
            axis.ticks.x = element_blank(),
            axis.text.y = element_blank(),
            axis.title = element_blank(), 
            plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
            plot.subtitle = element_text(size = 9, hjust = 0.5), 
            legend.text = element_text(size = 9), 
            panel.grid.major.y = element_blank())+
      guides(col = guide_legend(title = "Continent", size = 5))
  })
  
  output$gdpbar <- renderPlot({
    ggplot(data = gdp_bar, aes(x = reorder(country, gdpPercap), 
                               y = gdpPercap , fill = country))+
      geom_bar(stat = "identity",
               position = "dodge")+
      geom_text(aes(x = country, 
                    y = gdpPercap, 
                    label = gdpPercap),
                hjust = -0.1,
                size = 3, fontface = "bold")+
      scale_y_continuous(limits = c(0, 125), expand = c(0, 5))+
      facet_wrap(~continent, scales = "free_y")+
      coord_flip()+
      scale_fill_manual(values = customized_palette,
                        breaks = countries_combined)+
      labs(title = "Top 5 Wealthiest Countries in Each Continent, 2020",
           xlab = "GDP Per Capita (in 100 USD)",
           caption = "Source: Gapminder Foundation")+
      theme_fivethirtyeight()+
      theme(legend.position = "none", 
            axis.title.y = element_blank(),
            axis.text.y = element_text(size = 9.5, face = "bold"),
            plot.title = element_text(size = 12, hjust = 0.5)
            )
  })
  
  output$gdpboxplot <- renderPlot({
    ggplot(gdp_box, aes(x = continent, y = gdpPercap, fill = continent))+
      stat_boxplot(geom = "errorbar", width = 0.15)+
      geom_boxplot()+
      scale_fill_manual(values = c("#F8A055", "#FA6E59", "#FFDB5C", "#4897D8"))+
      labs(title = "Distribution of GDP Per Capita in 2020 by Continent",
           caption = "Source: Gapminder Foundation")+
      theme_fivethirtyeight()+
      theme(legend.position = "none",
            plot.title = element_text(size = 12, hjust = 0.5),
            axis.text.x = element_text(face = "bold"))
  })
  
}

shinyApp(ui,server)

