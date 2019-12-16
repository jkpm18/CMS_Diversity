library(ggplot2)
library(plotly)
library(RColorBrewer)
library(ggthemes)
library(maps)
library(RColorBrewer)
library(leaflet)
library(htmltools)

source("data_prep2.R")

max_count <- max(tidydistrict_counts$Count)
schools <- unique(tidydata$School)
groups <- unique(tidydata$Group)
types <- c("All", unique(tidydata$Type))
communities <- c("All", unique(tidydata$LearningCommunity))
min_year <- min(tidydata$Year)
max_year <- max(tidydata$Year)

ui <- fluidPage(
  
  tags$head(
    
    tags$style(
      
      type = "text/css",
      
      ".irs-grid-text {font-size: 12pt !important; transform: translate(0px,40px);"
      
    )),
  
  fluidRow(
    column(1),
    column(10,
           br(),
           sliderInput("year", "Select year (or click play button)",
                       min=min_year, max=max_year, value=min_year,
                       animate = animationOptions(interval = 650,
                                                  loop = FALSE),
                       step = 1,
                       sep='',
                       width="100%"),
           HTML('<br>')
           ),
    column(1)
  ),
  br(),
  br(),
  br(),
  fluidRow(
    tabsetPanel(
      tabPanel("CMS District Overview",
               sidebarLayout(
                 sidebarPanel(
                   
                   h4("CMS District Diversity"),
                   p("\n The CMS school district appears to be relatively diverse. 
                       For example, in the year 2020 no single group makes up more than 36% of the student 
                     population in the district."),
                   p("\n But do the individual schools reflect that diversity? Explore the tabs in this app
                     to draw your own conclusions."),
                   hr(),
                   radioButtons("district_method", "What would you like to display?",
                                choices = c("Counts", "Percentages")),
                   hr(),
                   p("\n NOTE: At any point you can select a different year on the slider at the top of 
                     the screen, or click the play button to cycle through all of the years."),
                   hr(),
                   img(src = "CMS_logo_blue.png", width="45%", height="45%", alt = "CMS Logo")
                 ),
                 mainPanel(
                   fluidRow(plotOutput("district", height = "600px", width = "600px")),
                   fluidRow(
                     column(2),
                     column(10,
                        br(),
                        p('\n All race/ethnicity category names are presented here as defined by CMS.'),
                        p('(For example, "Indian" in the CMS data refers to "American Indian")')
                     )
                   )
                   
                 )
               )
      ),
      tabPanel("Map",
               sidebarLayout(
                 sidebarPanel(  
                   h4("Map of All CMS Schools"),
                   p("Select one or more race/ethnicity from the dropdown menu below"),
                   hr(),
                   selectInput("Race", "Race/Ethnicity (select one or more)", choices = groups,
                               selected = NULL,
                               multiple = TRUE),
                   p("NOTE: You can also delete selections from the dropdown menu above using the backspace key"),
                   hr(),
                   p("ADDITIONAL FILTERS:"),
                   selectInput("Type", "School Grade Levels (select one)", choices = types,
                               selected = "All"),
                   selectInput("Community", "Learning Communities (select one)", choices = communities,
                               selected = "All"),
                   # p("ADDITIONAL INSTRUCTIONS: "),
                   # HTML('<ul>
                   #      <li></li>
                   #      <li>Hover mouse over the center of a dot on the map to see the School and Percentage</li>
                   #      </ul>'),
                   hr(),
                   img(src = "CMS_logo_blue.png", width="40%", height="40%", alt = "CMS Logo")
                 ),
                 mainPanel(
                   fluidRow(
                     column(10,
                            HTML('<br>'),
                            textOutput("shading")
                     ),
                     column(2)
                   ),
                   fluidRow(leafletOutput("map", height = "650px", width = "650px"))
                 )
               )
      ),
      tabPanel("Individual Schools",
               sidebarLayout(
                 sidebarPanel(
                   h4("Individual School Diversity"),
                   hr(),
                   selectInput("school", "Select the school", choices = schools),
                   checkboxInput("district_overlay", "Show district percentages", value=FALSE),
                   hr(),
                   p("NOTE: Some of these schools did not exist during all years."),
                   hr(),
                   img(src = "CMS_logo_blue.png", width="40%", height="40%", alt = "CMS Logo")
                 ),
                 mainPanel(
                   plotOutput("barchart", height = "600px", width = "600px")
                 )
               )
      ),
      tabPanel("Schools Resembling District",
        sidebarLayout(
          sidebarPanel(
            h4("Schools Similar to District Makeup"),
            p("With respect to the three largest groups in the district (Black, White, and Hispanic), 
              do any individual schools have percentages similar to that of the district 
              as a whole?"),
            hr(),
            radioButtons("level", "Specificy Level of Resemblance:",
                         choices = c("Very Closely Resembles District (within 10%)", 
                                     "Closely Resembles District (within 20%)", 
                                     "Somewhat Resembles District (within 30%)")),
            hr(),
            h5("CMS District Breakdown"),
            tableOutput("district_table"),
            hr(),
            img(src = "CMS_logo_blue.png", width="40%", height="40%", alt = "CMS Logo")
          ),
          mainPanel(
            fluidRow(
              column(11,
                hr(),
                p("This is a table of all schools whose population percentage of Black, White,
              and Hispanic students only differs from the districts percentage by the level you specified."),
                em('\nFor example, if in a given year the district was 40% Black, 30% White, and 20% Hispanic, 
                  then at a 10% ("Very Closely Resembles District") level the table will only contain schools that 
                  are 36-44% Black and 27-33% White and 18-22% Hispanic. And the 
                   20% ("Closely Resembles District") level would increase the width of those ranges.'),
                hr(),
                h4("LIST OF SCHOOLS")
              ),
              column(1)
            ),
            fluidRow(
              column(11, DT::DTOutput("table")),
              column(1)
            )
            
          )
        )
      ),
      tabPanel("About",
        br(),
        column(1),
        column(8, 
               h5('This app was developed by Chase Romano, Kirk Mason, and Nityanand Kore.'),
               p("It was the result of Ryan Wesslen's Visual Analytics course at the University of 
                 North Carolina Charlotte through the Data Science and Business Analytics MS program."),
               br(),
               HTML('<a href="https://github.com/jkpm18/CMS_Diversity" style="color: #e36209">View Code on GitHub</a>')
               ),
        column(3)
        
      )
    )
  )
)





server <- function(input, output) {
  
  selected_year <- reactive({ input$year })
  
  
  # ------------------------------ CMS DISTRICT Bar Chart ------------------------------
  
  data_district <- reactive({
    if(input$district_method == "Percentages") {
      tidydistrict_prcnts %>% filter(Year == selected_year())
    } else if(input$district_method == "Counts") {
      tidydistrict_counts %>% filter(Year == selected_year())
    }
  })
  
  gg_district <- reactive({
    if(input$district_method == "Percentages") {
      gg <- tidydistrict_prcnts %>% 
        filter(Year == selected_year()) %>%
        ggplot(aes(x = factor(Group, levels = rev(levels(Group))),
                   y = Percent_of_District / 100)) +
        labs(title = "CMS School District Breakdown",
             subtitle = "Percent of District's Student Population",
             y='Percentage of All Students in the District', x ='') +
        scale_y_continuous(limits = c(0, 1),
                           breaks = seq(0,1,by = .10),
                           labels = scales::percent_format(accuracy = 1))
    } else if(input$district_method == "Counts") {
      gg <- tidydistrict_counts %>% 
        filter(Year == selected_year()) %>%
        ggplot(aes(x = factor(Group, levels = rev(levels(Group))),
                   y = Count)) +
        labs(title = "CMS School District Breakdown",
             subtitle = "Number of Students in the District",
             y='Number of Students', x ='') +
        scale_y_continuous(limits = c(0, max_count))
    }
    
    gg <- gg + 
      scale_color_brewer(palette="Dark2") +
      scale_fill_brewer(palette="Dark2")
    
    return(gg)
  })
  
  output$district <- renderPlot({
    
    p <- gg_district() +
      geom_bar(stat="identity",
               aes(color = Group, fill = Group),
               show.legend = FALSE) +
      theme_bw() +
      theme(title = element_text(size = 17),
            axis.text.x = element_text(size = 15),
            axis.text.y = element_text(size = 15, face = "bold")) +
      coord_flip()
    
    p
  })
  
  
  # ------------------------------------- MAP -------------------------------------
  
  df_groups <- reactive({
    tidydata %>% filter(Group %in% input$Race)
  })
  
  df_groups_year <- reactive({
    
    if(input$Type == "All"){
      df <- df_groups() 
    } else {
      df <- df_groups() %>% 
        filter(Type==input$Type)
    }
    
    if(input$Community == "All") {
      df <- df
    } else {
      df <- df %>%
        filter(LearningCommunity==input$Community)
    }
    
    df <- df %>% 
      filter(Year == selected_year()) %>%
      group_by(School, Year, lat, lon) %>%
      summarise(Percentage = sum(Percentage)) %>%
      ungroup()
    
    return(df)
  })

  
  output$shading <- renderText({
    text <- paste0("The shading of each point on the map is ", 
                   "currently conveying the percent of students in each school in ", 
                   selected_year(),
                   " who were _________.")
    
    if(!is.null(input$Race)) {
      text <- substr(text, 0, nchar(text)-10)
      
      for (race in input$Race){
        text <- paste0(text, race, " or ")
      }
      text <- substr(text, 0, nchar(text)-4)
      text <- paste0(text, ".")
    }
     return(text)
  })
 
  
  # IMPROVED LEAFLET VERSION OF THE MAP...
  mypal <- reactive({
    colorNumeric(palette = c("navy", "lightcoral"), domain = df_groups_year()$Percentage, reverse = TRUE)
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      fitBounds(min(tidydata$lon), min(tidydata$lat), max(tidydata$lon), max(tidydata$lat)) 
  })
  
  observe({
    
    df <- df_groups_year()
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g%%",
      df$School, df$Percentage
    ) %>% lapply(htmltools::HTML)
    
    if(nrow(df) == 0){
      leafletProxy("map", data = df) %>%
        clearMarkers()
    }
    
    else{
      leafletProxy("map", data = df) %>%
        clearMarkers() %>%
        clearPopups()%>%
        removeControl("legend") %>%
        addCircleMarkers(radius = 8, 
                         weight = 1, 
                         color = ~mypal()(df$Percentage),
                         stroke = FALSE, 
                         fillOpacity = 0.95,
                         label =labels,
                         labelOptions = labelOptions(noHide = F, offset=c(0,-12)))  %>%
        addLegend("bottomleft",
                  title = "Percentage",
                  pal = mypal(),
                  values = df$Percentage,
                  layerId = "legend",
                  opacity = 0.90)
    }
  })
  
  
  
  # OLD LEAFLET VERSION OF THE MAP...
  
  # mypal <- reactive({
  #   colorNumeric(palette = c("navy", "lightcoral"), domain = df_groups_year()$Percentage, reverse = TRUE)
  # })
  # 
  # output$map <- renderLeaflet({
  #   leaflet() %>%
  #     addTiles() %>%
  #     fitBounds(min(tidydata$lon), min(tidydata$lat), max(tidydata$lon), max(tidydata$lat))
  # })
  # 
  # observe({
  #   df <- df_groups_year()
  # 
  #   if(nrow(df) == 0){
  #     leafletProxy("map", data = df) %>%
  #       clearMarkers()
  #   }
  #   else{
  #     leafletProxy("map", data = df) %>%
  #       clearMarkers() %>%
  #       removeControl("legend") %>%     
  #       addCircleMarkers(radius = 8, weight = 2,
  #                        color = ~mypal()(df$Percentage),stroke = FALSE, fillOpacity = 0.95
  #       ) %>%
  #       addLegend("bottomleft",title = "Percentage",pal = mypal(),
  #                 values = df$Percentage,
  #                 opacity = 1,
  #                 layerId = "legend") %>%
  #       addLabelOnlyMarkers(data = df,
  #                           lng = ~lon, lat = ~lat,label = (paste("School:",df$School," & Percentage:",df$Percentage)))#,
  #     #labelOptions = labelOptions(noHide = FALSE, direction = 'top', textOnly = TRUE))
  #   }
  # })
  
  
  # ORIGINAL VERSION OF THE MAP...
  
  # output$map <- renderPlotly({
  #   df <- df_groups_year()
  #   max_per <- max(df_groups()$Percentage)
  #   min_per <- min(df_groups()$Percentage)
  # 
  #   #myPalette <- colorRampPalette(rev(brewer.pal(8, "Dark2")))
  #   sc <- scale_colour_gradient(low = "white", high = "forestgreen", limits=c(min_per, max_per))
  # 
  #   mecklenburg <- ggplot(data=df) +
  #     borders("county","North Carolina",xlim = c(-80.9, -80.8),
  #             ylim = c(35.2, 35.4), colour = "gray85", fill = "gray80") +
  #     theme_map()
  # 
  # 
  #   map <- mecklenburg +
  #     geom_point(aes(x = lon, y = lat, group= School,colour = Percentage),
  #                data = df,
  #                size = 2)+
  #     #scale_colour_gradient(low = "blue", high="white",limits=c(0,.9))+
  #     labs(title = "",
  #          x='', y ='') +
  #     sc +
  #     theme(title = element_text(size = 10))
  # 
  #   ggplotly(map, tooltip = c("School", "Percentage"))
  # 
  # })
  
  
  # ------------------------- INDIVIDUAL SCHOOLS Bar Chart --------------------------
  
  df_school_year <- reactive({
    tidydata %>% filter(School == input$school, Year == selected_year())
  })
  
  output$barchart <- renderPlot({
    df <- df_school_year()
    
    p <- ggplot(data = df,
                aes(x = factor(Group, levels = rev(levels(Group))),
                    y = Percentage / 100)) +
      geom_bar(stat="identity",
               aes(color = Group, fill = Group),
               show.legend = FALSE) +
      labs(title = input$school,
           subtitle = "Percent of School's Student Population",
           x='', y ='') +
      scale_y_continuous(limits = c(0, 1),
                         breaks = seq(0,1,by = .10),
                         labels = scales::percent_format(accuracy = 1)) +
      theme_bw() +
      theme(title = element_text(size = 17),
            axis.text.x = element_text(size = 15),
            axis.text.y = element_text(size = 15, face = "bold")) +
      coord_flip() 
    
    if (input$district_overlay) {
      p <- p +
        geom_point(data = tidydistrict_prcnts %>% 
                     filter(Year == selected_year()), 
                   aes(x = factor(Group, levels = rev(levels(Group))),
                       y = Percent_of_District / 100),
                   color="gray",
                   fill="blue",
                   shape=25,
                   size =4,
                   stroke = 2,
                   alpha=.75)
    }
    
    if (nrow(df) == 0){
      p <- p +
        labs(caption = "School did not exist in this year.")
    }
    
    p <- p + 
      scale_color_brewer(palette="Dark2") +
      scale_fill_brewer(palette="Dark2")
    
    p
  })
  
  # ------------------------- SCHOOLS THAT MATCH DISTRICT --------------------------
  
  
  # Sidebar Table containing district percentages
  
  df_district <- reactive({
    tidydistrict_prcnts %>%
      filter(Year == selected_year()) %>%
      mutate(`% of District` = Percent_of_District) %>%
      select(Year, Group, `% of District`)
  })
  
  output$district_table <- renderTable({
    df_district()
  })
  
  # Create a reactive dataframe that contains just the schools that mirror the district
  #  makeup to the degree specified in the radio button
  
  upper_bound <- reactive({
    if(input$level == "Very Closely Resembles District (within 10%)") {
      upper_bound <- 1.10
    } else if(input$level == "Closely Resembles District (within 20%)") {
      upper_bound <- 1.20
    } else {
      upper_bound <- 1.30
    }
  })
  lower_bound <- reactive({
    if(input$level == "Very Closely Resembles District (within 10%)") {
      lower_bound <- 0.90
    } else if(input$level == "Closely Resembles District (within 20%)") {
      lower_bound <- 0.80
    } else {
      lower_bound <- 0.70
    }
  })
  
  data_matching_schools <- reactive({
    
    data_district_added %>%
      filter(Year == selected_year()) %>%
      filter( (Hispanic_Prcnt <= upper_bound() * district_prcnt_Hispanic & 
                Hispanic_Prcnt >= lower_bound() * district_prcnt_Hispanic) &
               (Black_Prcnt <= upper_bound() * district_prcnt_Black & 
                  Black_Prcnt >= lower_bound() * district_prcnt_Black) &
               (White_Prcnt <= upper_bound() * district_prcnt_White & 
                  White_Prcnt >= lower_bound() * district_prcnt_White) ) %>%
      mutate(`Black %` = Black_Prcnt,
             `White %` = White_Prcnt,
             `Hispanic %` = Hispanic_Prcnt,
             `Asian %` = Asian_Prcnt,
             `Indian %` = Indian_Prcnt,
             `Pac Islander %` = Pacific_Prcnt,
             `Two or More %` = Mixed_Prcnt) %>%
      select(Year,
             School,
             `Black %`,
             `White %`,
             `Hispanic %`,
             `Asian %`,
             `Indian %`,
             `Pac Islander %`,
             `Two or More %`
             )
      
    
  })
  
  output$table <- DT::renderDT({
    DT::datatable(data_matching_schools(),
                  options = list(pageLength = 25))
  })
  
} # END SERVER FUNCTION


shinyApp(ui = ui, server = server)


