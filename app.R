library(shiny)
library(ggplot2)
library(tidyverse)
library(sf)
library(mapview)
library(dplyr)
library(shinydashboard)
library(bslib)
library(DT)
library(leaflet)
library(plotly)


# page1 - supporting rate
# including general supporting rate and supporting rate by states
ui_page1 <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "minty"),
  titlePanel("Introduction"),
  
  # the absolute panel to input an image
  absolutePanel(
    top = "200px", # Adjust this to change the top position
    right = "20px", # Adjust this to change the right position
    shiny::imageOutput("myImage"),
  ),
  
  
  # set the fluid row to contain text information
  fluidRow(
    tags$style(HTML(".my-fluid-row { height: 140px; }")),
    # make the fluid row first column to store content
    column(
      width = 6,
      height = 2,
      textOutput("page1_content"),
      
    ),
    column(
      width = 5,
      height = 2,



    ),
    class = "my-fluid-row"
    
    
  ),

  
 
  # set the side bar that shows the select input and table
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Choose governors information or president 
      selectInput("candidate_choose", "Choose what to check:",
                  choices = c("president election", "governors election")),
      
      sliderInput("select_candidate", "Select The Number of Top Candidates", min = 1, max = 15, value = 5),
      tags$head(
        tags$style(
          HTML(".dataTables_length, .dataTables_filter, .dataTables_info { display: none; }"),
          
        )
      ),
      dataTableOutput("table2")
      
    ),mainPanel(
      # show the bar chart
      plotOutput("barPlot"),
      tags$style(HTML('
        #barPlot {
          position: relative;
          top: 120px; /* the distance to move downward */
          left: 7px; /* the distance away from the left */
        }
      '))
      
    )
    
    
  ),
  
  
)


# input the us states boundaries data from the sf package
us_states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
us_states <- st_transform(us_states, crs = "+proj=longlat +datum=WGS84")

# page2 - supporting states for each candidate
# include general parties and the supporting rate for each party
ui_page2 <- fluidPage(
  titlePanel(HTML("Supporting States &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; click on different state to check information")),
  sidebarLayout(
    sidebarPanel(
      selectInput("electionType", "Choose Election Type:", choices = c("president election", "governors election")),
      # Add label for the colors
      uiOutput("colorLegend")
    ),
    mainPanel(
      # show the map
      leafletOutput("usMap"),
      
      
      
    )
  ),

  # show the text information
  absolutePanel(
    top = "630px", # Adjust this to change the top position
    left = "30px",
    right = "450px", # Adjust this to change the right position
    titlePanel("Intoduction on supporting states"),
    uiOutput("dynamic_text") # show information about president election or governors election based on select input
  ),
  
  # show the pie chart
  absolutePanel(
    top = "630px", # Adjust this to change the top position
    right = "50px", # Adjust this to change the right position
    plotlyOutput("pieChart")
  )

)


# page3 - party information
ui_page3 <- fluidPage(
  titlePanel("Party"),
  sidebarLayout(
    sidebarPanel(
      selectInput("electionTypePage3", "Choose Election Type:", choices = c("president election", "governors election")),
      # Add label for the colors
      uiOutput("colorLegendPage3")
    ),
    mainPanel(
      # show the map and pie chart
      leafletOutput("usMapPage3"),
      plotlyOutput("pieChartPage3")
      
      
    ),
    
    
  ),
  absolutePanel(
    top = "630px", # Adjust this to change the top position
    left = "30px",
    right = "450px", # Adjust this to change the right position
    titlePanel("Intoduction on supporting parties"),
    # show information about president election or governors election based on select input
    uiOutput("dynamic_text_page3")
  ),


)


# page4 - Download files and view tables
ui_page4 <- fluidPage(
  titlePanel("Data View and Download"),
  hr(),
  titlePanel("Load table needs to take a while..."),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Choose data set ----
      selectInput("dataset", "Choose a dataset:",
                  choices = c("president election", "state coordinate", "governors election")),
      
      # Button
      downloadButton("downloadData", "Download")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # show the table of data
      tableOutput("table")
      
    )
    
  )
)


# start the shiny app
ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "lumen"),
  tags$style(HTML(
    "
    body {
      background-color: lightyellow; 
    }
    "
  )),
  h1("The USA President & Governors Election 2020"),
  hr(),
  hr(),
  tabsetPanel(
    tabPanel("Introduction", ui_page1),
    tabPanel("Supporting States", ui_page2),
    tabPanel("Party", ui_page3),
    tabPanel("Data View and Download", ui_page4)
  )
)

# server
server <- function(input, output, session) {

  
  # read the csv file of 2020 USA president election and corresponding information 
  election <- read.csv("president_county_candidate.csv")  
  
  # read the csv file of USA state and their corresponding longitude and latitude 
  state <- read.csv("us_states_coordinate.csv")  
  
  # read the csv of USA governors election information
  governors <- read.csv("governors_county_candidate.csv")
  
  # use join function in dplyr to join the president election and state coordinate
  merged_data <- left_join(election, state, by = "state")
  
  # use join function in dplyr to join the governors election and state coordinate
  merged_data2 <- left_join(governors, state, by = "state")
  
  # calculate the total votes for president candidate
  total_votes <- merged_data %>%
    group_by(candidate) %>%
    summarise(TotalVotes = sum(votes)) %>%
    arrange(desc(TotalVotes))

  
  # calculate the votes for president candidate in each state
  region_votes <- merged_data %>%
    group_by(state, candidate) %>%
    summarise(TotalVotes = sum(votes))
  
  
  # calculate the total votes for governor candidates
  total_votes2 <- merged_data2 %>%
    group_by(candidate) %>%
    summarise(TotalVotes2 = sum(votes)) %>%
    arrange(desc(TotalVotes2))
  
  
  # calculate the votes for governor candidates in each state
  region_votes2 <- merged_data2 %>%
    group_by(state, candidate) %>%
    summarise(TotalVotes2 = sum(votes))
  
  
  selected_candidates <- reactiveVal(NULL)
  selected_data <- reactiveVal(NULL)
  TotalVotes <- reactiveVal(NULL)

  
  # detect the select input of page1
  observeEvent(c(input$candidate_choose, input$select_candidate), {
    operation <- input$candidate_choose
    if(operation == "president election"){
      selected_candidates(head(total_votes, input$select_candidate))
      selected_data(merged_data %>%
        filter(candidate %in% selected_candidates()$candidate) %>%
        group_by(candidate) %>%
        summarise(TotalVotes = sum(votes)) %>%
        arrange(desc(TotalVotes)) %>%  # do the decreasing order based on TotalVotes
        select(candidate, TotalVotes))  # choose only candidate and totalvotes
      

    }else if(operation == "governors election"){
      selected_candidates(head(total_votes2, input$select_candidate))
      selected_data(merged_data2 %>%
        filter(candidate %in% selected_candidates()$candidate) %>%
        group_by(candidate) %>%
        summarise(TotalVotes = sum(votes)) %>%
        arrange(desc(TotalVotes)) %>%  # do the decreasing order based on TotalVotes
        select(candidate, TotalVotes))  # choose only candidate and totalvotes

    }
    
    
    
    
  })
  
  
  # set the data in the table of page1
  output$table2 <- renderDataTable({
    selected_data_df <- as.data.frame(selected_data())
    datatable(selected_data_df, options = list(lengthMenu = list(-1), pageLength = 5, searching = FALSE))
    
  })
  
  # set the data of the bar chart in page1
  output$barPlot <- renderPlot({
    selected_data_df <- as.data.frame(selected_data())

    
    gg <- ggplot(selected_data_df, aes(x = reorder(candidate, -TotalVotes), y = TotalVotes, fill = candidate)) +
      geom_bar(stat = "identity", alpha = 0.7) +
      geom_text(aes(label = scales::label_number_si()(TotalVotes)), vjust = -0.5, size = 4) +
      labs(
        title = "Total votes vs Candidates",
        x = "Candidates",
        y = "Total Votes"
      ) +
      theme_minimal() +
      theme(plot.background = element_rect(fill = "lightyellow", color = "transparent"),
            panel.background = element_rect(fill = "lightyellow", color = "transparent"),
            panel.border = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1),
            text = element_text(size = 15, color = "black"),
            plot.title = element_text(size = 16, hjust = 0.5),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14)) +
      guides(fill = FALSE)
    
    
    print(gg)
    return(gg)
  })
  
  
  # Reactive value for selected dataset ---- used in page 4
  datasetInput <- reactive({
    switch(input$dataset,
           "president election" = election,
           "state coordinate" = state,
           "governors election" = governors
           )
  })
  
  # draw the Table of selected data set ---- used in page 4
  output$table <- renderTable({
    datasetInput()
  })
  
  # Downloadable csv of selected data set ---- used in page 4
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  
  
  # set the candidate winner data that will be used in pie chart in page2
  global_winner_data <- NULL

  
  #page2 drawing map
  observe({
    if(input$electionType == "president election"){
      data_to_use <- merged_data
    } else {
      data_to_use <- merged_data2
    }
    
    # Ensure the 'state' column in us_states is in lowercase
    valid_states <- tolower(us_states$ID)
    
    # calculate the winner data of the candidate
    winner_data <- data_to_use %>%
      mutate(state = tolower(state)) %>%
      filter(state %in% valid_states) %>%   # Filter only states present in us_states
      group_by(state, candidate) %>%
      summarise(votes = sum(votes)) %>%
      arrange(state, -votes) %>%
      slice(1) %>%
      ungroup()
    
    global_winner_data <<- winner_data
    
    # calculate the information that will be poped up in the map
    tooltip_data <- data_to_use %>%
      mutate(state = tolower(state)) %>%
      filter(state %in% valid_states) %>%   # Filter only states present in us_states
      group_by(state, candidate) %>%
      summarise(votes = sum(votes)) %>%
      arrange(state, -votes) %>%
      slice(1:4) %>%
      group_by(state) %>%
      summarise(tooltip_text = paste(candidate, ": ", votes, "votes", collapse = "<br>")) %>%
      ungroup()
    
    
    color_vector <- c("red", "blue",  "darkgoldenrod", "yellow","purple", "darkblue", "pink","black", "darkorange", "darkred","darkturquoise")
    candidates <- unique(winner_data$candidate)
    

    default_color <- "grey"
    if(length(candidates) > length(color_vector)){
      for(i in (length(color_vector)+1):length(candidates)) {
        color_vector <- c(color_vector, default_color)
      }
    }
    pal <- colorFactor(color_vector[1:length(candidates)], domain = candidates)
    
    
    # draw the map
    output$usMap <- renderLeaflet({
      leaflet(data = us_states) %>%
        addTiles() %>%
        addPolygons(data = us_states, 
                    fillColor = ~pal(winner_data$candidate[match(tolower(as.character(us_states$ID)), winner_data$state)]),
                    fillOpacity = 0.5,
                    color = "white", 
                    highlightOptions = highlightOptions(color = "white", weight = 2,
                                                        bringToFront = TRUE, sendToBack = TRUE),
                    popup = ~tooltip_data$tooltip_text[match(tolower(as.character(us_states$ID)), tooltip_data$state)]) %>%
        setView(lng = -96.8, lat = 39.1, zoom = 4)
    })
    
    # draw the color legend
    output$colorLegend <- renderUI({
      candidates <- unique(winner_data$candidate)
      color_vector <- c("red", "blue",  "darkgoldenrod", "yellow","purple", "darkblue", "pink","black", "darkorange", "darkred","darkturquoise")
      default_color <- "grey"
      if(length(candidates) > length(color_vector)){
        for(i in (length(color_vector)+1):length(candidates)) {
          color_vector <- c(color_vector, default_color)
        }
      }
      
      # sort candidates based on alphabetical order
      sorted_candidates <- sort(candidates)
      
      # 创建已排序的 legend_colors
      legend_colors <- color_vector[1:length(sorted_candidates)]
      
      legend_list <- purrr::map2(sorted_candidates, legend_colors, function(candidate, color) {
        tags$div(
          tags$span(style = paste("background-color:", color, "; opacity: 0.5; padding: 10px; display: inline-block; margin-right: 5px;"), " "),
          tags$span(candidate)
        )
      })
      
      do.call(tagList, legend_list)
    })
    
    

    
  })
  
  #page2 pie chart
  output$pieChart <- renderPlotly({
    if (input$electionType == "president election") {
      top_candidates <- head(unique(global_winner_data$candidate), 2)
    } else {
      top_candidates <- head(unique(global_winner_data$candidate), 11)
    }
    
    # get the data that will be used from global winner data
    pie_data <- global_winner_data %>%
      filter(candidate %in% top_candidates) %>%
      group_by(candidate) %>%
      summarise(votes = sum(votes))
    
    # set the color vectors for the candidates
    candidate_colors <- c("red", "blue", "darkgoldenrod", "yellow", "purple", "darkblue", "pink", "black", "darkorange", "darkred", "darkturquoise")
    names(candidate_colors) <- sort(unique(pie_data$candidate))
    
    # set color for each candidate
    pie_data$color <- scales::alpha(candidate_colors[pie_data$candidate],alpha = 0.5)
    
    # draw the pie chart
    pie_chart <- plot_ly(data = pie_data, labels = ~candidate, values = ~votes, type = "pie", marker = list(colors = ~color), width = 400, height = 330) %>%
      layout(
        title = NULL,
        margin = list(l = 20, r = 50, b = 50, t = 20),
        paper_bgcolor = "lightyellow",
        showlegend = FALSE,
        legend = list(
          x = 0.8,
          y = 0.5,
          bgcolor = "lightyellow",
          bordercolor = "black",
          borderwidth = 1,
          traceorder = "normal",
          itemsizing = "constant",
          itemwidth = 15
        )
      ) %>%
      config(
        displayModeBar = FALSE
      )
    
    return(pie_chart)
  })
  
  
  
  
  
  # page 3 map and graph
  observe({
    if(input$electionTypePage3 == "president election"){
      data_to_use <- merged_data
    } else {
      data_to_use <- merged_data2
    }
    
    valid_states <- tolower(us_states$ID)
    
    # get the winner data depends president election or governors election
    winner_party_data <- data_to_use %>%
      mutate(state = tolower(state)) %>%
      filter(state %in% valid_states) %>%
      group_by(state, party) %>%
      summarise(votes = sum(votes)) %>%
      arrange(state, -votes) %>%
      slice(1) %>%
      ungroup()
    
    # calculate the information that will be poped up in the map
    tooltip_data_party <- data_to_use %>%
      mutate(state = tolower(state)) %>%
      filter(state %in% valid_states) %>%
      group_by(state, party) %>%
      summarise(votes = sum(votes)) %>%
      arrange(state, -votes) %>%
      slice(1:4) %>%
      group_by(state) %>%
      summarise(tooltip_text = paste(party, ": ", votes, "votes", collapse = "<br>")) %>%
      ungroup()
    
    # calculate the data of party votes
    party_votes_data <- data_to_use %>%
      group_by(party) %>%
      summarise(total_votes = sum(votes)) %>%
      arrange(desc(total_votes))
    
    
    parties <- unique(winner_party_data$party)
    color_vector <- c("red", "blue", "green", "yellow", "purple", "pink", "orange", "brown")
    pal <- colorFactor(color_vector[1:length(parties)], domain = parties)
    
    output$usMapPage3 <- renderLeaflet({
      leaflet(data = us_states) %>%
        addTiles() %>%
        addPolygons(data = us_states,
                    fillColor = ~pal(winner_party_data$party[match(tolower(as.character(us_states$ID)), winner_party_data$state)]),
                    fillOpacity = 0.5,
                    color = "white",
                    highlightOptions = highlightOptions(color = "white", weight = 2,
                                                        bringToFront = TRUE, sendToBack = TRUE),
                    popup = ~tooltip_data_party$tooltip_text[match(tolower(as.character(us_states$ID)), tooltip_data_party$state)]) %>%
        setView(lng = -96.8, lat = 39.1, zoom = 4)
    })
    
    # draw the legend
    output$colorLegendPage3 <- renderUI({
      sorted_parties <- sort(parties)
      legend_colors <- color_vector[1:length(sorted_parties)]
      
      legend_list3 <- purrr::map2(sorted_parties, legend_colors, function(party, color) {
        tags$div(
          tags$span(style = paste("background-color:", color, "; opacity: 0.5; padding: 10px; display: inline-block; margin-right: 5px;"), " "),
          tags$span(party)
        )
      })
      
      do.call(tagList, legend_list3)
    })
    
    predefined_colors <- c("red", "blue", "darkgoldenrod", "yellow", "purple", "darkblue", "pink", "black", "darkorange", "darkred", "darkturquoise")
    
    # to make sure dem and rep always have the constant color
    color_map <- c("DEM" = "red", "REP" = "blue")
    
    # use other colors for the rest of parties
    remaining_parties <- setdiff(unique(party_votes_data$party), names(color_map))
    remaining_colors <- setdiff(predefined_colors, color_map)
    
    # set the color map
    color_map <- c(color_map, setNames(remaining_colors[1:length(remaining_parties)], remaining_parties))
    
    # use the color map to make pie chart
    output$pieChartPage3 <- renderPlotly({
      top_10_parties <- party_votes_data %>%
        arrange(desc(total_votes)) %>%
        head(10)
      top_10_parties$color <- scales::alpha(color_map[top_10_parties$party], alpha = 0.5)
      plot_ly(data = top_10_parties, labels = ~party, values = ~total_votes, type = "pie", marker = list(colors = ~color), width = 500, height = 300) %>%
        layout(
          title = NULL,
          legend = list(
            x = 1,
            y = 1,
            orientation = "v",
            xanchor = "right",
            bgcolor = "lightyellow",
            bordercolor = "black",
            borderwidth = 1,
            traceorder = "normal",
            itemsizing = "constant",
            itemwidth = 15
          ),
          paper_bgcolor = "lightyellow",
          showlegend = TRUE
        ) %>%
        config(
          displayModeBar = FALSE
        )
    })
    
    

  })
  
    
  
  
  
  # set the text information that will be shown in page 1
  output$page1_content <- renderText({
    "The 2020 US presidential election is a historic election, pitting Democratic candidate Joe Biden against current President Donald Trump. The election process has been affected by the pandemic, including the widespread use of mail-in voting and early voting. In the end, Biden won and became the 46th President of the United States, marking a change of power."
  })
  
  
  # set the text information that will be shown in page 2 depending on president election or governors election
  output$dynamic_text <- renderUI({
    if (input$electionType == "president election") {
      div(
        tags$p("In the 2020 United States presidential election, Joe Biden and Donald Trump were the main candidates."),
        tags$p("Joe Biden, running as a Democrat, won 25 states along with the District of Columbia.Donald Trump, running as a Republican, secured 25 states."),
        tags$p("In terms of the popular vote, Joe Biden received over 53 million votes in the state where he received the most votes. Donald Trump received over 35 million votes in the state where he received the most votes.")
      )
    } else if (input$electionType == "governors election") {
      div(
        tags$p("In the 2020 United States governors elections, 11 states and 2 territories went to the polls to elect their governors, so there are only 11 states been highlight in this page"),
        tags$p("1.Roy Cooper of North Carolina State (Democrat) received over 2.83 million votes, winning a second term in office."),
        tags$p("2.Jay Inslee of Washington State (Democrat) won with approximately 2.29 million votes, earning his third term as governor."),
        tags$p("3.Mike Parson of Missouri State (Republican) successfully won re-election, defeating his Democratic challenger Nicole Galloway.")
      )
    }
  })
  
  
  # set the text information that will be shown in page 3 depending on president election or governors election
  output$dynamic_text_page3 <- renderUI({
    if (input$electionTypePage3 == "president election") {
      div(
        tags$p("In the 2020 United States presidential election, the major parties were the Democratic Party and the Republican Party. Joe Biden, the Democratic candidate, received approximately 51.4% of the popular vote. Donald Trump, the incumbent Republican President, received about 46.7% of the popular vote."),
        tags$p("Third parties and independent candidates also participated, but their impact was minimal in comparison to the two major parties. The Libertarian Party candidate, Jo Jorgensen, received about 1.9 million votes, making up around 1.17% of the popular vote. The Green Party candidate, Howie Hawkins, garnered significantly fewer.")

      )
    } else if (input$electionTypePage3 == "governors election") {
      div(
        tags$p("The Democratic Party managed to secure some key victories (44.1% of total votes), including in states like North Carolina, where incumbent Roy Cooper was re-elected. However, the Republicans maintained a stronghold in several states, particularly in conservative-leaning areas.(52.4% of total votes)"),
        tags$p("While third parties did participate in the gubernatorial elections, their impact remained limited. The focus of the elections largely remained on key battleground states where the balance of power was expected to influence not just state but also federal politics."),
        
      )
    }
  })
  
  
  # set the image that will be shown in page1
  output$myImage <- renderImage({
    list(src = "elections.jpeg",
         width = 430, height = 220)
  }, deleteFile = FALSE)
  
}

# Run the application 
shinyApp(ui = ui, server = server)


#plot tutorial
#https://plotly-r.com/bars-histograms

#shiny control tutorial
#https://shiny.posit.co/r/getstarted/shiny-basics/lesson3/
#https://mastering-shiny.org/action-workflow.html
#https://rstudio.github.io/cheatsheets/html/shiny.html

#map set up tutorial
#https://www.storybench.org/plot-state-state-data-map-u-s-r/
#https://map-rfun.library.duke.edu/01_georeference
