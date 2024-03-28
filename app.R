library(dplyr)
library(shiny)
library(DT)
library(shinydashboard)
library(ggplot2)
library(highcharter)
library(plotly)
library(shinyjs)
#-------------------------------------------------------------------------
# Read dataset
file_path <- "C:/Users/anon/Desktop/DatasetIV/airline_delay.csv"
data <- read.csv(file_path)
#--------------------------------------------------------------------------
# Preprocessing: Convert numeric columns to integer columns
numeric_columns <- c("carrier_ct", "weather_ct", "nas_ct", "security_ct", "late_aircraft_ct")
data[, numeric_columns] <- lapply(data[, numeric_columns], as.integer)
#--------------------------------------------------------------------------
# Remove NA values from data
your_data_no_na <- na.omit(data)
#---------------------------------------------------------------------------
# Calculate the total number of delays, cancellations, and diverted flights for each year
result <- your_data_no_na %>%
  group_by(year) %>%
  summarise(
    total_delays = sum(arr_del15, na.rm = TRUE),
    total_cancellations = sum(arr_cancelled, na.rm = TRUE),
    total_diverted = sum(arr_diverted, na.rm = TRUE)
  )

# View the resulting summary
print(result)
#----------------------------------------------------------------------------------------
#Calculate the total number of delays, cancellations, and diverted flights for each carrier name.
result <- your_data_no_na %>%
  group_by(carrier_name) %>%
  summarise(
    total_delays = sum(arr_del15, na.rm = TRUE),
    total_cancellations = sum(arr_cancelled, na.rm = TRUE),
    total_diverted = sum(arr_diverted, na.rm = TRUE)
  )
print(result)
#--------------------------------------------------------------------------------------------
#Calculate the average delay time for each carrier.
result <- your_data_no_na %>%
  group_by(carrier_name) %>%
  summarise(
    avg_delay_time = mean(arr_delay, na.rm = TRUE)
  )
print(result)
#--------------------------------------------------------------------------------------------
#Calculate the average delay time for each year.
data %>%
  group_by(year) %>%
  summarise(
    avg_delay_time = mean(arr_delay, na.rm = TRUE)
  )
#--------------------------------------------------------------------------------------------
# Calculate the average number of cancelled flights for each year
data %>%
  group_by(year) %>%
  summarise(
    avg_cancelled_flights  = mean(arr_cancelled, na.rm = TRUE)
  )
#--------------------------------------------------------------------------------------------
## Calculate the average number of diverted flights for each year
data %>%
  group_by(year) %>%
  summarise(
    avg_diverted_flights = mean(arr_diverted, na.rm = TRUE)
  )
#-------------------------------------------------------------------------------------------
#Calculate the proportion of delayed flights (more than 15 minutes) for each carrier.
data %>%
  group_by(carrier_name) %>%
  summarise(
    #نسبه تاخير الرحلات ف كل شركه
    proportion_delayed= sum(arr_del15 > 0, na.rm = TRUE) / n()
  )
#---------------------------------------------------------------------------------------------
#Compare the average delay times and cancellation rates between different airlines.
data %>%
  group_by(carrier_name) %>%
  summarise(
    avg_delay_time = mean(arr_del15, na.rm = TRUE),
    cancellation_rate = mean(arr_cancelled > 0, na.rm = TRUE)
    
  )
#------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------
# Convert 'date' column to Date type (if not already)
#data$date <- as.Date(data$date)

# Define UI for application that draws a histogram
# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "My Shiny Dashboard"),
  dashboardSidebar(
    # Sidebar content (input controls)
    sidebarMenu(
      #menuItem("Tab 1", tabName = "tab1", icon = icon("dashboard")),
      menuItem("Tab 1", tabName = "tab1", icon = icon("chart-bar")),
      
      menuItem("Tab 2", tabName = "tab2", icon = icon("image")),
      menuItem("Tab 3", tabName = "tab3", icon = icon("chart-simple")),
      # Add more menu items as needed
      checkboxGroupInput("show_charts", "Select Charts to Display",
                         choices = c(
                           "Total Number of Flights Over the different carrier" = "chart1",
                           "Monthly Distribution of Delayed Flights by Carrier" = "chart2",
                           "Proportion of Cancellations by Carrier" = "chart3",
                           "Year Distribution of Cancelled Flights" = "chart4",
                           "Year Distribution of Diverted Flights" = "chart5",
                           "Year Distribution of Delayed Flights due to Weather" = "chart6",
                           "Heatmap of Delay Times Over years and Carriers" = "chart7",
                           "Relationship Between Carrier and Cancellations" = "chart8",
                           "Delay Types Over Years by Airport" = "chart9",
                           "Histogram of Arrival Delays" = "chart10",
                           "Diverted Flights vs. Cancellations by Carrier" = "chart11"
                         ),
                         selected = c("chart1", "chart2", "chart3", "chart4", "chart5", "chart6", "chart7", "chart8", "chart9", "chart10", "chart11")
                         
                         
                         
      )
    )
  ),
  #----------------------------------------------------------------------------------
  #----------------------------------------------------------------------------------
  dashboardBody(
    # Body content (main panel)
    tabItems(
      # Tab 1 content
      tabItem(
        tabName = "tab1",
        fluidRow(
          box(
            
            # Dropdown menu for selecting specific carrier
            selectInput("selected_carrier", "Select Carrier", choices = unique(data$airport)
            )#end of box
          ),
          
          box(
            # Display the DataTable based on the selected year range
            dateRangeInput("date_range", "Select Year Range", 
                           start = min(data$year), end = max(data$year),
                           min = min(data$year), max = max(data$year))
          ),
          
          box(
            # Display the DataTable based on the selected carrier
            checkboxGroupInput("selected_carriers", "Select Carriers", choices = unique(data$carrier))
          ),
          
          box(
            # Dynamic filter for selecting units
            selectInput("unit", "Select Unit:", c("Absolute Values", "Percentage Change")),
            
            # Dynamic filter for selecting carriers
            selectInput("carrier1", "Select Carrier:", unique(data$carrier)),
            
            # Dynamic filter for selecting months
            selectInput("month", "Select Month:", unique(data$month)),
            
            
            # Display the scatter plot based on user selections
            plotOutput("scatterPlot")
          ),
          box(
            # Radio buttons for choosing daily or cumulative data
            radioButtons("data_type", "Select Data Type:",
                         choices = c("Daily", "Cumulative"),
                         selected = "Daily"),
            # Add other filters as needed
            # Display the scatter plot based on user selections
            plotOutput("scatterPlot1")
          )
        )#end of fluidRow
      ),#end of tab1
      
      #), # Close tabItem for "tab1"
      #------------------------------------------------------------------------------------------------
      #------------------------------------------------------------------------------------------------
      # Tab 2 content for the bar plot
      tabItem(
        tabName = "tab2",
        fluidRow(
          plotlyOutput("my_plot")
        )
      ), # Close tabItem for "tab2"
      #------------------------------------------------------------------------------------------------
      #------------------------------------------------------------------------------------------------
      # Tab 3 content for the bar plot
      tabItem(
        tabName = "tab3",
        # Display the DataTable based on the selected year range
        fluidRow(
          box(
            title = "Chart 1: Total Number of Flights Over the different carrier",
            plotOutput("chart1"),
          ),
          box(
            title = "Chart 2: Monthly Distribution of Delayed Flights by Carrier",
            plotOutput("chart2"),
          ),
          box(
            title = "Chart 3: Proportion of Cancellations by Carrier",
            plotOutput("chart3"),
          ),
          box(
            title = "Chart 4: Year Distribution of Cancelled Flights",
            plotOutput("chart4"),
          ),
          box(
            title = "Chart 5: Year Distribution of Diverted Flights",
            plotOutput("chart5"),
          ),
          box(
            title = "Chart 6: Year Distribution of Delayed Flights due to Weather",
            plotOutput("chart6")
          ),
          box(
            title = "Chart 7: Heatmap of Delay Times Over years and Carriers",
            plotOutput("chart7")
          ),
          box(
            title = "Chart 8: Relationship Between Carrier and Cancellations",
            plotOutput("chart8")
          ),
          box(
            title = "Chart 9: Delay Types Over Years by Airport",
            plotOutput("chart9")
          ),
          box(
            title = "Chart 10: Histogram of Arrival Delays",
            plotOutput("chart10")
          ),
          box(
            title = "Chart 11: Diverted Flights vs. Cancellations by Carrier",
            plotOutput("chart11")
          )
          
        ) # Close fluei3 tabItem for "tab3"
        #------------------------------------------------------------------------------------------------
        #------------------------------------------------------------------------------------------------
      ) # Close tab3
    ) # Close tabitems dashboardBody
  ) # Close dashboardBody
) #close dashboardpage

# Define server
server <- function(input, output, session) {
  output$dataTable <- renderDT({
    Dropdown_data <- subset(data, carrier == input$selected_carrier)
    checkbox_data <- subset(data, carrier %in% input$selected_carriers)
    DataRange_data <- subset(data, year >= input$date_range[1] & year <= input$date_range[2])
  })
  
  # Render interactive bar plot using plotly
  output$barplot <- renderPlotly({
    # Assuming 'data' is your dataset and 'arr_del15' is the column for the bar plot
    plot <- ggplot(data, aes(x = factor(year), y = arr_del15, text = paste("Year: ", year, "<br>arr_del15: ", arr_del15))) +
      geom_bar(stat = "identity", fill = "blue") +
      labs(title = "Bar Plot Example", x = "Year", y = "arr_del15") +
      theme_minimal()
    ggplotly(plot, tooltip = "text")
  })
  
  output$weather_delay_plot <- renderPlotly({
    if (input$data_type == "Daily") {
      # Daily data
      ggplotly(
        ggplot(data, aes(x = date, y = weather_delay, text = paste("Date: ", date, "<br>Weather Delay: ", weather_delay))) +
          geom_line() +
          labs(title = "Weather Delay Over Time (Daily)", x = "Date", y = "Weather Delay") +
          theme_minimal()
      )
    } else {
      # Cumulative data
      cumulative_data <- data %>% 
        group_by(date) %>% 
        summarise(weather_delay = sum(weather_delay))
      
      ggplotly(
        ggplot(cumulative_data, aes(x = date, y = weather_delay, text = paste("Date: ", date, "<br>Cumulative Weather Delay: ", weather_delay))) +
          geom_line() +
          labs(title = "Cumulative Weather Delay Over Time", x = "Date", y = "Cumulative Weather Delay") +
          theme_minimal()
      )
    }
    #------------------------------------------------------------------------------------------
    #------------------------------------------------------------------------------------------
  })
  output$chart1 <- renderPlot({
    if ("chart1" %in% input$show_charts) {
      ggplot(data, aes(x = carrier, y = arr_flights)) +
        geom_line(color = "red") +
        labs(title = "Total Number of Flights Over the different carrier", x = "Carrier", y = "Total Flights")
    }
  })
  #------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------
  output$chart2 <- renderPlot({
    if ("chart2" %in% input$show_charts) {
      ggplot(data, aes(x = factor(month), y = arr_del15, fill = carrier)) +
        geom_bar(stat = "identity") +
        labs(title = "Monthly Distribution of Delayed Flights by Carrier", x = "Month", y = "Delayed Flights", fill = "Carrier")
    }
  })
  #------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------
  
  output$chart3 <- renderPlot({
    if ("chart3" %in% input$show_charts) {
      pie_data <- data %>%
        group_by(carrier_name) %>%
        summarise(total_cancellations = sum(arr_cancelled))
      ggplot(pie_data, aes(x = "", y = total_cancellations, fill = carrier_name)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y") +
        labs(title = "Proportion of Cancellations by Carrier", fill = "Carrier Name")
    }
  })
  #------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------
  output$chart4 <- renderPlot({
    if ("chart4" %in% input$show_charts) {
      ggplot(data, aes(x = factor(year), y = arr_cancelled)) +
        geom_bar(stat = "identity", fill = "grey") +
        labs(title = "Year Distribution of Cancelled Flights", x = "Year", y = "Cancelled Flights")
    }
  })
  #------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------
  output$chart5 <- renderPlot({
    if ("chart5" %in% input$show_charts) {
      ggplot(data, aes(x = factor(year), y = arr_diverted)) +
        geom_bar(stat = "identity", fill = "grey") +
        labs(title = "Year Distribution of Diverted Flights", x = "Year", y = "Diverted Flights")
    }
  })
  #------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------
  output$chart6 <- renderPlot({
    if ("chart6" %in% input$show_charts) {
      ggplot(data, aes(x = factor(year), y = weather_ct)) +
        geom_bar(stat = "identity", fill = "grey") +
        labs(title = "Year Distribution of Delayed Flights due to Weather", x = "Year", y = "Delayed Flights due to Weather")
    }
  })
  #------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------
  output$chart7 <- renderPlot({
    if ("chart7" %in% input$show_charts) {
      ggplot(data, aes(x = year, y = carrier_name, fill = arr_delay)) +
        geom_tile() +
        scale_fill_gradient(low = "white", high = "red") +
        labs(title = "Heatmap of Delay Times Over years and Carriers",
             x = "Year", y = "Carrier Name", fill = "Total Delay Time (minutes)")
    }
  })
  #------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------
  output$chart8 <- renderPlot({
    if ("chart8" %in% input$show_charts) {
      ggplot(data, aes(x = carrier, y = arr_cancelled)) +
        geom_point() +
        labs(title = "Relationship Between Carrier and Cancellations", x = "Carrier", y = "Cancelled Flights")
    }
  })
  #------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------
  output$chart9 <- renderPlot({
    if ("chart9" %in% input$show_charts) {
      data_long <- tidyr::gather(data, key = "delay_type", value = "delay_count", carrier_ct:late_aircraft_ct)
      ggplot(data_long, aes(x = factor(year), y = delay_count, fill = delay_type)) +
        geom_bar(stat = "identity", position = "stack") +
        labs(title = "Delay Types Over Years by Airport", x = "Year", y = "Delay Count", fill = "Delay Type")
    }
  })
  #------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------
  output$chart10 <- renderPlot({
    if ("chart10" %in% input$show_charts) {
      ggplot(data, aes(x = arr_cancelled)) +
        geom_histogram(binwidth = 15, fill = "skyblue", color = "black", aes(y = ..density..)) +
        labs(title = "Histogram of Arrival Delays", x = "Arrival Delay (minutes)", y = "Density")
    }
  })
  #------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------
  output$chart11 <- renderPlot({
    if ("chart11" %in% input$show_charts) {
      bubble_data <- data %>%
        group_by(carrier) %>%
        summarise(total_diverted = sum(arr_diverted),
                  total_cancellations = sum(arr_cancelled))
      
      # Create a bubble chart with two different attributes
      ggplot(bubble_data, aes(x = total_diverted, y = total_cancellations, color = carrier)) +
        geom_point(alpha = 0.7) +
        scale_size(range = c(5, 20)) +
        labs(title = "Diverted Flights vs. Cancellations by Carrier",
             x = "Total Diverted Flights", y = "Total Cancellations",
             color = "Carrier")
    }
  })
  #------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------
  output$my_plot <- renderPlotly({
    # Create a plot with hover information
    plot <- ggplot(data, aes(x = airport, y = arr_del15, text = paste("airport: ", airport, "<br>arr_del15: ", arr_del15))) +
      geom_line() +
      labs(title = "Flights More Than 15 Minutes Late", x = "airport", y = "Number of Flights") +
      theme_minimal()
    
    # Convert the ggplot object to plotly
    ggplotly(plot, tooltip = "text")
  })
  #------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------
  filtered_df <- reactive({
    # Filter data based on user input
    output$scatter-plot <- renderPlot({
      ggplot(filtered_df(), aes(x = month, y = arr_del15, color = carrier)) +
        geom_line() +
        labs(title = "Arrival Delays",
             x = "Month",
             y = "Number of Flights More Than 15 Minutes Late")
    })
    carriers <- input$carrier1
    if (is.null(carriers)) carriers <- unique(data$carrier1)
    
    filtered_data <- data %>%
      #noteeee back-------------------------------------------------------------
    filter(carrier1 %in% carriers)
    
    # Group data based on selected data type
    if (input$data-type == "Daily") {
      grouped_data <- radio %>%
        group_by(year, month, carrier) %>%
        summarize(arr_del15 = sum(arr_del15))
    } else {
      grouped_data <- radio %>%
        group_by(year, month, carrier) %>%
        summarize(arr_del15 = cumsum(arr_del15))
    }
    
    return(grouped_data)
  })
  # Reactive function to filter data based on user selections
  filtered_data <- reactive({
    # Filter data based on user-selected filters
    selected_data <- data
    
    # Filter based on carrier
    if (!is.null(input$carrier1)) {
      selected_data <- filter(selected_data, carrier == input$carrier1)
    }
    
    
    # Filter based on month
    if (!is.null(input$month)) {
      selected_data <- filter(selected_data, month == input$month)
    }
    if (input$unit == "Percentage Change") {
      # Calculate percentage change if needed
      selected_data <- mutate(selected_data, arr_del15 = (arr_del15 / arr_flights) * 100)
    }
    return(selected_data)
  })
  
  # Reactive function to generate scatter plot based on filtered data
  output$scatterPlot <- renderPlot({
    # Create the scatter plot based on filtered_data()
    ggplot(filtered_data(), aes(x = arr_flights, y = arr_del15, color = arr_delay)) +
      geom_point() +
      labs(title = "Relationship between Flights and Delays",
           x = "Number of Flights",
           y = "Number of Flights Delayed > 15 mins",
           color = "Total Delayed Time (minutes)") +
      theme_minimal()
  })
  # Reactive function to generate scatter plot based on filtered data
  output$scatterPlot1 <- renderPlot({
    # Create the scatter plot based on filtered_data() and radio button choice
    ggplot(data, aes(x = if (input$data_type == "Daily") arr_flights else month,
                     y = if (input$data_type == "Daily") arr_del15 else arr_delay,
                     color = if (input$data_type == "Daily") month else arr_delay)) +
      geom_point() +
      labs(title = "Relationship between Flights and Delays",
           x = if (input$data_type == "Daily") "Number of Flights" else "Month",
           y = if (input$data_type == "Daily") "Number of Flights Delayed > 15 mins" else "Total Delayed Time (minutes)",
           color = if (input$data_type == "Daily") "Month" else "Total Delayed Time (minutes)") +
      theme_minimal()
  })
  
  
}

shinyApp(ui, server)