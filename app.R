library(shiny)
library(googlesheets4)
library(tidyverse)
library(DT)
library(ggplot2)
library(lubridate)
library(devtools)
library(emoGG)

gs4_deauth()

# Define Google Sheet URL
masterDataSheet_url <- "https://docs.google.com/spreadsheets/d/1813b2nagaxZ80xRfyMZNNKySZOitro5Nt7W4E9WNQDA/edit?gid=0#gid=0"

# Load data
cwqt_all <- range_read(masterDataSheet_url, 
                       sheet = 2,
                       col_names = TRUE, 
                       col_types = "c", 
                       trim_ws = TRUE) |> 
  select(where(~ any(!is.na(.)))) |> 
  #Format variables
  mutate(across(where(is.character), 
                ~ifelse(. %in% c("n/a", "N/A", "NA", "?"), NA, .))) |> 
  #remove the < and > symbols from MPN
  mutate(`Most Probable Number (MPN) of Enterococcus colonies per 100 ml`= 
           str_remove_all(`Most Probable Number (MPN) of Enterococcus colonies per 100 ml`,
                          "[<>]")) |> 
  #Convert other variables to more useful formats
  mutate(Site = factor(Site),
         `Site ID` = factor(`Site ID`),
         `Most Probable Number (MPN) of Enterococcus colonies per 100 ml`= as.numeric(`Most Probable Number (MPN) of Enterococcus colonies per 100 ml`),
         Year = as.numeric(Year),
         Month = as.numeric(Month)) |> 
  mutate(BatteryHighTide_DateTime = mdy_hm(paste0(`Full Date`, " ", `Battery High Tide`)),
         SampleTime_DateTime = mdy_hm(paste0(`Full Date`, " ", `Sample Time`)),
         `Full Date` = mdy(`Full Date`)) |> 
  mutate(across(contains('Precipitation'), ~ str_replace_all(., "Trace", ".01"))) |> 
  mutate(across(contains('Precipitation'), as.numeric)) |> 
  rowwise() |> 
  mutate(WeeklyRainfall = sum(c_across(contains('Precipitation')), na.rm = T)) |> 
  ungroup()

# Define UI
ui <- fluidPage(
  titlePanel("Community Water Quality Testing Data Visualization"),
  sidebarLayout(
    sidebarPanel(
      helpText(
        "Graph the data collected each year. More info about CWQT at: https://www.billionoysterproject.org/water-quality"
      ),
      selectInput("site", "Select Site:", choices = unique(cwqt_all$Site)),
      selectInput("year", "Select Year:", choices = unique(cwqt_all$Year))
    ),
    mainPanel(
      plotOutput("line_chart"),
      DTOutput("table")
    )
  )
)

# Define server logic
server <- function(input, output) {
  filtered_data <- reactive({
    cwqt_all %>%
      filter(Site == input$site, Year == input$year)
  })
  
  output$table <- renderDT({
    datatable(filtered_data())
  })
  
  output$line_chart <- renderPlot({
    ggplot(filtered_data(), aes(x = `Full Date`, y = `Most Probable Number (MPN) of Enterococcus colonies per 100 ml`)) + 
      geom_vline(aes(xintercept = `Full Date`, color = WeeklyRainfall), linewidth = 9, alpha = 0.4) +
      scale_color_continuous(low = "white", high ="#024984") +
      theme_minimal() +
      scale_y_log10() +
      geom_hline(yintercept=104, linetype="dashed", color = "red") +
      #geom_text(aes(x = ymd('2024-05-10'), y = 104, label = "Unacceptable", vjust = -.4, hjust = 0)) +
      geom_hline(yintercept=35, linetype="dashed", color = "yellow") +
      #geom_text(aes(x = ymd('2024-05-10'), y = 35, label = "Unacceptable if levels persist", vjust = -.4, hjust = 0)) +
      geom_line() +
      geom_emoji(emoji = "1f4a9") +
      labs(color = "Weekly Rain (Inches)", y = "Enterococcus Most Probable Number (MPN)") + 
      scale_x_date(breaks = filtered_data()$`Full Date`) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
