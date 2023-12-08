Sabre Fencing Analytics

Author: Tan Kah Hong

Date: 2023-12-05

# Introduction:

* Fencing competitions generate a wealth of data capturing the performance of individual fencers across different clubs.
  
* The Sabre Fencing Analytics project aims to create a versatile tool capable of organizing, processing, and manipulating competition data to generate insightful performance reports for individual fencers and fencing clubs.

# Key Features:

**Data Cleaning:**

Alphanumeric characters, such as '3T', are replaced with numeric values, facilitating subsequent computations.

**Club Standardization:**

Clubs with multiple names are standardized by taking only the first value. This step ensures consistency in the analysis.

**Whitespace Removal:**

Extra white spaces are trimmed from the right of club names to prevent duplicate grouping caused by inconsistent spacing.

**Dynamic Datatable and Visualizations:**

The tool provides a dynamic datatable and interactive visualizations, allowing users to compare fencers' average performances across different clubs interactively.

# The Data Set:

* Fencing competition results are sourced from an online platform called FencingTimeLive.
  
* To streamline the analysis, addressing missing values and alphanumeric entries in the "Place" column of the CSV file downloaded from the platform is crucial.

| Place |    Name   |  Club(s) | Division | Country | License # |
|:-----:|:---------:|:--------:|:--------:|:-------:|:---------:|
|   1   |  Fencer 1 | School A |    NA    |   SGP   |     NA    |
|   2   |  Fencer 2 | School B |    NA    |   SGP   |     NA    |
|   3T  |  Fencer 3 | School C |    NA    |   PHI   |     NA    |
|   3T  |  Fencer 4 | School D |    NA    |   SGP   |     NA    |
|   5   |  Fencer 5 | School A |    NA    |   SGP   |     NA    |
|   6   |  Fencer 6 | School B |    NA    |   SGP   |     NA    |
|   7   |  Fencer 7 | School C |    NA    |   SGP   |     NA    |
|   8   |  Fencer 8 | School D |    NA    |   PHI   |     NA    |
|   9   |  Fencer 9 | School A |    NA    |   SGP   |     NA    |
|   10  | Fencer 10 | School B |    NA    |   SGP   |     NA    |
|   11  | Fencer 11 | School C |    NA    |   SGP   |     NA    |
|   12  | Fencer 12 | School D |    NA    |   SGP   |     NA    |
|   13  | Fencer 13 | School A |    NA    |   SGP   |     NA    |
|   14  | Fencer 14 | School B |    NA    |   SGP   |     NA    |
|   15  | Fencer 15 | School C |    NA    |   MAC   |     NA    |
|   16  | Fencer 16 | School D |    NA    |   SGP   |     NA    |
|   17  | Fencer 17 | School A |    NA    |   PHI   |     NA    |
|   18  | Fencer 18 | School B |    NA    |   SGP   |     NA    |
|   19  | Fencer 19 | School C |    NA    |   SGP   |     NA    |
|   20  | Fencer 20 | School D |    NA    |   SGP   |     NA    |
|   21  | Fencer 21 | School A |    NA    |   SGP   |     NA    |
|   22  | Fencer 22 | School B |    NA    |   SGP   |     NA    |
|   23  | Fencer 23 | School C |    NA    |   SGP   |     NA    |
|   24  | Fencer 24 | School D |    NA    |   SGP   |     NA    |

Table: An example of the FencingTimeLive data set in CSV format.

# Loading the Libraries:

* To facilitate user interaction and data processing, the following libraries are employed, enabling the creation of a Shiny app:
  
```
library(shiny)
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(plotly)
library(DT)
library(RColorBrewer)
```
# Creating a User-Defined Function:

* To address the challenge posed by alphanumeric values under the "Place" column named "__T" in the fencing competition result dataset, a user-defined function named replace_and_convert is created. 

* This function ensures that only numeric values remain in the "Place" column for streamlined computation and summarization of the results.
  
```
replace_and_convert <- function(DF) {
  DF$Place <- gsub("[^0-9.]+", "", DF$Place)
  DF$Place <- as.numeric(DF$Place)
  return(DF)
}
```
# Creating the Data Processing Feature:

* Utilizing dplyr tools, a streamlined data processing feature is created to organize and summarize the fencing data effectively. 

* This feature computes key metrics for each fencing club, such as total number of participating fencers, average position attained, highest and lowest ranked result, and medal tally.
  
```
Processed_DF <- DF %>%
  replace_and_convert() %>%
  select(Place, Name, Club_Name = `Club(s)`) %>%
  mutate(Club_Name = str_split(Club_Name, "/", simplify = TRUE)[, 1]) %>%
  mutate(Club_Name = str_trim(Club_Name, side = "right")) %>%
  group_by(Club_Name) %>%
  summarise(
    Total_Number = n(),
    Average_Position = as.integer(mean(Place)),
    Highest_Ranking = as.integer(min(Place)),
    Highest_Ranked_Fencer = Name[which.min(Place)],
    Lowest_Ranking = as.integer(max(Place)),
    Lowest_Ranked_Fencer = Name[which.max(Place)],
    Gold = sum(Place == 1),
    Silver = sum(Place == 2),
    Bronze = sum(Place == 3),
    Total_Medal_Count = sum(Gold, Silver, Bronze)
  ) %>%
  arrange(Total_Number)
```
# Creating the Shiny App:

* The Shiny app integrates all the features created earlier, allowing users to seamlessly upload competition results and interact with the computed summary.
  
* The app features a user interface with a file input button for uploading the CSV file and showcases the processed data in an easy-to-read datatable and interactive visualizations.

# Shiny App

## How to use:

1. Upload your fencing data CSV file using the "Choose CSV File" input.
   
2. Explore individual fencer performances with a scatter plot and delve into computed key mertrics with the bar chart and data table.
   
## User Interface Section:

* The fluidPage creates the main layout for the shiny app.
  
* 'titlePanel' and 'fileInput' are used to display title and allow users to upload the competiton results in csv format. 
'DTOutput' and 'plotlyOutput' are used as placeholders for the outputs that will be generated based on my analysis.

```
ui <- fluidPage(
  titlePanel("Fencing Data Processing App"),
  fileInput("file", "Choose CSV File"),
  DTOutput("summaryTable"),
  plotlyOutput("scatterplot"),
  plotlyOutput("plot")
)
```
## Server Section:

* The server function defines the server-side logic for the shiny app.
  
* Reactive functions like 'Tabular_Data' and 'Visualisation_Data' process the uploaded csv file for tabular data in the Datatable and visualisation data in the plots respectively.
  
* 'renderPlotly' and 'renderDT' are used to generate the interactive plots and data table outputs respectively.
  
* By using 'renderDT', it creates a interactive datatable with key metrics like total number of participating fencers, average position attained, highest and lowest ranked result, and medal tally. These metrics can be filtered in ascending/descending order and a search bar is available for users to search for a specific club found in the Datatable.
  
* Within 'renderPlotly' there are 2 visualisations, the scatterplot which identifies the final ranking of all fencers and arranged based on their clubs, and the other being a horizontal bar chart that tracks the average position of fencers from each club.
  
* In both visualisations, key functions like 'ggplot2' and 'plotly' are used to create the interactive visualisations that provides information when hovered over individual data points.
  
* Diving into the scatterplot output, 'scale_colour_manual' is used to colour code the gold, silver and bronze medalist of the competition for their exceptional performance.
  
* The horizontal bar chart employs 'scale_fill_gradient' to visually represent club performance by color-coding average positions. Green indicates lower average positions, while red signifies higher average positions.

```
server <- function(input, output) {
  ## Reactive functions for data processing and visualization
  Tabular_Data <- reactive({
    # Read and process the uploaded CSV file
    req(input$file)
    DF <- read_csv(input$file$datapath)
    
    ## Data Processing Feature
    Processed_DF <- DF %>%
      replace_and_convert() %>%
      select(Place, Name, Club_Name = `Club(s)`) %>%
      mutate(Club_Name = str_split(Club_Name, "/", simplify = TRUE)[, 1]) %>%
      mutate(Club_Name = str_trim(Club_Name, side = "right")) %>%
      group_by(Club_Name) %>%
      summarise(
        Total_Number = n(),
        Average_Position = as.integer(mean(Place)),
        Highest_Ranking = as.integer(min(Place)),
        Highest_Ranked_Fencer = Name[which.min(Place)],
        Lowest_Ranking = as.integer(max(Place)),
        Lowest_Ranked_Fencer = Name[which.max(Place)],
        Gold = sum(Place == 1),
        Silver = sum(Place == 2),
        Bronze = sum(Place == 3),
        Total_Medal_Count = sum(Gold, Silver, Bronze)
      ) %>%
      arrange(Total_Number)
    
    return(Processed_DF)
  })

  Visualisation_Data <- reactive({
    ## Read the uploaded CSV file for visualization
    req(input$file)
    DF <- read_csv(input$file$datapath)
    
    ## Visualized data for the performance of all fencers
    Visualised_DF <- DF %>%
      replace_and_convert() %>%
      select(Place, Name, Club_Name = `Club(s)`) %>%
      mutate(Club_Name = str_split(Club_Name, "/", simplify = TRUE)[, 1]) %>%
      mutate(Club_Name = str_trim(Club_Name, side = "right")) %>%
      group_by(Club_Name)
    
    return(Visualised_DF)
  })

  ## Display individual performances of each fencer organized by clubs

  output$scatterplot <- renderPlotly({
    data <- Visualisation_Data()

    scatter_data <- ggplot(data, aes(x = Place, y = Club_Name, color = as.factor(Place), text = paste("Name:", Name, "<br>Club:", Club_Name, "<br>Final Ranking:", Place))) +
      geom_point(size = 3, position = position_jitter(width = 0.2, height = 0)) +
      labs(title = "Performance of all fencers", x = "Final Ranking", y = "Club Name", color = "Final Ranking") +
      theme_minimal(base_size = 6) +
      scale_color_manual(values = c("1" = "#fee101", "2" = "#d7d7d7", "3" = "#a77044", "darkgrey")) + 
      theme(legend.position = "none") 

    ggplotly(scatter_data, tooltip = "text")
  })

  ## Display the processed data in a table
  output$summaryTable <- renderDT({
    Tabular_Data()
  })
  
  output$plot <- renderPlotly({
    data <- Tabular_Data() %>%
      mutate(Club_Name = reorder(Club_Name, Average_Position))

    bar_data <- ggplot(data, aes(x = Club_Name, y = Average_Position, fill = Average_Position, text = paste("Club Name:", Club_Name, "<br>Average Position:", Average_Position))) +
      geom_col() +
      scale_fill_gradient(low = "#66c2a5", high = "#fc8d62", name = "Average Position") +
      labs(title = "Fencer Statistics by Club", x = "Club Name", y = "Average Position", fill = "Average Position") +
      theme_minimal(base_size = 6) +
      coord_flip() 

    ggplotly(bar_data, tooltip = "text")
  })
}

## Run the Shiny app
shinyApp(ui, server)

```

