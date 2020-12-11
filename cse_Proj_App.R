library(dplyr)
library(ggplot2)
library(shiny)
library(DT)
library(ggrepel)
library(tidyr)
library(shinycssloaders)
library(shinythemes)

#Import Data
appraiser <- read.csv("appraiser2.csv")
state <- read.csv("wa.csv")

button_color_css <- "
{
/* Change the background color of the update button
to blue. */
background: DodgerBlue;
/* Change the text size to 15 pixels. */
font-size: 15px;
}"

# UI
ui <- fluidPage(
  
  #Navbar structure for UI
  navbarPage("NOOK", theme = shinytheme("lumen"),
             tabPanel("Property Finder", fluid = TRUE, icon = icon("globe-americas"),
                      tags$style(button_color_css),
                      # Sidebar layout with a input and output definitions
                      sidebarLayout(
                        sidebarPanel(
                          
                          titlePanel("Desired Property Characteristics"),
                          #shinythemes::themeSelector(),
                          fluidRow(column(3,
                                          
                                          # Select which Gender(s) to plot
                                          checkboxGroupInput(inputId = "StatusID",
                                                             label = "Select Status(es):",
                                                             choices = c("Active", "Inactive"),
                                                             selected = "Active"),
                                          
                                          # Select which Division(s) to plot
                                          checkboxGroupInput(inputId = "Ownership",
                                                             label = "Select Ownership Type(s):",
                                                             choices = c("Bank or Real Estate Owned" = "TRUE", "Other" = "FALSE"),
                                                             selected = "FALSE")
                          ),
                          column(6, offset = 2,
                                 # Select which Region(s) to plot
                                 checkboxGroupInput(inputId = "RegionID",
                                                    label = "Select Region(s):",
                                                    choices = c("Bellevue", "Issaquah", "Kirkland", "Woodinville"),
                                                    selected = "Bellevue")
                          )),
                          hr(),
                          
                          titlePanel("Listing Price"),
                          sliderInput(inputId = "PriceID",
                                      label = "Select Price Range",
                                      min = 0,
                                      max = 5500000,
                                      value = c(1, 5500000),
                                      width = "220px"),
                          hr(),
                          
                          titlePanel("Year Built"),
                          sliderInput(inputId = "YearID",
                                      label = "Year Built",
                                      min = 1900,
                                      max = 2021,
                                      value = c(1900,2021),
                                      width = "220px"),
                          hr(),
                          
                          titlePanel("Square Footage"),
                          sliderInput(inputId = "SFID",
                                      label = "Square Footage",
                                      min = 646,
                                      max = 10800,
                                      value = c(646, 10800),
                                      width = "220px"),
                          hr(),
                          
                          titlePanel("Lot Size"),
                          sliderInput(inputId = "LotID",
                                      label = "Lot Size (Acres)",
                                      min = 0.045,
                                      max = 15.512,
                                      value = c(0.045, 15.512),
                                      width = "220px"),
                          hr()
                        ),
                        
                        mainPanel(
                          withSpinner(plotOutput(outputId = "scatterplotFinder", click = "click_plotFinder"
                          )),
                          hr(),
                          fluidRow(column(7,
                                          helpText("Tip: Click locations to populate table below with information on the property")
                                          #actionButton(inputId = "draw", label = "Input Event and Times")
                                          
                          ),
                          column(width = 2, offset = 2, conditionalPanel(
                            condition = "output.propertyTable",
                            actionButton(inputId = "FinderClear", label = "Clear Table")))),
                          br(),
                          fluidRow(
                            withSpinner(dataTableOutput(outputId = "propertyTable"))))
                      )
             )
  )
)

# Server
server <- function(input, output, session) {
  
  #Property Finder
  
  Appraiser_finder <- reactive({
    
    req(input$StatusID)
    req(input$Ownership)
    req(input$RegionID)
    filter(appraiser, Status %in% input$StatusID) %>%
      filter(Bank.Or.Real.Estate.Owned %in% input$Ownership) %>%
      filter(Region %in% input$RegionID) %>%
      filter(Listing.Price >= input$PriceID[1], Listing.Price <= input$PriceID[2]) %>%
      filter(Year.Built >= input$YearID[1], Year.Built <= input$YearID[2]) %>%
      filter(Square.Footage >= input$SFID[1], Square.Footage <= input$SFID[2]) %>%
      filter(Lot.Size >= input$LotID[1], Lot.Size <= input$LotID[2]) %>%
      group_by(Region) %>%
      dplyr::mutate(Entries = n())
  })
  
  output$scatterplotFinder <- renderPlot({
    input$StatusID
    input$Ownership
    input$RegionID
    input$PriceID
    input$YearID
    input$SFID
    input$LotID
    isolate({
      if (length(Appraiser_finder()$Address) == 0) {
        ggplot() +
          geom_polygon(data = state, aes(x = long, y = lat, group = group), color = "darkslategray1", fill = "gray20") +
          coord_quickmap(xlim = c(-122.5, -121.5), ylim = c(47, 48)) +
          theme_void() +
          ggtitle("No properties fit selected characteristics. \nPlease modify selections.") +
          theme(plot.title = element_text(face = "bold", color = "#FF8D1E", size = 20))
      } else {
        ggplot() +
          geom_polygon(data = state, aes(x = long, y = lat, group = group), color = "darkslategray1", fill = "gray20") +
          coord_quickmap(xlim = c(-122.5, -121.5), ylim = c(47, 48)) +
          guides(fill = FALSE) +
          geom_point(data = Appraiser_finder(), aes(x = Lon, y = Lat, color = Region, shape = Region), alpha = 0.5) +
          theme_void() +
          labs(color = "Region", shape = "Region") +
          #{if(length(input$StatusID) <= 1) scale_color_manual(guide = "none", values = c("Active" = "#1E90FF", "Inactive" = "#FF8D1E"))} +
          #{if(length(input$DivisionFinder) > 1)
           # scale_color_manual(values = c("Active" = "blue", "Inactive" = "red"))} +
          #{if(length(input$Ownership) <= 1) scale_shape_manual(guide = "none", values = c("Bank or Real Estate Owned" = "circle", "Other" = "triangle"))} +
          #{if(length(input$Ownership) > 1)
          #  scale_shape_manual(values = c("Bank or Real Estate Owned" = "circle", "Other" = "triangle"))} +
          theme(axis.text = element_blank(), axis.ticks = element_blank()) +
          theme(plot.title = element_text(hjust=0.5, face = "bold")) +
          theme(plot.background = element_rect(fill = "white"), plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) +
          guides(alpha = FALSE) +
          theme(legend.text = element_text(size = 12),
                legend.title = element_text(size = 15)) +
          theme(plot.background = element_rect(
            color = "white"
          ))
      }
    })
  })
  
  user_clickFinder <- reactiveValues()
  reactive({
    user_clickFinder$DT <- data.frame(matrix(0, ncol = ncol(appraiser), nrow = 1))
    names(user_clickFinder$DT) <- colnames(appraiser)
  })
  
  observeEvent(input$click_plotFinder, {
    add_row <- nearPoints(Appraiser_finder(), input$click_plotFinder, xvar = "Lon", yvar = "Lat", threshold = 5)
    user_clickFinder$DT <- rbind(add_row, user_clickFinder$DT)
  })
  
  brushFinder <- reactive({
    req(length(user_clickFinder$DT) > 1)
    user_clickFinder$DT
  })
  
  observeEvent({
    input$FinderClear
  },{
    user_clickFinder$DT <- NULL
  })
  
  output$propertyTable <- DT::renderDataTable({
    
    Appraiser_finder()
    # DT::datatable(unique(brushFinder()[,c("Status", "Address", "Listing.Price", "Year.Built", "Square.Footage", "Lot.Size", "Region")]),
    #               colnames = c("Sort" = "Listing.Price", "Price" = "Listing.Price", "Year Built" = "Year.Built", "Square Footage" = "Square.Footage", "Lot Size" = "Lot.Size"),
    #               rownames = T,
    #               options = list(order = list(9, 'asc'),
    #                              columnDefs = list(list(visible=T, targets=c(9)),
    #                                                list(className = "dt-center"),
    #                                                list(classname = "dt-right"))
    #               ))
  })
  
  #session$onSessionEnded(stopApp)
}

# Run the application
shinyApp(ui = ui, server = server)
