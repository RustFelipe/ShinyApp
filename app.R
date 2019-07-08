library(shiny)
library(shinythemes)
library(ggplot2)

#Set the data
lrdata <- read.csv(file = "reservoirDATA_3.csv", header = TRUE, sep = ",", dec = ".")
data1 <-  subset(lrdata, select = c("Reservoirs",	"Age", "Year",	"Month",	"Campaigns",	
                                    "Day.of.the.year", "Depth_m", "Water.Temperature_Celsius", 
                                    "Diss.Oxigen_percentage", "Diss.Oxigen_mg.L", "pH", "alkalinity", 
                                    "Wind.speed_m.s",	"Air.temperature_Celsius", "Tphosphorus",	
                                    "Tnitrogen", "iron",	"DOC_mg.L",		"DIC_mg.L", "co2.flux",	
                                    "ch4.flux",	"pCO2_ppm",	"pCH4_ppm",	"ch4.concentration_uM", 
                                    "co2.concentration_uM",	"dist.from.shore_m", "land.cover_"))

#Transforming field campaigngs to factor
data1$Campaigns <- factor(data1$Campaigns)
data1$Age <- factor(data1$Age)
data1$Year <- factor(data1$Year)

#Transforming Month to factor and giving new labels
data1$Month <- factor(data1$Month,
                      labels = c("June", "August", "October"))


# plot theme
mytheme2 <- function() {
  theme(legend.position = "top", legend.direction="horizontal",
        legend.title = element_text(size = 20),
        legend.text = element_text(size=20),
        axis.line = element_line(size=0.5, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 20, family = "Arial"),
        text = element_text(size = 20, family = "Arial"),
        axis.title.y = element_text(size = 16, colour = "black", vjust = +1),
        axis.title.x = element_text(size = 16, colour = "black", vjust = -1),
        axis.text.x = element_text(colour="black", size = 20),
        axis.text.y = element_text(colour="black", size = 20))
}

ui <- {fluidPage(  
  
  navbarPage(title = div(
                  a(img(height = "55px",
                   width = "55px",
                   align = "left",
                   style = "position: relative; top: -17px; left: -5px;",
                   src = "carbbass.gif"), 
               href = "http://carbbas.uqam.ca/en.html"),
    "Data analysis of La Romaine hydroelectric complex"),
    
             tabPanel("Data visualization",
                      sidebarLayout(
                        sidebarPanel(
                          tags$style(".well {background-color:#e6f9ff;}"),
                          helpText("Create graphs using
                                   available variables."),
                          
                          selectInput(inputId = "var",
                                      label = "Select the reservoir",
                                      choices = list("La Romaine 1", 
                                                     "La Romaine 2",
                                                     "La Romaine 3",
                                                     "All"),
                                      selected = "All"),
                          
                          selectInput(inputId = "tes",
                                      label = "Select the year*",
                                      choices = list("2015", 
                                                     "2016",
                                                     "2017",
                                                     "2018",
                                                     "All"),
                                      selected = "All"),
                          
                          helpText("*La Romaine 1 and 3 were flooded only in 2016 and 2017, respectively"),
                          
                          selectInput(inputId = "rows2",
                                      label = "Select variables - Y axis",
                                      choices = names(data1),
                                      selected = "ch4.concentration_uM"),
                          
                          selectInput(inputId = "rows",
                                      label = "Select variables - X axis",
                                      choices = names(data1),
                                      selected = "Depth_m"),
                          
                          selectInput(inputId = "cor",
                                      label = "Color by variables",
                                      choices = names(data1),
                                      selected = "series"),
                          selectInput("plot", "Plot type",
                                      c("Scatter" = "scatter",
                                        "Box Plot" = "box_plot")),
                          
                          selectInput("ggplot_scaletype", "Scale type",
                                      c("Normal" = "normal",
                                        "Log10" = "log10",
                                        "Log10 - Y axis" = "log10Y",
                                        "Log10 - X axis" = "log10X"))),
                        
                        mainPanel (
                          tabsetPanel(type ="tabs",
                                      tabPanel("Plot", plotOutput("test")),
                                      tabPanel("Table", helpText("Coming soon")))
                         )
                        )
                      ),
            tabPanel("Modeling", helpText("Coming soon")),
            theme = shinytheme("flatly")
    ),             
  helpText("Partners:"),
   a(img(height = "85px",
             width = "150px",
             src = "hydroquebec.jpg"), 
             href = "http://www.hydroquebec.com")
  
 )
}

server <- function(input, output) {
  
  data <- reactive ({
    
    data1 <- switch(input$var, 
                    "La Romaine 1" = subset(data1, subset = Reservoirs %in%  c("RO1")),
                    "La Romaine 2" = subset(data1, subset = Reservoirs %in%  c("RO2")),
                    "La Romaine 3" = subset(data1, subset = Reservoirs %in%  c("RO3")),
                    "All" = subset(data1, subset = Reservoirs %in%  c("RO1", "RO2", "RO3")))
    
    
    switch(input$tes,
           "2015" = subset(data1, subset = Year %in%  c("2015")),
           "2016" = subset(data1, subset = Year %in%  c("2016")),
           "2017" = subset(data1, subset = Year %in%  c("2017")),
           "2018" = subset(data1, subset = Year %in%  c("2018")),
           "All" = subset(data1, subset = Year %in%  c("2015", "2016", "2017", "2018")))
  })
  
  
  output$test <- renderPlot({
    
    lab <- function() {
      labs(x = input$rows, 
           y = input$rows2,
           color=input$cor) 
    }
    
    scatterplot <- ggplot(data(), aes_string(x = input$rows, y = input$rows2)) +
      geom_point(shape = 19, aes_string(color = (input$cor))) +
      geom_smooth(method = "lm", colour = "black", se = FALSE)+
      lab() +
      mytheme2 () +
      guides(color = guide_legend(title.position = 'top'))
    
    boxplot <- ggplot(data(), aes_string(x = input$rows, y = input$rows2)) +
      geom_boxplot(shape = 19, aes_string(fill = (input$cor))) +
      lab() +
      mytheme2 () +
      guides(color = guide_legend(title.position = 'top'))
    
    
    p1 <- switch(input$ggplot_scaletype,
                 normal = scatterplot,
                 log10 = scatterplot + scale_x_log10() + scale_y_log10(),
                 log10Y = scatterplot + scale_y_log10(),
                 log10X = scatterplot + scale_x_log10())  
    
    p2 <- switch(input$ggplot_scaletype,
                 normal = boxplot,
                 log10 = boxplot + scale_x_log10() + scale_y_log10(),
                 log10Y = boxplot + scale_y_log10(),
                 log10X = boxplot + scale_x_log10())
    
    p <- switch(input$plot,
                scatter = p1,
                box_plot = p2)
    
    p              
    
  })
  
}

shinyApp(ui = ui, server = server)

