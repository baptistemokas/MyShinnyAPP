# Load packages#######################################################################
######################################################################################
######################################################################################
library(shiny)
library(shinythemes)
library(plyr)
library(dplyr)
library(readr)

library(data.table)
library(splitstackshape)
library(zoo)
library(DT)
library(stats)
library(base)
library(ExPanDaR)
library(ggplot2)
#############################             IMPORT OF RAW DATA    ######################
######################################################################################
######################################################################################
#LOCAL Play????
#setwd("/Users/baptistasmokas/Desktop/Shiny_Project/shiny_app")
#FILE METRIC
dir_csv_metric = "./csv_metric"
my_metric = list.files(path=dir_csv_metric, pattern="*.csv", full.names=TRUE)
metric_csv = ldply(my_metric, read_csv)
############################# IMPORT OF FULL DATASET AND ADD THE QUERY INSIDE ########


#FILE DATA 
dir_csv_data = "./csv_data"
my_data = list.files(path=dir_csv_data, pattern="*.csv", full.names=TRUE)
data_csv = ldply(my_data, read_csv)

#FILE DATA + NAMES
all_paths = list.files(path=dir_csv_data, pattern="*.csv", full.names=TRUE)
all_content <-  all_paths %>% lapply(read.table, header = TRUE, sep = ",", encoding = "UTF-8")
all_filenames <- all_paths %>%  basename() %>%  as.list()
all_lists <- mapply(c, all_content, all_filenames, SIMPLIFY = FALSE)
all_result <- rbindlist(all_lists, fill = T)
names(all_result)[25] <- "type"
names(all_result)[4] <- "date"


########## FILE DATA + NAMES + Count citation each year

##########   CREATION OF THE BASIC DF / -> Query /// date /// Number of publication //// Nombre de citations
#trend_d <- xtabs(formula=Cites~Query + date, data=all_result)
#trend_d

DT <- data.table(all_result) 
trend_d <- DT[ , .(Totalcount = sum(Cites)), by = .(type,date)]
trend_dsorted <- trend_d[order(trend_d$type, trend_d$date),]
names(trend_dsorted)[3] <- "close"

### Verification of column type
sapply(trend_dsorted, typeof)


### Conversion de date en format date : 

#yrs <- trend_dsorted$date
#as.Date(ISOdate(yrs, 1, 1)) 
#as.Date(paste(yrs, 1, 1, sep = "-"))
#as.Date(as.yearmon(yrs))
#names(trend_dsorted)[1] <- "type"
###
trend_dsorted$date <- paste(trend_dsorted$date,"-01-31T04:00:00Z")

#REmove the last 3 caracters of the name wich is extracted from the filename ".csv"
trend_dsorted$type  <- gsub('.{4}$', '', trend_dsorted$type)
#Remove some letter 
trend_dsorted$type <- substring(trend_dsorted$type, 6)


#Remove all line wich contain an AN please
trend_dsorted <- trend_dsorted[!grepl("NA", trend_dsorted$date),]

#Remove all white space in the date column : 
trend_dsorted$date <- gsub('\\s+', '', trend_dsorted$date)

write.csv(all_result, "./temp/data_fullcsv.csv")
write.csv(trend_dsorted, "./temp/trend_dsorted.csv")
trend_dsorted <- read_csv("./temp/trend_dsorted.csv")
trend_dsorted$X1 <- NULL
typeof(trend_dsorted)

trend_description <- read_csv("./trend_description.csv")

#Convert close into double 
#trend_dsorted$close <- as.double(trend_dsorted$close) 
#trend_dsorted$date <- as.double(trend_dsorted$date)
sapply(trend_dsorted, class)

#CREATE DATAFRAM TO SHOW $
trend_show <- all_result
trend_show$Abstract <- NULL
trend_show$type <- NULL
trend_show$Volume <- NULL
trend_show$Issue <- NULL
trend_show$DOI <- NULL
trend_show$ISSN <- NULL
trend_show$CitationURL <- NULL


##############
#IMPORT LIST OF WORDS ;: 
list_of_words <- read_csv("./listofwords.csv")


trend_dsorted2 <- trend_dsorted
trend_dsorted2[,"cumul"] <- cumsum(trend_dsorted2$close)
#############################                 PREPROCESSING      #####################
######################################################################################
######################################################################################

###### FOR THE APP WE NEED TO ADD A NEW ROW INSIDE OUR DATAFRAME   ###################
# Load data

# trend_description <- read_csv("data/trend_description.csv")
# Select whether to overlay smooth trend line


  











































ui <- fluidPage(theme = shinytheme("sandstone"),   #slate
                titlePanel("___EPISTEMOLOGICAL DYNAMICS IN MACHINE LEARNING RESERCH", "Baptiste Mokas - Shiny App"),
                navbarPage("-",
                           
                           tabPanel("The Projet",
                                    img(src='nycdsa.png', height="5%", align = "right"),
                                    mainPanel(
                                      HTML(paste("Baptiste Mokas", "- A NYCDSA PROJECT", '<br/>', "2020", '<br/>', "_", '<br/>')),
                                      tabsetPanel(type = "tabs",
                                                          tabPanel("Aim of the project", HTML(paste(
                                                                     '<br/>',
                                                                     "As a datascientist, you may ask yourself, ... ", '<br/>',
                                                                     h1("What technology should you master ?"),'<br/>',
                                                                     h3(".. Or how to scrap Google scholar datascience publications"),'<br/>',
                                                                     "What kind of technologies are the most relevent ?",'<br/>',
                                                                     "What technologies are the most in boom ?",'<br/>',
                                                                     "What should I know as a datascientist ?",'<br/>',
                                                                     '<br/>',
                                                                     "How to know who to contact ?",'<br/>',
                                                                     "When you are looking for a specialist, who should you contact ?",'<br/>',
                                                                     '<br/>',
                                                                     "When you want to master something for a projet, what others related concept and theories should you master ?"
                                                                        )
                                                                     )
                                                                     
                                                                     ),
                                                  
                                                         
                                                          tabPanel("Epistemology ?", 
                                                                   img(src='EPIS.png', height="100%", width="100%", align = "right"),
                                                                   )
                                                  )
                                                  )
                                    
                                    
                                    ),
                           
                           
                           tabPanel("Methodology", 
                                    mainPanel(
                                      tabsetPanel(type = "tabs",
                                                  tabPanel("Google scholar", 
                                                           img(src='scolar.png', height="100%", width="100%", align = "right")
                                                          ),
                                                  tabPanel("General sight",
                                                           img(src='method.png', height="100%", width="100%", align = "right")
                                                           ),
                                                  tabPanel("Publish or Perish", 
                                                           tabsetPanel(type = "tabs", 
                                                                       
                                                                       tabPanel("Application", 
                                                                                img(src='publishapp.png', height="100%", width="100%", align = "right"),
                                                                                ),
                                                                       
                                                                       tabPanel("Python Code", 
                                                                                tags$a(href="https://github.com/joshmfrankel/pop-publish-or-perish", "Git-Hub")
                                                                                )
                                                                       ) 
                                                           
                                                          ),
                                                  
                                                  tabPanel("What is the ecosystem of datascience ?",
                                                          tabsetPanel(type = "tabs",
                                                                       tabPanel("Word-list", 
                                                                                div(dataTableOutput("mywords"), style = "font-size: 70%")
                                                                                ),
                                                                      tabPanel("Skitlearn", 
                                                                               img(src='sklearn.png', height="100%", width="100%", align = "right"),
                                                                      ),
                                                                      tabPanel("Academic Program", 
                                                                               tags$a(href = "https://datascience-x-master-paris-saclay.fr/le-master/structure-des-enseignements/", "Master of datascience of Paris", target = "_blank"),
                                                                               
                                                                               tags$a(href = "https://cds.nyu.edu/", "NYU’s Center for Data Science", target = "_blank"),
                                                                               
                                                                               tags$a(href = "https://datascience-x-master-paris-saclay.fr/le-master/structure-des-enseignements/", "Columbia University in the City of New York", target = "_blank"),
                                                                               
                                                                               tags$a(href = "https://datascience-x-master-paris-saclay.fr/le-master/structure-des-enseignements/", "Georgia Institute of Technology", target = "_blank"),
                                                                               
                                                                               tags$a(href = "https://datascience-x-master-paris-saclay.fr/le-master/structure-des-enseignements/", "North Carolina State University – Raleigh, North Carolina", target = "_blank"),
                                                                               
                                                                               tags$a(href = "https://datascience-x-master-paris-saclay.fr/le-master/structure-des-enseignements/", "University of Denver", target = "_blank")
                                                                      ),
                                                                      tabPanel("Toward multidisciplinarity..", 
                                                                               img(src='energy.png', height="100%", width="100%", align = "right"),
                                                                      ),
                                                                      tabPanel("..", 
                                                                               img(src='connection.png', height="100%", width="100%", align = "right"),
                                                                      ),
                                                                      tabPanel("Personal map", 
                                                                               img(src='perso.png', height="100%", width="100%", align = "right")
                                                                                )
                                                        )
                                                  
                                                  
                                                  
                                                  
                                                  
                                                  )
                                            )
                                    )
                                    ),
                                    
                           
                           
                           tabPanel("Raw Data", 
                                  div(dataTableOutput("mytable0"), style = "font-size: 70%")
                                    
                                  ),
                           
                           
                           
                           
                           tabPanel("Overview",  
                                    plotOutput(outputId = "lineplot3", height = "630px")),
                                    
                           
                                                          
                                                          
                                                
                           tabPanel("Evolutionnary Dynamics",
                                sidebarLayout(sidebarPanel(
                                      
                                      # Select type of trend to plot
                                      selectInput(inputId = "type", label = strong("type"),
                                                  choices = unique(trend_dsorted$type),
                                                  selected = "attention_networks"),
                                      
                                      # Select date range to be plotted
                                      dateRangeInput("date", strong("Date range"), start = "1920-01-01", end = "2018-07-31",
                                                     min = "1920-01-01", max = "2018-07-31"),
                                      
                                      # Select whether to overlay smooth trend line
                                      checkboxInput(inputId = "smoother", label = strong("Overlay smooth trend line"), value = FALSE),
                                      
                                      # Display only if the smoother is checked
                                      conditionalPanel(condition = "input.smoother == true",
                                                       sliderInput(inputId = "f", label = "Smoother span:",
                                                                   min = 0.01, max = 1, value = 0.67, step = 0.01,
                                                                   animate = animationOptions(interval = 100)),
                                                       HTML("Higher values give more smoothness.")),
                                             
                                ),
                  
                  # Output: Description, lineplot, and reference
                 
                                   mainPanel(tabsetPanel(type = "tabs",
                                                    
                                                    tabPanel("Number of publication per year", 
                                                             plotOutput(outputId = "lineplot", height = "450px"), textOutput(outputId = "desc"),
                                                            tags$a(href = "https://www.google.com/finance/domestic_trends", "Source: Google Domestic Trends", target = "_blank")),
                                                    
                                                    tabPanel("Cumulative evolution",
                                                             plotOutput(outputId = "lineplot2", height = "450px")), 
                                                             
                                                    
                                                    
                                                    tabPanel("Dataframe description", 
                                                             div (dataTableOutput ("mytable2"), style = "font-size: 70%") ))
                                             )
                                           )
                  ),
                  tabPanel("Find your contact", 
                           
                           selectInput(inputId = "typeC", label = strong("Concept that you are interested on :"),
                                       choices = unique(all_result$type),
                                       selected = "attention_networks"),
                           
                           div(dataTableOutput("mytable1"), style = "font-size: 80%")
                  ),
                tabPanel("Perspectives of developpement", 
                         verticalLayout(
                           a("test"),
                           a("test"),
                           '<br/>',
                           '<br/>',
                           '<br/>',
                           '<br/>',
                           '<br/>',
                           a(href="http://example.com/link3", "Link Three")
                         ),
                         HTML(paste(
                           h3("Limitation"), 
                           "google pov",'<br/>',
                           "Dynamics cycle",'<br/>',
                           "What about disruption ?",'<br/>',
                           "Abstraction / Hierarchical in conceptual dynamics",'<br/>',
                           "Bayesian Network vs Bayesian Networks ??? ",'<br/>',
                           "Good to adapt to the système, but for datascientist, not reserchers.. ((is there something to find where everyone are looking ? )",'<br/>',
                           "QUANTITY =! QUALITY ",'<br/>',
                           "PERSONAL POV ",'<br/>',
                           "THE VALUE IS NOT THE REAL VALUE BUT THE ONE THAT WE GIVE .. WE TRUST",'<br/>',
                           "UNCORRELATED WITH COMPLEXITY .. OBVIOUSLY.. ",'<br/>',
                           "Python based scrapping straight into the shinny app… _> was the aim but to hard in so ",
                           "h and g index???", 
                           "area near to each ?",
                           "SELF CYCLIC CAUSAL SYSTÈME / PREFERENTIAL ATTACHEMENT / ",'<br/>'))
                         ),
                
               
                tabPanel("About",
                         "this application is a project conducted during the NYC DATASCIENCE ACADEMY bootcamp in 2020",
                         "Author : Baptiste Mokas",
                         
                         tags$a(href="www.rstudio.com", "GIT HUB"),
                         HTML(paste('<br/>',
                                    "baptiste.mokas@gmail.com",'<br/>',
                                    "website : emergence-lfe.org", '<br/>',
                                    h2("Thank you")
                                    
                                    ))
                        
                         
                         )
            )
)          
                
                
# Define server function
server <- function(input, output) {
  
  # Subset data
  selected_trends <- reactive({
    req(input$date)
    validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
    trend_dsorted %>%
      filter(
        type == input$type,
        date > as.POSIXct(input$date[1]) & date < as.POSIXct(input$date[2]
        ))
  })
  
  output$lineplot <- renderPlot({
    color = "#434343"
    par(mar = c(4, 4, 1, 1))
    plot(x = selected_trends()$date, y = selected_trends()$close, type = "l",
         xlab = "Date", ylab = "Trend index", col = color, fg = color, col.lab = color, col.axis = color)
    # Display only if smoother is checked
    if(input$smoother){
      smooth_curve <- lowess(x = as.numeric(selected_trends()$date), y = selected_trends()$close, f = input$f)
      lines(smooth_curve, col = "#E6553A", lty=2, lwd = 3)
    }
  })
  

  # Create scatterplot object the plotOutput function is expecting
  selected_trends2 <- reactive({
    req(input$date)
    validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
    trend_dsorted2 %>%
      filter(
        type == input$type,
        date > as.POSIXct(input$date[1]) & date < as.POSIXct(input$date[2]
        ))
  })
  
  
  output$lineplot2 <- renderPlot({
    color = "#434343"
    par(mar = c(4, 4, 1, 1))
    plot(x = selected_trends2()$date, y = selected_trends2()$cumul, type = "l",
         xlab = "Date", ylab = "Trend index", col = color, fg = color, col.lab = color, col.axis = color)
    # Display only if smoother is checked
    if(input$smoother){
      smooth_curve2 <- lowess(x = as.numeric(selected_trends2()$date), y = selected_trends2()$cumul, f = input$f)
      lines(smooth_curve2, col = "#E6553A", lty=2, lwd = 3)
    }
  })
  
  
  
  
 output$lineplot3 <- renderPlot({
    ggplot(metric_csv, aes(x=Query, y=Cites_Paper)) +
      geom_bar(stat='identity') +
      coord_flip()
  })
  
  
  output$mytable1 = DT::renderDataTable({
    all_result %>%
      filter(
        type == input$typeC,
      )
  })
  
  
  output$mytable0 = DT::renderDataTable({
    trend_show
  })
  
  output$mywords = DT::renderDataTable({
    list_of_words
  })
  
  output$mytable = DT::renderDataTable({
    trend_show
  })
  
  output$mytable2 = DT::renderDataTable({
    trend_dsorted
  })
  
  
  url <- a("Google Homepage", href="https://www.google.com/")
  output$tab <- renderUI({
    tagList("URL link:", url)
  })
  
  # Pull in description of trend
  output$desc <- renderText({
    trend_text <- filter(trend_description, type == input$type) %>% pull(text)
    paste(trend_text, "The index is set to 1.0 on January 1, 2004 and is calculated only for US search traffic.")
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)

