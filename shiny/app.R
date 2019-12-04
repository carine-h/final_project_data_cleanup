library(shiny)

library(shinythemes)

library(tidyverse)

library(ggplot2)



# Import data for concentration tab

social_sciences_permutation <- read_rds("./concentration/social_sciences_permutation.rds")
humanities_permutation <- read_rds("./concentration/humanities_permutation.rds")
stem_permutation <- read_rds("./concentration/stem_permutation.rds")
violin_concentration <- read_rds("./concentration/violin_concentration.rds")
barplot_concentration <- read_rds("./concentration/barplot_concentration.rds")


# Import data for race tab

asian_permutation <- read_rds("./race/asian_permutation.rds")
black_permutation <- read_rds("./race/black_permutation.rds")
white_permutation <- read_rds("./race/white_permutation.rds")
hispanic_permutation <- read_rds("./race/hispanic_permutation.rds")
violin_race <- read_rds("./race/violin_race.rds")
barplot_race <- read_rds("./race/barplot_race.rds")


# Import data for religion tab

christian_permutation <- read_rds("./religion/christian_permutation.rds")
agnostic_or_atheist_permutation <- read_rds("./religion/agnostic_or_atheist_permutation.rds")
violin_religion<- read_rds("./religion/violin_religion.rds")
barplot_religion <- read_rds("./religion/barplot_religion.rds")

# Import data for legacy tab

legacy_no_permutation <- read_rds("./legacy/legacy_no_permutation.rds")
legacy_yes_permutation <- read_rds("./legacy/legacy_yes_permutation.rds")
violin_legacy<- read_rds("./legacy/violin_legacy.rds")
barplot_legacy <- read_rds("./legacy/barplot_legacy.rds")


# Import data for first-gen tab

firstgen_no_permutation <- read_rds("./firstgen/firstgen_no_permutation.rds")
firstgen_yes_permutation <- read_rds("./firstgen/firstgen_yes_permutation.rds")
violin_firstgen<- read_rds("./firstgen/violin_firstgen.rds")
barplot_firstgen <- read_rds("./firstgen/barplot_firstgen.rds")

# Import data for  political views tab


#Import data for international tab

international_yes_permutation <- read_rds("./international/international_yes_permutation.rds")
international_no_permutation <- read_rds("./international/international_no_permutation.rds")
violin_international <- read_rds("./international/violin_international.rds")
barplot_international <- read_rds("./international/barplot_international.rds")

#Import data for urban tab

urban_permutation <- read_rds("./urban/urban_permutation.rds")
rural_permutation <- read_rds("./urban/rural_permutation.rds")
violin_urban <- read_rds("./urban/violin_urban.rds")
barplot_urban <- read_rds("./urban/barplot_urban.rds")

#Import data for athlete tab

athlete_yes_permutation <- read_rds("./athlete/athlete_yes_permutation.rds")
athlete_no_permutation <- read_rds("./athlete/athlete_no_permutation.rds")
violin_athlete <- read_rds("./athlete/violin_athlete.rds")
barplot_athlete <- read_rds("./athlete/barplot_athlete.rds")

#Import data for politics tab
data_plot_political <- read_rds("./politics/data_plot_political.rds")
linear_plot_political <- read_rds("./politics/linear_plot_political.rds")
regression_table_political <- read_rds("./politics/regression_table_political.rds")

# Define UI for application 




ui <- navbarPage(
    
    
    
    # Application title
    
    
    
    title = "Harvard Self-Segregation Study",
    
    
    
    # Application theme
    
    
    
    theme = shinytheme("cosmo"),
    
    
    
    
    
    # Tab divisions
    
    
    
    # "ABOUT" TAB provides and overview of the project and its findings
    
    
    
    tabPanel(
        
        title = "About",
        
        fluidRow(
            
            column(12,
                   
                   wellPanel(
                       
                       htmlOutput("about")
                       
                   ))
            
        )),
    
    
    
    
    # "Race" tab will provide all analysis related to race variable
    
    
    
    tabPanel(
        
        title = "Race",
        
        h3("How did our respondents vary in race"),
        fluidRow(
            column(12,
                mainPanel(
                  plotOutput("barplot_race")
               ))),
        
        br(),
        
        h3("How did participants of each race respond"),
        
        fluidRow(
          column(12,
                 mainPanel(
                   plotOutput("violin_race")
                 ))),
        
        br(),
        
        sidebarLayout(
            
            sidebarPanel(
                
                radioButtons(inputId = "race_permutation", 
                             
                             label = "Race", 
                             
                             choices = c("White", "Black", "Asian", "Hispanic"
                                         
                             ))),
            
            mainPanel(
                
                plotOutput("race_permutation_plot")
                
            ))
        
        ), 
    
    
    # The "Religion" tab will provide all analysis related to race variable

  tabPanel(
  
    title = "Religion",
  
    h3("How did our respondents vary by religion"),
    fluidRow(
     column(12,
             mainPanel(
               plotOutput("barplot_religion")
            ))),
  
    br(),
  
   h3("How did participants of each religion respond"),
  
    fluidRow(
     column(12,
            mainPanel(
              plotOutput("violin_religion")
            ))),
  
    br(),
  
    sidebarLayout(
    
      sidebarPanel(
      
        radioButtons(inputId = "religion_permutation", 
                   
                   label = "Religion", 
                   
                   choices = c("Christian", "Agnostic or Atheist"
                               
                   ))),
    
    mainPanel(
      
      plotOutput("religion_permutation_plot")
      
    ))),

  # The "Legacy" tab will provide all analysis related to the legacy variable
  
  tabPanel(
    
    title = "Legacy",
    
    h3("How did our respondents vary by legacy status"),
    fluidRow(
      column(12,
             mainPanel(
               plotOutput("barplot_legacy")
             ))),
    
    br(),
    
    h3("How did participants of each legacy status respond"),
    
    fluidRow(
      column(12,
             mainPanel(
               plotOutput("violin_legacy")
             ))),
    
    br(),
    
    sidebarLayout(
      
      sidebarPanel(
        
        radioButtons(inputId = "legacy_permutation", 
                     
                     label = "Legacy", 
                     
                     choices = c("Legacy", "Not Legacy"
                                 
                     ))),
      
      mainPanel(
        
        plotOutput("legacy_permutation_plot")
        
      ))),
  
  # The "Athlete" tab will provide all analysis related to the athlete variable
  
  tabPanel(
    
    title = "Athlete",
    
    h3("How many of our respondents were atheletes?"),
    fluidRow(
      column(12,
             mainPanel(
               plotOutput("barplot_athlete")
             ))),
    
    br(),
    
    h3("How did athletes and non-athletes respond?"),
    
    fluidRow(
      column(12,
             mainPanel(
               plotOutput("violin_athlete")
             ))),
    
    br(),
    
    sidebarLayout(
      
      sidebarPanel(
        
        radioButtons(inputId = "athlete_permutation", 
                     
                     label = "athlete", 
                     
                     choices = c("Athlete", "Not Athlete"
                                 
                     ))),
      
      mainPanel(
        
        plotOutput("athlete_permutation_plot")
        
      ))),

  # The "International" tab will provide all analysis related to international variable
  
  tabPanel(
    
    title = "International",
    
    h3("How many of our respondents were International Students"),
    fluidRow(
      column(12,
             mainPanel(
               plotOutput("barplot_international")
             ))),
    
    br(),
    
    h3("How did international students and domestic students respond?"),
    
    fluidRow(
      column(12,
             mainPanel(
               plotOutput("violin_international")
             ))),
    
    br(),
    
    sidebarLayout(
      
      sidebarPanel(
        
        radioButtons(inputId = "international_permutation", 
                     
                     label = "International", 
                     
                     choices = c("International", "Not International"
                                 
                     ))),
      
      mainPanel(
        
        plotOutput("international_permutation_plot")
        
      ))),    
  
 # The "Concentration" tab will provide all analysis related to the concentration variable 
 
  tabPanel(
    
    title = "Concentration",
    
    h3("How did our respondents vary by concentration"),
    fluidRow(
      column(12,
             mainPanel(
               plotOutput("barplot_concentration")
             ))),
    
    br(),
    
    h3("How did participants of each concentration respond"),
    
    fluidRow(
      column(12,
             mainPanel(
               plotOutput("violin_concentration")
             ))),
    
    br(),
    
    sidebarLayout(
      
      sidebarPanel(
        
        radioButtons(inputId = "concentration_permutation", 
                     
                     label = "Concentration", 
                     
                     choices = c("Social Science", "STEM", "Humanities"
                                 
                     ))),
      
      mainPanel(
        
        plotOutput("concentration_permutation_plot")
        
      ))), 
 
 # The "First Gen" tab will provide all analysis related to the first gen variable
 
 tabPanel(
   
   title = "First Gen",
   
   h3("How many of our respondents were First Gen Students"),
   fluidRow(
     column(12,
            mainPanel(
              plotOutput("barplot_firstgen")
            ))),
   
   br(),
   
   h3("How did first gen students and non first gen students respond?"),
   
   fluidRow(
     column(12,
            mainPanel(
              plotOutput("violin_firstgen")
            ))),
   
   br(),
   
   sidebarLayout(
     
     sidebarPanel(
       
       radioButtons(inputId = "firstgen_permutation", 
                    
                    label = "First Gen", 
                    
                    choices = c("First Gen", "Not First Gen"
                                
                    ))),
     
     mainPanel(
       
       plotOutput("firstgen_permutation_plot")
       
     ))),    
 
 # The "Urban" tab will provide all analysis related to the urban variable
 
 tabPanel(
   
   title = "Urban",
   
   h3("How many of our respondents are from urban or rural settings"),
   fluidRow(
     column(12,
            mainPanel(
              plotOutput("barplot_urban")
            ))),
   
   br(),
   
   h3("How did urban and rural students respond?"),
   
   fluidRow(
     column(12,
            mainPanel(
              plotOutput("violin_urban")
            ))),
   
   br(),
   
   sidebarLayout(
     
     sidebarPanel(
       
       radioButtons(inputId = "urban_permutation", 
                    
                    label = "Urban", 
                    
                    choices = c("Urban", "Rural"
                                
                    ))),
     
     mainPanel(
       
       plotOutput("urban_permutation_plot")
       
     ))), 
 
 # The "Politics" tab will provide all analysis related to the political variable
 
 tabPanel(
   
   title = "Politics",
   
   h3("What did the political maekup of our sample look like?"),
   fluidRow(
     column(12,
            mainPanel(
              plotOutput("data_plot_political")
            ))),
   
   br(),
   
   h3("Let's look at a regression of political views"),
   
   fluidRow(
     column(12,
            mainPanel(
              plotOutput("regression_table_political")
            ))),
   
   br(),
   
   sidebarLayout(
     
     sidebarPanel(
       
       radioButtons(inputId = "urban_permutation", 
                    
                    label = "Urban", 
                    
                    choices = c("Urban", "Rural"
                                
                    ))),
     
     mainPanel(
       
       plotOutput("urban_permutation_plot")
       
     )))
 
)
            
  
  





# Define server logic required to create user interface elements using function()




server <- function(input, output) {
    
    
    
    # Content of the "ABOUT" tab. We put a discription of our app in the p()
    
    
    
    output$about <- renderUI({
        
        HTML(paste(
            
            h2("Research Question"),
            
            p("Harvard College prides itself on its mission of diversity and inclusion. The official website states, “Harvard's commitment to diversity in all forms is rooted in our fundamental belief that engaging with unfamiliar ideas, perspectives, cultures, and people creates the conditions for dramatic and meaningful growth.” Harvard believes students learn the most when they engage with students from different backgrounds, perspectives, and identities. How is the university holding up to the standard it sets for itself? To what extent do students form close connections to people different from themselves? Using data from students about their networks, we study the degree of self-segregation that takes place in students’ social networks."),
            
            br(),
            
            br(),
            
            h3("Research Design"),
            
            p("Our data on friend group composition comes from a survey taken by around 80 Eliot House residents. Respondents were asked to fill out information about themselves as well as their four closest friends or acquaintances at Harvard pertaining to ethnicity, background, legacy status, immigration status, extracurricular activities, areas of studies and others."),
            
            br(),
            
            h3("Data Visualization"),
            
            p("We display the observed distributions of friend groups from different backgrounds with what we would expect if factors like racial or religious background were completely uncorrelated with friend group selection. To that end, we look at data that the Crimson has compiled for each class. We explore the level of self-segregation at Harvard by comparing the composition of students’ friend groups to the overall diversity of Harvard students along the variables of gender, class year, extracurricular and academic, national, rural/urban, racial, religious and political background as well as athlete, first-generation, and legacy status."),
            
            br(),
            
            h3("Findings"),
            
            p("For this milestone, we analyzed self-segregation by race. For our final project, using a similar method, we want to visualize and evaluate the significance of our findings for religion, concentrational division, athlete, legacy, and first-generation status, national/international background, and political opinions. For race, we find that white students are significantly less likely to self-segregate than the average, whereas African American and Asian American respondents were more likely to form friendships within their own groups.")
            
            
        ))
        
    })
    
    
    
    # Content of Race Tab
    
    output$violin_race <- renderPlot({
      
      violin_race
      
    })  
    
    output$barplot_race<- renderPlot({
        
   barplot_race      
        
    })
    
    
    output$race_permutation_plot <- renderPlot({
        
        if(input$race_permutation == "White"){
            
            white_permutation
            
        }
        
        
        
        else if(input$race_permutation == "Black"){
            
            black_permutation
            
        }
        
        
        
        else if(input$race_permutation == "Asian"){
            
            asian_permutation
            
        }
        
        
        
        else if(input$race_permutation == "Hispanic"){
            
            hispanic_permutation
            
        }
      
    })
    
    
    #Content of Religion Tab
      
      output$violin_religion <- renderPlot({
        
        violin_religion
        
      })  
      
      output$barplot_religion<- renderPlot({
        
        barplot_religion     
        
      })
      
      
      output$religion_permutation_plot <- renderPlot({
        
        if(input$religion_permutation == "Christian"){
          
          christian_permutation
          
        }
        
        
        
        else if(input$religion_permutation == "Agnostic or Atheist"){
          
          agnostic_or_atheist_permutation
          
        }
        
      })
        
      
      #Content of Legacy Tab
        
        output$violin_legacy <- renderPlot({
          
          violin_legacy
          
        })  
        
        output$barplot_legacy<- renderPlot({
          
          barplot_legacy     
          
        })
        
        
        output$legacy_permutation_plot <- renderPlot({
          
          if(input$legacy_permutation == "Legacy"){
            
            legacy_yes_permutation
            
          }
          
          
          
          else if(input$religion_permutation == "Not Legacy"){
            
            legacy_no_permutation
            
          }     
        
        
    })
      

   #Content of Athlete Tab
        
        output$violin_athlete <- renderPlot({
          
          violin_athlete
          
        })  
        
        output$barplot_athlete<- renderPlot({
          
          barplot_athlete     
          
        })
        
        
        output$athlete_permutation_plot <- renderPlot({
          
          if(input$athlete_permutation == "Athlete"){
            
            athlete_yes_permutation
            
          }
          
          
          
          else if(input$religion_permutation == "Not Athlete"){
            
            athlete_no_permutation
            
          }     
          
        })
          
          
  #Content of International Tab
          
          output$violin_international <- renderPlot({
            
            violin_international
            
          })  
          
          output$barplot_international<- renderPlot({
            
            barplot_international     
            
          })
          
          
          output$international_permutation_plot <- renderPlot({
            
            if(input$international_permutation == "International"){
              
              international_yes_permutation
              
            }
            
            
            
            else if(input$international_permutation == "Not International"){
              
              international_no_permutation
              
            }              
        })
    
    
  #Content of Concentration Tab
          
          output$violin_concentration <- renderPlot({
            
            violin_concentration
            
          })  
          
          output$barplot_concentration<- renderPlot({
            
            barplot_concentration    
            
          })
          
          
          output$concentration_permutation_plot <- renderPlot({
            
            if(input$concentration_permutation == "Social Science"){
              
              social_sciences_permutation
              
            }
            
            
            else if(input$concentration_permutation == "STEM"){
              
              stem_permutation
              
            }   
            
            else if(input$concentration_permutation == "Humanities"){
              
              humanities_permutation
              
            }     
          })
    
   #Content of First Gen Tab
          
          output$violin_firstgen <- renderPlot({
            
            violin_firstgen
            
          })  
          
          output$barplot_firstgen<- renderPlot({
            
            barplot_firstgen    
            
          })
          
          
          output$firstgen_permutation_plot <- renderPlot({
            
            if(input$firstgen_permutation == "First Gen"){
              
              firstgen_yes_permutation
              
            }
            
            
            
            else if(input$firstgen_permutation == "Not First Gen"){
              
              firstgen_no_permutation
              
            }              
          })
    
  #Content of Urban Tab
          
          output$violin_urban <- renderPlot({
            
            violin_urban
            
          })  
          
          output$barplot_urban<- renderPlot({
            
            barplot_urban    
            
          })
          
          
          output$urban_permutation_plot <- renderPlot({
            
            if(input$urban_permutation == "Urban"){
              
              urban_permutation
              
            }
            
            
            
            else if(input$urban_permutation == "Rural"){
              
              rural_permutation
              
            }              
          })
    

          
          
          #Content of Politics Tab
          
          output$violin_urban <- renderPlot({
            
            violin_urban
            
          })  
          
          output$barplot_urban<- renderPlot({
            
            barplot_urban    
            
          })
          
          
          output$urban_permutation_plot <- renderPlot({
            
            if(input$urban_permutation == "Urban"){
              
              urban_permutation
              
            }
            
            
            
            else if(input$urban_permutation == "Rural"){
              
              rural_permutation
              
            }              
          })  
          
          
          
}





# Run the application 

shinyApp(ui = ui, server = server)