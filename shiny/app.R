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
    
    
    
    title = "Harvard Friend Group Selection Study",
    
    
    
    # Application theme
    
    
    
    theme = shinytheme("cosmo"),
    
    
    
    
    
    # Tab divisions
    
    
    
    # "ABOUT" TAB provides and overview of the project and its findings
    
    
    
    tabPanel(
        
        title = "About",
        
        fluidPage(
          HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/j40ZULTNE78" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
        ),
        
        p(a("PDF Version of Project", href = "https://online.flippingbook.com/view/845969/")),
        
        fluidRow(
            
            column(12,
                   
                   wellPanel(
                       
                       htmlOutput("about")
                       
                   ))
            
        )),
    
    
    
    
    # "Race" tab will provide all analysis related to race variable
    
    
    
    tabPanel(
      title = "Race",
        
      fluidRow(
        
        column(12,
               
               wellPanel(
                 
                 htmlOutput("racedescription")
                 
               ))
        
      ),
        
        h3("How did our respondents vary in race?"),
        fluidRow(
            column(12,
                mainPanel(
                  plotOutput("barplot_race")
               ))),
        
        br(),
        br(),
        
        h3("How did participants of each race respond?"),
        
        fluidRow(
          column(12,
                 mainPanel(
                   plotOutput("violin_race")
                 ))),
        
        br(),
        br(),
        br(),
        
        h3("How does the mean number of same race responses compare to what we would expect under random friend assignment?"),
        
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
    
    fluidRow(
      
      column(12,
             
             wellPanel(
               
               htmlOutput("religiondescription")
               
             ))
      
    ),
  
    h3("How did our respondents vary by religion?"),
    fluidRow(
     column(12,
             mainPanel(
               plotOutput("barplot_religion")
            ))),
  
    br(),
    br(),
  
   h3("How did participants of each religion respond?"),
  
    fluidRow(
     column(12,
            mainPanel(
              plotOutput("violin_religion")
            ))),
  
    br(),
   br(),
   br(),
   
   h3("How does the mean number of same religion responses compare to what we would expect under random friend assignment?"),
  
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
    
    fluidRow(
      
      column(12,
             
             wellPanel(
               
               htmlOutput("legacydescription")
               
             ))
      
    ),
    
    h3("How did our respondents vary by legacy status?"),
    fluidRow(
      column(12,
             mainPanel(
               plotOutput("barplot_legacy")
             ))),
    
    br(),
    br(),
    
    h3("How did participants of each legacy status respond"),
    
    fluidRow(
      column(12,
             mainPanel(
               plotOutput("violin_legacy")
             ))),
    
    br(),
    br(),
    br(),
    
    h3("How does the mean number of same legacy status responses compare to what we would expect under random friend assignment?"),
    
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
    
    fluidRow(
      
      column(12,
             
             wellPanel(
               
               htmlOutput("athletedescription")
               
             ))
      
    ),
    
    h3("How many of our respondents were atheletes?"),
    fluidRow(
      column(12,
             mainPanel(
               plotOutput("barplot_athlete")
             ))),
    
    br(),
    br(),
    
    h3("How did athletes and non-athletes respond?"),
    
    fluidRow(
      column(12,
             mainPanel(
               plotOutput("violin_athlete")
             ))),
    
    br(),
    br(),
    br(),
    
    h3("How does the mean number of same athelete status responses compare to what we would expect under random friend assignment?"),
    
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
    
    fluidRow(
      
      column(12,
             
             wellPanel(
               
               htmlOutput("internationaldescription")
               
             ))
      
    ),
    
    h3("How many of our respondents were International Students?"),
    fluidRow(
      column(12,
             mainPanel(
               plotOutput("barplot_international")
             ))),
    
    br(),
    br(),
    
    h3("How did international students and domestic students respond?"),
    
    fluidRow(
      column(12,
             mainPanel(
               plotOutput("violin_international")
             ))),
    
    br(),
    br(),
    br(),
    
    h3("How does the mean number of same international status responses compare to what we would expect under random friend assignment?"),
    
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
    
    fluidRow(
      
      column(12,
             
             wellPanel(
               
               htmlOutput("concentrationdescription")
               
             ))
      
    ),
    
    h3("How did our respondents vary by concentration?"),
    fluidRow(
      column(12,
             mainPanel(
               plotOutput("barplot_concentration")
             ))),
    
    br(),
    br(),
    
    h3("How did participants of each concentration respond?"),
    
    fluidRow(
      column(12,
             mainPanel(
               plotOutput("violin_concentration")
             ))),
    
    br(),
    br(),
    br(),
    
    h3("How does the mean number of same concentration focus responses compare to what we would expect under random friend assignment?"),
    
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
   
   fluidRow(
     
     column(12,
            
            wellPanel(
              
              htmlOutput("firstgendescription")
              
            ))
     
   ),
   
   h3("How many of our respondents were First Gen Students?"),
   fluidRow(
     column(12,
            mainPanel(
              plotOutput("barplot_firstgen")
            ))),
   
   br(),
   br(),
   
   h3("How did first gen students and non first gen students respond?"),
   
   fluidRow(
     column(12,
            mainPanel(
              plotOutput("violin_firstgen")
            ))),
   
   br(),
   br(),
   br(),
   
   
   h3("How does the mean number of same first gen status responses compare to what we would expect under random friend assignment?"),
   
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
   
   fluidRow(
     
     column(12,
            
            wellPanel(
              
              htmlOutput("urbandescription")
              
            ))
     
   ),
   
   h3("How many of our respondents are from urban or rural settings?"),
   fluidRow(
     column(12,
            mainPanel(
              plotOutput("barplot_urban")
            ))),
   
   br(),
   br(),
   
   h3("How did urban and rural students respond?"),
   
   fluidRow(
     column(12,
            mainPanel(
              plotOutput("violin_urban")
            ))),
   
   br(),
   br(),
   br(),
   
   h3("How does the mean number of same urban status responses compare to what we would expect under random friend assignment?"),
   
   sidebarLayout(
     
     sidebarPanel(
       
       radioButtons(inputId = "urban_permutation", 
                    
                    label = "Urban", 
                    
                    choices = c("Urban", "Rural"
                                
                    ))),
     
     mainPanel(
       
       plotOutput("urban_permutation_plot")
       
     ))),
 
 #The "Politics" tab will provide all analysis related to the political variable
 
 tabPanel(
   
   title = "Politics",
   
   fluidRow(
     
     column(12,
            
            wellPanel(
              
              htmlOutput("politicsdescription")
              
            ))
     
   ),
   
   h3("What did the political maekup of our sample look like?"),
   fluidRow(
     column(12,
            mainPanel(
              plotOutput("data_plot_political")
            ))),
   
   br(),
   
   h3("How did people's political views relate to their friends political views?"),
   
   fluidRow(
     column(12,
            mainPanel(
              plotOutput("linear_plot_political")
            ))),
   
   br(),
    
   h3("Let's analyze this correlation statistically"),
   
   fluidRow(
     column(12,
            mainPanel(
              tableOutput("regression_table_political")
            ))))
 
)
            
  
  





# Define server logic required to create user interface elements using function()




server <- function(input, output, session) {
    
    
    
    # Content of the "ABOUT" tab. We put a discription of our app in the p()
    
    
    
    output$about <- renderUI({
        
        HTML(paste(
            
            h2("Research Question"),
            
            p("Harvard College prides itself on its mission of diversity and inclusion. The official website states, “Harvard's commitment to diversity in all forms is rooted in our fundamental belief that engaging with unfamiliar ideas, perspectives, cultures, and people creates the conditions for dramatic and meaningful growth.” Harvard believes students learn the most when they engage with students from different backgrounds, perspectives, and identities. How is the university holding up to the standard it sets for itself? To what extent do students form close connections to people different from themselves? Using data from students about their networks, we study the degree of self-segregation that takes place in students’ social networks."),
            
            br(),
            
            br(),
            
            h3("Research Design"),
            
            p("Our data on friend group composition comes from a survey taken by over 100 Eliot House residents. Respondents were asked to fill out information about themselves as well as their four closest friends or acquaintances at Harvard pertaining to ethnicity, background, legacy status, immigration status, extracurricular activities, areas of studies and others."),
            
            br(),
            
            h3("Data Visualization"),
            
            p("We display the observed distributions of friend groups from different backgrounds with what we would expect if factors like racial or religious background were completely uncorrelated with friend group selection. To that end, we look at data that the Crimson has compiled for each class. We explore the level of self-segregation at Harvard by comparing the composition of students’ friend groups to the overall diversity of Harvard students along the variables of gender, class year, extracurricular and academic, national, rural/urban, racial, religious and political background as well as athlete, first-generation, and legacy status."),
            
            br(),
            
            h3("Findings"),
            
            p("In short, we conclude that there is a significant degree of self-segregation amongst at least some subgroups for all of the variables studied. In particular, we observe a significant degree of self-segregation among Asian Americans, Christians, international students, STEM and social sciences students, athletes, non-legacy students, and first-generation students. Amongst these, the difference between the overall proportion of students and the prevalence of in-group friendships is particularly striking for Asian Americans, Social Science concentrators, student athletes, and first-generation students. While sample sizes are not large enough to draw any meaningful conclusions, a preliminary analysis also suggests significant rates of in-group selection among African American and Hispanics as well as humanities concentrators. Interestingly, however, White, atheist/agnostic, and American respondents display statistically significant rates of “negative self-segregation”, suggesting that they are more likely to seek out friends different from themselves. .Lastly, political values are clearly and significantly correlated amongst respondents and their friend groups. Due perhaps in part to the scarcity of self-identified conservatives at Harvard, our regression analysis suggests that even strongly conservative students’ friend groups tend to be fairly moderate. For every increase of one point among a 4-point scale measuring political liberalism, however, the mean value of a respondents’ friend group rises by more than a quarter.")
            
            
        ))
        
    })
    
    
    
    # Content of Race Tab
    
    output$racedescription <- renderUI({
      
      HTML(paste(
          
          h3("Summary"),
          
          p("These plots show the distribution of repeated racial characteristics in friend groups. We have a graph with a distribution that shows the makeup of our sample. The next graph shows our actual findings-- how for each racial category, how many other friends do they have in the same category? Finally we have a permutation plot for each race to visualize if our findings are actually significant. These permutations show what randomized friend group racial makeup would look like given the Crimson data which captures overall racial makeup of the Harvard undergraduate population. Essentially, the permutation assume no self-segregation and serve as a benchmark for the data that we collect. This way, we can compare means of racial makeup to a randomized sample in order to determine if there is self-segregation in friend groups along racial lines and if this differs among different racial categories.")
      ))
      
    })
    
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
    
    output$religiondescription <- renderUI({
      
      HTML(paste(
        
        h3("Summary"),
        
        p("These plots show the distribution of repeated religious characteristics in friend groups. We have a graph with a distribution that shows the makeup of our sample. The next graph shows our actual findings-- how for each religious category, how many other friends do they have in the same category? Finally we have a permutation plot for each religion to visualize if our findings are actually significant. These permutations show what randomized friend group religious makeup would look like given the Crimson data which captures overall religious makeup of the Harvard undergraduate population. Essentially, the permutation assumes no self-segregation and serve as a benchmark for the data that we collect. This way, we can compare means of religious makeup to a randomized sample in order to determine if there is self-segregation in friend groups along religious lines and if this differs among different religious categories.")
      ))
      
    })
      
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
      
      output$legacydescription <- renderUI({
        
        HTML(paste(
          
          h3("Summary"),
          
          p("These plots show the distribution of legacy makeup in friend groups. We have a graph with a distribution that shows the makeup of our sample. The next graph shows our actual findings-- for each category (legacy and non-legacy), how many other friends do they have in the same category? Finally we have a permutation plot for each status to visualize if our findings are actually significant. These permutations show what a randomized friend group’s makeup (legacy vs. non-legacy) would look like given the Crimson data which captures overall makeup of the Harvard undergraduate population. Essentially, the permutations assume no self-segregation and serve as a benchmark for the data that we collect. This way, we can compare means of makeup to a randomized sample in order to determine if there is self-segregation in friend groups along these category’s lines and if this differs among categories (legacy vs. non-legacy).")
        ))
        
      })
        
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
          
          
          
          else if(input$legacy_permutation == "Not Legacy"){
            
            legacy_no_permutation
            
          }     
        
        
    })
      

   #Content of Athlete Tab
        
        output$athletedescription <- renderUI({
          
          HTML(paste(
            
            h3("Summary"),
            
            p("These plots show the distribution of athletes in friend groups. We have a graph with a distribution that shows the makeup of our sample. The next graph shows our actual findings-- for each category (athlete and non-athlete), how many other friends do they have in the same category? Finally we have a permutation plot for each status to visualize if our findings are actually significant. These permutations show what a randomized friend group’s makeup (athlete vs. non-athlete) would look like given the Crimson data which captures overall makeup of the Harvard undergraduate population. Essentially, the permutations assume no self-segregation and serve as a benchmark for the data that we collect. This way, we can compare means of athletic makeup to a randomized sample in order to determine if there is self-segregation in friend groups along these category’s lines and if this differs among categories (athlete vs. non-athlete).")
          ))
          
        })
        
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
          
          
          
          else if(input$athlete_permutation == "Not Athlete"){
            
            athlete_no_permutation
            
          }     
          
        })
          
          
  #Content of International Tab
        
        output$internationaldescription <- renderUI({
          
          HTML(paste(
            
            h3("Summary"),
            
            p("These plots show the distribution of international makeup in friend groups. We have a graph with a distribution that shows the makeup of our sample. The next graph shows our actual findings-- for each category (international and non-international), how many other friends do they have in the same category? Finally we have a permutation plot for each status to visualize if our findings are actually significant. These permutations show what a randomized friend group’s makeup (international vs. non-international) would look like given the Crimson data which captures overall makeup of the Harvard undergraduate population. Essentially, the permutations assume no self-segregation and serve as a benchmark for the data that we collect. This way, we can compare means of makeup to a randomized sample in order to determine if there is self-segregation in friend groups along these category’s lines and if this differs among categories (international vs. non-international).")
          ))
          
        })
          
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
          
          output$concentrationdescription <- renderUI({
            
            HTML(paste(
              
              h3("Summary"),
              
              p("These plots show the distribution of repeated concentrations in friend groups. We have a graph with a distribution that shows the makeup of our sample. The next graph shows our actual findings-- how for each concentration category, how many other friends do they have in the same category? Finally we have a permutation plot for each concentration category to visualize if our findings are actually significant. These permutations show what randomized friend group concentration makeup would look like given the Crimson data which captures overall academic makeup of the Harvard undergraduate population. Essentially, the permutation assumes no self-segregation and serve as a benchmark for the data that we collect. This way, we can compare means of concentration makeup to a randomized sample in order to determine if there is self-segregation in friend groups along academic lines and if this differs among different concentration categories.")
            ))
            
          })
          
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
          
          output$firstgendescription <- renderUI({
            
            HTML(paste(
              
              h3("Summary"),
              
              p("These plots show the distribution of first-generation students in friend groups. We have a graph with a distribution that shows the makeup of our sample. The next graph shows our actual findings-- how for first-generation and non-first-gen students, how many other friends do they have in the same category? Finally we have a permutation plot for each category to visualize if our findings are actually significant. These permutations show what randomized friend group makeup would look like given the Crimson data which captures overall academic makeup of the Harvard undergraduate population. Essentially, the permutation assumes no self-segregation and serve as a benchmark for the data that we collect. This way, we can compare means of makeup to a randomized sample in order to determine if there is self-segregation in friend groups along first-generation status.")
            ))
            
          })
          
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
          
          output$urbandescription <- renderUI({
            
            HTML(paste(
              
              h3("Summary"),
              
              p("These plots show the distribution of repeated geographical characteristics in friend groups. We have a graph with a distribution that shows the makeup of our sample. The next graph shows our actual findings-- for each geographical category (urban and rural), how many other friends do they have in the same category? Finally we have a permutation plot for each category to visualize if our findings are actually significant. These permutations show what randomized friend group geographic makeup would look like given the Crimson data which captures overall geographical makeup of the Harvard undergraduate population. Essentially, the permutations assume no self-segregation and serve as a benchmark for the data that we collect. This way, we can compare means of geographical makeup to a randomized sample in order to determine if there is self-segregation in friend groups along geographical lines and if this differs among different geographical categories.")
            ))
            
          })
          
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
          
          output$politicsdescription <- renderUI({
            
            HTML(paste(
              
              h3("Summary"),
              
              p("These plots show the distribution of political view makeup in friend groups. Finally we have a scatter plot visualizing how the political views (with very liberal being a 4 and very conservative being a 0) of respondents compares to the average political views of their friend group. Essentially, the scatter plot shows a positive relationship between people’s political attitudes and the average political attitudes of their friends, as shown in the blue positive trend line. The shading around the line shows the standard error for each segment of the line, showing the uncertainty of the actual value around each x-value prediction.")
            ))
            
          })
          
          output$data_plot_political <- renderPlot({
            
            data_plot_political
            
          })  
          
          output$linear_plot_political<- renderPlot({
            
            linear_plot_political   
            
         })
          
          output$regression_table_political<- renderTable({
            
            regression_table_political
          
                   
          })  
          
          
          
}





# Run the application 

shinyApp(ui = ui, server = server)