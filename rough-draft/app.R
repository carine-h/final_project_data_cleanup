library(shiny)

library(shinythemes)

library(tidyverse)

library(ggplot2)







# Load data






# Due to the immense difficulty and time taken to run the data extraction

# scripts, this is a vanilla app







# Import data for violin test

race_violin <- read_rds("./race_violin.rds")




# Import data for observed Histograms




o_white <- read_rds("./o_white.rds")

o_black <- read_rds("./o_black.rds")

o_asian <- read_rds("./o_asian.rds")

o_hispanic <- read_rds("./o_hispanic.rds")





# Import data for Permutation test




p_white <- read_rds("./p_white.rds")

p_asian <- read_rds("./p_asian.rds")

p_black <- read_rds("./p_black.rds")

p_hispanic<- read_rds("./p_hispanic.rds")








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
    
    
    
    
    
    # "VARIABLES" TAB explains the variables analyzed in the data sets
    
    
    
    tabPanel(
        
        title = "Summary Statistics",
        
        fluidRow(
            
            column(12,
                   
                   mainPanel(
                       
                       plotOutput("violin_plot")
                       
                   ))
            
        )),
    
    
    
    
# we create the tab for the race visualization and its sidebar layout for the different races:
    
    
    
    tabPanel(
        
        title = "Race Visualization",
        
        h3("How did participants of each race respond"),
        
        br(),
        
        sidebarLayout(
            
            sidebarPanel(
                
                radioButtons(inputId = "k_type", 
                             
                             label = "Race", 
                             
                             choices = c("White", "Black", "Asian", "Hispanic"
                                         
                             ))
                
            ),
            
            
            
            mainPanel(
                
                plotOutput("k_chart")
                
            )
            
        )),
    
    
    
    
    
  
    
    
    
    tabPanel(
        
        title = "Race permutation test",
        
        h3(""),
        
        br(),
        
        sidebarLayout(
            
            sidebarPanel(
                
                radioButtons(inputId = "us_type", 
                             
                             label = "Race:", 
                             
                             choices = c("White", "Black", "Asian", "Hispanic"
                                         
                             ))
                
            ),
            
            
            
            mainPanel(
                
                plotOutput("us_chart")
                
            )
            
        )
        
    )
    
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
    
    
    
    # Content of the "VARIABLES" tab
    
    
    
    output$violin_plot <- renderPlot({
        
   race_violin         
        
    })
    
    
    
    
    
    
    output$k_chart <- renderPlot({
        
        if(input$k_type == "White"){
            
            o_white
            
        }
        
        
        
        else if(input$k_type == "Black"){
            
            o_black
            
        }
        
        
        
        else if(input$k_type == "Asian"){
            
            o_asian
            
        }
        
        
        
        else if(input$k_type == "Hispanic"){
            
            o_hispanic
            
        }
        
        
        
        
        
    })
    
    
    

    
    
    
    output$us_chart <- renderPlot({
        
        if(input$us_type == "White"){
            
            p_white
            
        }
        
        
        
        else if(input$us_type == "Black"){
            
            p_black
            
        }
        
        
        
        else if(input$us_type == "Asian"){
            
            p_asian
            
        }
        
        
        
        else if(input$us_type == "Hispanic"){
            
            p_hispanic
            
        }
        
        
        
    })
    
    
    
}





# Run the application 

shinyApp(ui = ui, server = server)