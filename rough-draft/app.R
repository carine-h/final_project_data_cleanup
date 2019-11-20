library(shiny)

library(shinythemes)

library(tidyverse)

library(ggplot2)







# Load data




# Datasets scraped from the Spotify API in a Python script in extractions folder

# Due to the immense difficulty and time taken to run the data extraction

# scripts, this is a vanilla app




#kpop_top10 <- read.csv("data/kpop_top10.csv")

#us_top10 <- read.csv("data/us_top10.csv")







# Import data for K-pop Top 10 Comparison (observed Histograms)




o_white <- read_rds("./o_white.rds")

o_black <- read_rds("./o_black.rds")

o_asian <- read_rds("./o_asian.rds")

o_hispanic <- read_rds("./o_hispanic.rds")





# Import data for Billboard Top 10 Comparison (Permutation test)




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
                   
                   wellPanel(
                       
                       htmlOutput("variables")
                       
                   ))
            
        )),
    
    
    
    
    
    # "BEYOND K-POP?" TAB compares BTS' statistics with those of prominent Korean artists
    
    
    
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
    
    
    
    
    
    # "BEYOND BILLBOARD?" TAB compares BTS' statistics with those of prominent artists in Billboard
    
    
    
    tabPanel(
        
        title = "Race permutation test",
        
        h3("How Does BTS Compare in the Top 10 Billboard Artists?"),
        
        br(),
        
        sidebarLayout(
            
            sidebarPanel(
                
                radioButtons(inputId = "us_type", 
                             
                             label = "Musical Attribute:", 
                             
                             choices = c("White", "Black", "Asian", "Hispanic"
                                         
                             ))
                
            ),
            
            
            
            mainPanel(
                
                plotOutput("us_chart")
                
            )
            
        )
        
    )
    
)





# Define server logic required to create user interface elements




server <- function(input, output) {
    
    
    
    # Content of the "ABOUT" tab
    
    
    
    output$about <- renderUI({
        
        HTML(paste(
            
            h2("Overview"),
            
            br(),
            
            "Beyond the Stage (BTS) searches into various possible explanations for the rapid rise of the Korean music group", tags$a("BTS (방탄소년단, Beyond the Scene)", href = "https://en.wikipedia.org/wiki/BTS_(band)"), ". With ",  tags$a("3 new Guinness World Records", href =  "https://www.billboard.com/articles/news/bts/8507811/bts-break-3-guinness-world-records-boy-with-luv"), "in just 2019,", tags$a ("4 Billboard Music Awards", href = "https://www.billboard.com/video/bts-wins-best-duo-or-group-about-billboard-music-award-2019-on-billboardyd-sourceflv-8509697"), ",", tags$a("YouTube", href = "https://www.youtube.com/watch?v=62E_xyj_oDA&list=PL_Cqw69_m_yzbMVGvQA8QWrL_HdVXJQX7"), "records for numerous music video releases, and", tags$a("sold-out concerts in some of the largest stadiums in the world", href = "https://www.forbes.com/sites/caitlinkelley/2019/03/02/bts-sold-out-america-europe-world-tour-love-yourself/#660891c55af3"), ", what distinguishes this group of seven from other artists?",
            
            br(),
            
            br(),
            
            h3("Summary of Findings"),
            
            br(),
            
            h4("Comparison to Top 10 Korean Artists"),
            
            p("BTS is quite similar to the other Korean Artists (voted Top 10 through the Melon Music Awards), most notably in Speechiness."),
            
            p("Based on Spotify data, BTS members employ musical attributes similar to other Korean popular music artists. They distinguish themselves with rap, and a higher emphasis on spoken word in their tracks. From the concerns of youth in a neoliberal Korea, to songs encouraging every listener to “Love Yourself” in alliance with their UNICEF campaign, a higher Speechiness value supports articles attributing the group’s success to their social messages."),
            
            br(),
            
            h4("Comparison to Top 10 Billboard Artists"),
            
            p("BTS is quite similar to the 2018 Top 10 Billboard-selected artists (almost completely dominated by American artists) on several attributes with notable exceptions."),
            
            p("Where BTS stands out are in the attributes of Energy and Liveness, where the group maintains the highest value. Most U.S. news sources covering BTS’ performances in America have called attention to their high-energy live performances. Here we find these qualities embedded in their music."),
            
            p("BTS is on the higher end of Speechiness yet again, although lower than Drake and XXXTENTACION. Although BTS’ songs span almost every music genre, their initial emphasis on rap remains salient in their tracks. BTS is also on the positive end for Valence values, aligning with their aim to uplift their listeners, even while interrogating complex issues. The group has the lowest value for Acousticness, a common perception associated with Korean popular music."),
            
            br(),
            
            h3("Sources"),
            
            a("Spotify", href = "https://open.spotify.com/artist/3Nrfpe0tUJi4K4DXYWgMUX"),
            
            br(),
            
            a("Wikipedia", href = "https://en.wikipedia.org/wiki/BTS_World_Tour:_Love_Yourself"),
            
            br(),
            
            a("Billboard", href = "https://www.billboard.com/charts/year-end/2018/top-artists"),
            
            br(),
            
            a("Melon (멜론)", href = "https://www.soompi.com/article/1262803wpp/2018-melon-music-awards-announces-winners-top-10-artists"),
            
            br(),
            
            a("Haebichan Jung's kpopclassifier", href = "https://github.com/haebichan/kpopclassifier.git"),
            
            br(),
            
            br(),
            
            a("Browse Source Code Here", href = "https://github.com/marzipan241/beyond-the-stage.git"),
            
            br(),
            
            br(),
            
            h4("About"),
            
            p("Margaret Sun is a sophomore studying Sociology at Harvard College who is immensely interested in internet mobilization (and alliteration, apparently), and hopes to take time in the future to build on this project. There are plenty more variables to be explored, and any suggestions on where to expand would be greatly appreciated!")
            
        ))
        
    })
    
    
    
    # Content of the "VARIABLES" tab
    
    
    
    output$variables <- renderUI({
        
        HTML(paste(
            
            h3("Variables analyzed come from the Spotify API. Find their explanations below:"),
            
            br(),
            
            p(""),
            
            h4("Danceability"),
            
            p("On a scale of 0 (least danceable) to 1 (most danceable), a combination of beat strength, overall regularity, rhythm stability, and tempo informs Danceability."),
            
            br(),
            
            h4("Energy"),
            
            p("On a scale of 0 (least energy) to 1 (most energy), dynamic range, perceived loudness, timbre, and general entropy inform Energy. Classical music would score low here, while death metal high."),
            
            br(),
            
            h4("Speechiness"),
            
            p("On a scale of 0 (least speech) to 1 (most speech), Speechiness measures spoken words in a track. 0.66 or above indicates tracks almost entirely composed of spoken words, while the 0.33 to 0.66 range contains both music and speech."),
            
            br(),
            
            h4("Acousticness"),
            
            p("On a scale of 0 (least acoustic) to 1 (most acoustic), the general distribution of values is positively skewed, with most values closer to 0 because of use of electronic amplification in most songs."),
            
            br(),
            
            h4("Instrumentalness"),
            
            p("On a scale of 0 (least instrumental) to 1 (no vocal content), Instrumentalness measures whether tracks have vocals. Most songs on Spotify fall closer to 0 due to lyrics. “Ooh” and “aah” filler sounds count as instrumental."),
            
            br(),
            
            h4("Liveness"),
            
            p("On a scale of 0 (least likely performed live) to 1 (performed live), values above 0.8 for Liveness means the track is most likely live. This is also a positively skewed distribution."),
            
            br(),
            
            h4("Valence"),
            
            p("On a scale of 0 to 1, high Valence songs sound positive (euphoric, cheerful), which low Valence songs sounds negative (angry, depressed). "),
            
            br(),
            
            h4("Tempo"),
            
            p("On a scale of 0 to 1, Tempo measures a track’s beats per minute (BPM). Tempo is also the pace or speed of a track in relation to the average beat duration."),
            
            br(),
            
            h4("Duration (ms)"),
            
            p("Duration measures the length of the track in milliseconds."),
            
            br()
            
        ))
        
    })
    
    
    
    # References to RDS files relevant to the "BEYOND K-POP?" tab
    
    
    
    output$k_chart <- renderPlot({
        
        if(input$k_type == "White"){
            
            white_observed_histogram
            
        }
        
        
        
        else if(input$k_type == "Black"){
            
            black_observed_histogram
            
        }
        
        
        
        else if(input$k_type == "Asian"){
            
            asian_observed_histogram
            
        }
        
        
        
        else if(input$k_type == "Hispanic"){
            
            hispanic_observed_histogram
            
        }
        
        
        
        
        
    })
    
    
    
    # References to RDS files relevant to the "BEYOND BILLBOARD?" tab
    
    
    
    output$us_chart <- renderPlot({
        
        if(input$us_type == "White"){
            
            white_permutation_plot
            
        }
        
        
        
        else if(input$us_type == "Black"){
            
            black_permutation_plot
            
        }
        
        
        
        else if(input$us_type == "Asian"){
            
            asian_permutation_plot
            
        }
        
        
        
        else if(input$us_type == "Hispanic"){
            
            hispanic_permutation_plot
            
        }
        
        
        
    })
    
    
    
}





# Run the application 

shinyApp(ui = ui, server = server)