#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for Retroperitoneal Sarcoma Nomogram
ui <- fluidPage(
   
  # Application title
  titlePanel("Retroperitoneal Soft Tissue Sarcoma Nomogram"),
  
  # Sidebar with the necessary parameter: Age, tumor size, Histology subtype, Multifocality, Extent of resection
  sidebarLayout(
    sidebarPanel(
      numericInput("Age", label = h3("Age of patient"), value = 1, step = 1),
      numericInput("Tumor_size", label = h3("Tumor Size, cm"), value = 1, step = 1),
      selectInput("FNCLCC", label = h3("FNCLCC grade"), 
                  choices = list("1" = 1, "2" = 2, "3" = 3), multiple = FALSE
      ),
      selectInput("Histologic_subtype", label = h3("Histologic subtype"), 
                  choices = list("SFT" = 1, "MPNST" = 2, "DD Lipo" = 3, "LMS" = 4, "Other" = 5, 
                                 "UPS" = 6, "WD Lipo" = 7), multiple = FALSE
      ),
      selectInput("Multifocality", label = h3("Multifocality"), 
                  choices = list("No" = 1, "Yes" = 2), multiple = FALSE
      ),
      selectInput("Extent_of_resection", label = h3("Extent of resection"), 
                  choices = list("Complete" = 1, "Incomplete" = 2), multiple = FALSE
      ),
      
      br(),
      br(),
      actionButton("Enter", "Enter", icon("paper-plane"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4"), 
      br(),
      br(),
      actionButton("Clear Screen", "Clear Screen", icon("refresh"))
    ),
    
    # Show the points for the respective paratmeters, total points and 7 years for OS and DFS
    mainPanel(
      h2(textOutput("os_title")),
      textOutput("age_op"),
      textOutput("tumor_size_op"),
      textOutput("fnclcc"),
      textOutput("Histologic"),
      textOutput("Multifocality"),
      textOutput("Extent_of_resection"), 
      textOutput("Total_point"),
      br(),
      br(),
      h2(textOutput("dfs_title")),
      textOutput("tumor_size_dfs"),
      textOutput("fnclcc_dfs"),
      textOutput("Histologic_dfs"),
      textOutput("Multifocality_dfs"),
      textOutput("DFS"), 
      br(), 
      br(),
      p("Adapted from 'Outcome prediction in primary resected retroperitoneal soft tissue sarcoma: 
        histology-specific overall survival and disease-free survival nomograms built on major sarcoma center data sets.
        Gronchi A, Miceli R, Shurell E, Eilber FC, Eilber FR, Anaya DA, Kattan MW, Honore C, Lev DC, Colobo C, Bonvalot S, 
        Mariani L, Pollock RE. J Clin Oncol. 2013 May 1;21 (13): 1649-55. doi:10.1200/JCO.2012.44.3747. Epub 2013 Mar 25.'")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
#Generate the points for all parameters for OS and DFS   
  #Event for Enter button
  observeEvent(
    eventExpr = input[["Enter"]],
    handlerExpr = {
      
      OS_title <- "7 years Overall Survival"
      DFS_title <- "7 years DFS"
      
      age_output <- 
        ifelse((input$Age > 10 & input$Age <= 20), 14 - 3.5/10 * (input$Age -10),
               ifelse((input$Age > 20 & input$Age <= 30), 10.5 - 3.75/10 * (input$Age -20),
                      ifelse((input$Age > 30 & input$Age <= 40), 6.75 - 4.25/10 * (input$Age -30),
                             ifelse((input$Age > 40 & input$Age <= 50), 2.5 - 2.5/10 * (input$Age -40),        
                                    ifelse((input$Age > 50 & input$Age <= 60), 2.5/10 * (input$Age -50),
                                           ifelse((input$Age > 60 & input$Age <= 65), 2.5 + 4.5/5 * (input$Age -60),
                                                  ifelse((input$Age > 65 & input$Age <= 70), 7 + 5/5 * (input$Age -65),
                                                         ifelse((input$Age > 70 & input$Age <= 75), 12 + 6/5 * (input$Age -70),
                                                                ifelse((input$Age > 75 & input$Age <= 80), 18 + 5.75/5 * (input$Age -75), 
                                                                       ifelse((input$Age > 80 & input$Age <= 85), 23.75 + 6.25/5 * (input$Age -80),
                                                                              ifelse((input$Age > 85 & input$Age <= 90), 30 + 6/5 * (input$Age -85),0)))))))))))
      
      tumor_size_output <- 
        ifelse((input$Tumor_size > 0 & input$Tumor_size <= 10), 29/10 * input$Tumor_size,
               ifelse((input$Tumor_size > 10 & input$Tumor_size <= 12), 29 + 6/2 * (input$Tumor_size - 10),
                      ifelse((input$Tumor_size > 12 & input$Tumor_size <= 14), 35 + 5/2 * (input$Tumor_size - 12),
                             ifelse((input$Tumor_size > 14 & input$Tumor_size <= 16), 40 + 4.5/2 * (input$Tumor_size - 14),
                                    ifelse((input$Tumor_size > 16 & input$Tumor_size <= 18), 44.5 + 4.25/2 * (input$Tumor_size - 16),
                                           ifelse((input$Tumor_size > 18 & input$Tumor_size <= 20), 48 + 2.5/2 * (input$Tumor_size - 18),
                                                  ifelse((input$Tumor_size > 20 & input$Tumor_size <= 30), 50.5 + 5/10 * (input$Tumor_size - 20),
                                                         ifelse((input$Tumor_size > 30 & input$Tumor_size <= 40), 55 - 1.25/10 * (input$Tumor_size - 30),
                                                                ifelse((input$Tumor_size > 40 & input$Tumor_size <= 50), 53.75 - 1.25/10 * (input$Tumor_size - 40),
                                                                       ifelse((input$Tumor_size > 50 & input$Tumor_size <= 60), 52.5 - 1.25/10 * (input$Tumor_size - 50),
                                                                              ifelse((input$Tumor_size > 60 & input$Tumor_size <= 70), 51.25 - 1.25/10 * (input$Tumor_size - 60),
                                                                                     ifelse((input$Tumor_size > 70 & input$Tumor_size <= 80), 50 - 1.25/10 * (input$Tumor_size - 70),0 ))))))))))))
      
      
      fnclcc_output <- ifelse(input$FNCLCC == 1, 0,
                              ifelse(input$FNCLCC == 2, 75, 
                                     ifelse(input$FNCLCC == 3, 100, 0)))
      
      
      Histologic_output <- ifelse(input$Histologic_subtype == 1, 0,
                                  ifelse(input$Histologic_subtype == 2, 5, 
                                         ifelse(input$Histologic_subtype == 3, 7.5, 
                                                ifelse(input$Histologic_subtype == 4, 20,
                                                       ifelse(input$Histologic_subtype == 5, 21.75, 
                                                              ifelse(input$Histologic_subtype == 6, 25.5,
                                                                     ifelse(input$Histologic_subtype == 7, 41.75, 0)))))))
      
      
      
      Multifocality_output <- ifelse(input$Multifocality == 1, 0,
                                     ifelse(input$Multifocality == 2, 26.75, 0))
      
      
      
      Extent_of_resection_output <- ifelse(input$Extent_of_resection == 1, 0,
                                           ifelse(input$Extent_of_resection == 2, 15, 0))
      
      total_point <- age_output + tumor_size_output+ fnclcc_output + Histologic_output + Multifocality_output + Extent_of_resection_output
      
      
      OS_7_years <- 
        ifelse((total_point > 0 & total_point <= 19), 0.991,
               ifelse((total_point > 19 & total_point <= 68), 0.99 - 0.04/49 * (total_point - 19),
                      ifelse((total_point > 69 & total_point <= 90), 0.95 - 0.05/21 * (total_point - 69), 
                             ifelse((total_point > 90 & total_point <= 113), 0.9 - 0.1/23 * (total_point - 90),
                                    ifelse((total_point > 113 & total_point <= 128), 0.8 - 0.1/15 * (total_point - 113),
                                           ifelse((total_point > 128 & total_point <= 138), 0.7 - 0.1/10 * (total_point - 128), 
                                                  ifelse((total_point > 138 & total_point <= 148), 0.6 - 0.1/10 * (total_point - 138), 
                                                         ifelse((total_point > 148 & total_point <= 158), 0.5 - 0.1/10 * (total_point - 148),
                                                                ifelse((total_point > 158 & total_point <= 164), 0.4 - 0.1/6 * (total_point - 158), 
                                                                       ifelse((total_point > 164 & total_point <= 173), 0.3 - 0.1/9 * (total_point - 164), 
                                                                              ifelse((total_point > 173 & total_point <= 184), 0.2 - 0.1/11 * (total_point - 173),
                                                                                     ifelse((total_point > 184 & total_point <= 192), 0.1 - 0.05/8 * (total_point - 184),
                                                                                            ifelse((total_point > 192 & total_point <= 206), 0.05 - 0.04/14 * (total_point - 192), 
                                                                                                   ifelse((total_point > 206), 0.009,"Unknown"))))))))))))))
    
      tumor_size_dfs <- 
        ifelse((input$Tumor_size > 0 & input$Tumor_size <= 10), 26.75/10 * input$Tumor_size,
               ifelse((input$Tumor_size > 10 & input$Tumor_size <= 12), 26.75 + 4.25/2 * (input$Tumor_size - 10),
                      ifelse((input$Tumor_size > 12 & input$Tumor_size <= 14), 31 + 5.75/2 * (input$Tumor_size - 12),
                             ifelse((input$Tumor_size > 14 & input$Tumor_size <= 16), 36.75 + 3.25/2 * (input$Tumor_size - 14),
                                    ifelse((input$Tumor_size > 16 & input$Tumor_size <= 18), 40 + 3/2 * (input$Tumor_size - 16),
                                           ifelse((input$Tumor_size > 18 & input$Tumor_size <= 20), 43 + 2.5/2 * (input$Tumor_size - 18),
                                                  ifelse((input$Tumor_size > 20 & input$Tumor_size <= 30), 45.5 + 4.5/10 * (input$Tumor_size - 20),
                                                         ifelse((input$Tumor_size > 30 & input$Tumor_size <= 80), 50 - 5.5/50 * (input$Tumor_size - 30), 0 ))))))))
      
      
      fnclcc_dfs <- ifelse(input$FNCLCC == 1, 0,
                              ifelse(input$FNCLCC == 2, 62, 
                                     ifelse(input$FNCLCC == 3, 100, 0)))
      
      
      Histologic_dfs <- ifelse(input$Histologic_subtype == 1, 0,
                                  ifelse(input$Histologic_subtype == 2, 1, 
                                         ifelse(input$Histologic_subtype == 3, 27.5, 
                                                ifelse(input$Histologic_subtype == 4, 40,
                                                       ifelse(input$Histologic_subtype == 5, 32.5, 
                                                              ifelse(input$Histologic_subtype == 6, 42.5,
                                                                     ifelse(input$Histologic_subtype == 7, 50, 0)))))))
      
      Multifocality_dfs <- ifelse(input$Multifocality == 1, 0,
                                     ifelse(input$Multifocality == 2, 47.5, 0))
      
      
      total_point_dfs <- tumor_size_dfs + fnclcc_dfs +  Histologic_dfs + Multifocality_dfs
      
      
      DFS_7_years <- 
        ifelse((total_point_dfs > 0 & total_point_dfs <= 2), 0.96,
               ifelse((total_point_dfs > 2 & total_point_dfs <= 35), 0.95 - 0.05/33 * (total_point_dfs - 2),
                      ifelse((total_point_dfs > 35 & total_point_dfs <= 70), 0.9 - 0.1/35 * (total_point_dfs - 35), 
                             ifelse((total_point_dfs > 70 & total_point_dfs <= 91), 0.8 - 0.1/21 * (total_point_dfs - 70),
                                    ifelse((total_point_dfs > 91 & total_point_dfs <= 108), 0.7 - 0.1/17 * (total_point_dfs - 91),
                                           ifelse((total_point_dfs > 108 & total_point_dfs <= 122), 0.6 - 0.1/14 * (total_point_dfs - 108), 
                                                  ifelse((total_point_dfs > 122 & total_point_dfs <= 135), 0.5 - 0.1/13 * (total_point_dfs - 122), 
                                                         ifelse((total_point_dfs > 135 & total_point_dfs <= 148), 0.4 - 0.1/13 * (total_point_dfs - 135),
                                                                ifelse((total_point_dfs > 148 & total_point_dfs <= 161), 0.3 - 0.1/13 * (total_point_dfs - 148), 
                                                                       ifelse((total_point_dfs > 161 & total_point_dfs <= 179), 0.2 - 0.1/18 * (total_point_dfs - 161), 
                                                                              ifelse((total_point_dfs > 179 & total_point_dfs <= 190), 0.1 - 0.05/11 * (total_point_dfs - 179),
                                                                                     ifelse((total_point_dfs > 190 & total_point_dfs <= 210), 0.05 - 0.05/20 * (total_point_dfs - 190),
                                                                                            ifelse((total_point_dfs > 210), 0.009,"Unknown")))))))))))))
   
  #Output for OS and DFS Title
  output$os_title <- renderText({ 
        paste(OS_title)
      })    
  
  output$dfs_title <- renderText({ 
    paste(DFS_title)
  })        
      
      
  #Output for Age for OS
  output$age_op <- renderText({ 
    paste("Points based on age of patient: ", age_output)
  })
  
  #output for fnclcc for OS
  output$fnclcc <- renderText({ 
    paste("Points based on FNCLCC Grade: ", fnclcc_output)
    
  })
  
  #output for Histologic for OS
  output$Histologic <- renderText({ 
    paste("Points based on Histologic subtype: ", Histologic_output)
  })
  
  #output for Multifocality for OS
  output$Multifocality <- renderText({ 
    paste("Points based on Multifocality: ", Multifocality_output)
    
  })
  
  #output for Extent of resection for OS
  output$Extent_of_resection <- renderText({ 
    paste("Points based on Extent of resection: ", Extent_of_resection_output)
    
  })
  
  #output for total point and OS
  output$Total_point <- renderText({
  paste("Total points: ", total_point, " | 7 year OS: ", round(OS_7_years, digits = 3))
})
  

  #Output for tumor size for DFS  
  output$tumor_size_dfs <- renderText({ 
    paste("Points based on tumor size: ", tumor_size_dfs)
    
  })
  
  #Output for fnclcc for DFS
  output$fnclcc_dfs <- renderText({ 
    paste("Points based on FNCLCC Grade: ", fnclcc_dfs)
  })  
  
  
  #Output for Histologic for DFS
  output$Histologic_dfs <- renderText({ 
    paste("Points based on Histologic subtype: ", Histologic_dfs)
    
  })

  #Output for Multifocality for DFS  
  output$Multifocality_dfs <- renderText({ 
    paste("Points based on Multifocality: ", Multifocality_dfs)
  })  
  
  
  #Output for DFS  
  output$DFS <- renderText({
    paste("Total points: ", total_point_dfs, "| 7 years DFS: ",round( DFS_7_years, digits = 3))
  })
  
    })
  
  
#Event for Reset button  
  observeEvent(
    eventExpr = input[["Clear Screen"]],
    handlerExpr = {
      
      #updateNumericInput("Age", value = 1)
      #updateNumericInput("Tumor_size", value = 1)

      #Output for OS and DFS Title
      output$os_title <- renderText({ 
        paste("")
      })    
      
      output$dfs_title <- renderText({ 
        paste("")
      })        
      
      #Output for Age for OS
      output$age_op <- renderText({ 
        paste("")
      })
      
      #output for fnclcc for OS
      output$fnclcc <- renderText({ 
        paste("")
        
      })
      
      #output for Histologic for OS
      output$Histologic <- renderText({ 
        paste("")
      })
      
      #output for Multifocality for OS
      output$Multifocality <- renderText({ 
        paste("")
        
      })
      
      #output for Extent of resection for OS
      output$Extent_of_resection <- renderText({ 
        paste("")
        
      })
      
      #output for total point and OS
      output$Total_point <- renderText({
        paste("")
      })
      
      
      #Output for tumor size for DFS  
      output$tumor_size_dfs <- renderText({ 
        paste("")
        
      })
      
      #Output for fnclcc for DFS
      output$fnclcc_dfs <- renderText({ 
        paste("")
      })  
      
      
      #Output for Histologic for DFS
      output$Histologic_dfs <- renderText({ 
        paste("")
        
      })
      
      #Output for Multifocality for DFS  
      output$Multifocality_dfs <- renderText({ 
        paste("")
      })  
      
      
      #Output for DFS  
      output$DFS <- renderText({
        paste("")
      })
      
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

