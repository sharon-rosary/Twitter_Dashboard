#load the revelant libraries
library(shiny)  #for web applications
library(shinydashboard)
library(DataExplorer)
library(scales)
library(dplyr)
library(ggplot2)


#get the working directory
getwd()

# read the data from the directory
SocialMedia<- read_csv("SocialMedia (1).csv")
View(SocialMedia)

############################################################################################################################
# The customizations for this dashboard are : 3 value boxes, two ggplots, dashboard icon on the left #
############################################################################################################################

#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Twitter Dashboard", titleWidth=195)  

#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
    
  )
)



frow1 <- fluidRow(
   valueBoxOutput("value1")
  ,valueBoxOutput("value2")
  ,valueBoxOutput("value3")
)

frow2 <- fluidRow(
  box(selectInput("v_select",label="Top Tweets By",choices = c("Likes","Engagements","Media Engagements","URL clicks"),selected="URL clicks"),width=4)
  
  ,box(
  title = "Number of tweets per weekday"
  ,status = "primary"
  ,solidHeader = TRUE 
  ,collapsible = TRUE 
  ,plotOutput("tweetperweekday", height = "300px")
) 
)
frow3 <- fluidRow(
  box(
    title = "Total ten tweets"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("totaltweets", height = "600px")
  )
  
  ,box(
    title = "Impressions vs Engagements"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("impeng", height = "300px")
  ) 
)


# combine the two fluid rows to make the body
body <- dashboardBody(frow1, frow2, frow3)

#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'This is my Page title', header, sidebar, body, skin='blue')

# create the server functions for the dashboard  
server <- function(input, output) { 
  
  #some data manipulation to derive the values on the boxes on top
  total_tweets      <- nrow(SocialMedia)
  average_imp_rate  <- summarise(SocialMedia, Average = mean(impressions, na.rm = T))
  average_eng_rate  <- summarise(SocialMedia, Average = mean(engagement_rate*100, na.rm = T))


  #creating the valueBoxOutput content
  output$value1 <- renderValueBox({
    valueBox(
      formatC(total_tweets, format="d", big.mark=',')
      ,'Total number of tweets posted'
      ,color = "aqua")
  })
  
  output$value2 <- renderValueBox({
    
    valueBox(
      formatC(average_imp_rate, format="d", big.mark=',')
      ,'Average impression rate (No. of places where it was shown)'
      ,color = "aqua")
  })
  
  output$value3 <- renderValueBox({
    
    valueBox(
      formatC(average_eng_rate, format="d", big.mark=',')
      ,'Average engagement rate (engagements like replies, retweets, likes)'
      ,color = "aqua")
  })
  
  #creating the plotOutput content
  
  output$totaltweets <- renderPlot({
    if (input$v_select == "Likes"){
      new_data_url <- head(SocialMedia[order(SocialMedia$likes, decreasing=TRUE), ], 10)
      new_data_url$topten <- new_data_url$likes
    }else if (input$v_select == "Engagements"){
      new_data_url <- head(SocialMedia[order(SocialMedia$engagements, decreasing=TRUE), ], 10)
      new_data_url$topten <- new_data_url$engagements 
    }else if (input$v_select == "Media Engagements"){
      new_data_url <- head(SocialMedia[order(SocialMedia$media_engagements, decreasing=TRUE), ], 10)
      new_data_url$topten <- new_data_url$media_engagements 
    } else {
      new_data_url <- head(SocialMedia[order(SocialMedia$url_clicks, decreasing=TRUE), ], 10)
      new_data_url$topten <- new_data_url$url_clicks
    }
    
    ggplot(new_data_url,aes(x=reorder(Tweet,topten),y=topten))+
    ggtitle("Top ten tweets")+
    labs(x="Tweets ",y=" ")+
    geom_bar(stat="identity",fill="royalblue4",width = 0.10)+
    geom_text(mapping=aes(label=url_clicks),position=position_dodge(width=0.2),cex=3,hjust=-0.1)+
    scale_x_discrete(label = function(x) stringr::str_trunc(x, 12)) +
    theme(axis.text.x = element_text( hjust = 1, vjust = 0.5)) +
    coord_flip()
  })

  output$tweetperweekday <- renderPlot({
    days <- weekdays(as.Date(SocialMedia$time))
    tweet_weekday <- data.frame(table(days))
    tweet_weekday$days <- factor(tweet_weekday$days, levels= c("Sunday", "Monday", 
                                             "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
    
    tweet_weekday <- tweet_weekday[order(tweet_weekday$days), ]
    
    tweet_weekday %>%
      ggplot(aes(x=days,y=Freq,)) + 
      geom_point(size = 10, colour = "royalblue4") + 
      geom_segment( aes(x=days, xend=days, y=0, yend=Freq),size = 2.5, color="royalblue4")+
      labs(y= "Number of Tweets", x="Weekday")
  })
  output$impeng <- renderPlot({
    plot(SocialMedia$engagements ~ SocialMedia$impressions, pch = 19, col = "black")
  })

  
}

shinyApp(ui, server)