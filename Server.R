library(DT) #For data table
# install.packages("ggcorrplot")
library(ggcorrplot) #for correlation matrix

credentials <- data.frame(
  user=c("admin1","admin2"),
  password=c("admin1","admin2"),
  stringsAsFactors = FALSE
)
server <- function(input, output, session){
  # structure
  #Authentication
  res_auth<-secure_server(check_credentials = check_credentials(credentials))
  output$structure <- renderPrint(
    #STRUCTURE  OF THE DATA
    my_data %>%
      str()
  )
  
  #summary
  output$summary <- renderPrint(
    my_data %>%
      summary()
  )
  
  #dataTable
  # output$dataT < - renderDataTable(
  #   my_data
  # )
  output$dataT <- renderDataTable({
    datatable(my_data, options = list(pageLength = 5))
  })
  
  output$histplot <- renderPlotly({
    p1<-my_data %>%
      plot_ly() %>%
      add_histogram(~get(input$var1)) %>%
      layout(xaxis = list(title = paste(input$var1)))
    
    #boxPlot
    p2<-my_data %>%
      plot_ly()%>%
      add_boxplot(x = ~get(input$var1)) %>%
      layout(yaxis = list(showticklables = F))
    
    
    #stacking plot one upon another
    subplot(p2,p1, nrows = 2,shareX = TRUE) %>%
      hide_legend() %>%
      layout(title="Distribution chart - Histogram and Boxplot",
             yaxis = list(title="Frequency"))
  })
  
  ### Bar Charts - State wise trend
  output$bar <- renderPlotly({
    my_data %>% 
      plot_ly() %>% 
      add_bars(x=~State, y=~get(input$var2)) %>% 
      layout(title = paste("Statewise Arrests for", input$var2),
             xaxis = list(title = "State"),
             yaxis = list(title = paste(input$var2, "Arrests per 100,000 residents") ))
  })
  
  ###Scatter Plot
  output$scatter <- renderPlotly({
    p = my_data %>% 
      ggplot(aes(x=get(input$var3), y=get(input$var4))) +
      geom_point() +
      geom_smooth(method=get(input$fit)) +
      labs(title = paste("Relation b/w", input$var3 , "and" , input$var4),
           x = input$var3,
           y = input$var4) +
      theme(plot.title = element_text(size = 10, hjust = 0.5))
    
    
    # applied ggplot to make it interactive
    ggplotly(p)
})
  
  output$cor <- renderPlotly({
    my_df <- my_data %>% select_if(is.numeric)  # Ensure only numeric columns
    
    # Compute correlation matrix
    corr <- round(cor(my_df, use = "complete.obs"), 1)
    
    # Compute correlation p-values
    p.mat <- cor_pmat(my_df)
    
    # Create correlation plot
    corr.plot <- ggcorrplot(
      corr, 
      hc.order = TRUE, 
      lab = TRUE,
      outline.col = "white",
      p.mat = p.mat
    )
    
    ggplotly(corr.plot)
  })
  
  
  
  # Rendering the box header  
  output$head1 <- renderText(
    paste("5 states with high rate of", input$var2, "Arrests")
  )
  
  # Rendering the box header 
  output$head2 <- renderText(
    paste("5 states with low rate of", input$var2, "Arrests")
  )
  # Rendering table with 5 states with high arrests for specific crime type
  output$top5 <- renderTable({
    
    my_data %>% 
      select(State, input$var2) %>% 
      arrange(desc(get(input$var2))) %>% 
      head(5)
    
  })
  
  # Rendering table with 5 states with low arrests for specific crime type
  output$low5 <- renderTable({
    
    my_data %>% 
      select(State, input$var2) %>% 
      arrange(get(input$var2)) %>% 
      head(5)
    
  })
  # Choropleth map
  output$map_plot <- renderPlot({
    new_join %>% 
      ggplot(aes(x=long, y=lat,fill=get(input$crimetype) , group = group)) +
      geom_polygon(color="black", size=0.4) +
      scale_fill_gradient(low="#73A5C6", high="#001B3A", name = paste(input$crimetype, "Arrest rate")) +
      theme_void() +
      labs(title = paste("Choropleth map of", input$crimetype , " Arrests per 100,000 residents by state in 1973")) +
      theme(
        plot.title = element_textbox_simple(face="bold", 
                                            size=18,
                                            halign=0.5),
        
        legend.position = c(0.2, 0.1),
        legend.direction = "horizontal"
        
      ) +
      geom_text(aes(x=x, y=y, label=abb), size = 4, color="white")
    
    
    
  })
  
  
}