library(shiny)
library(sqldf) 
db <- dbConnect(SQLite(), dbname="Test.sqlite")

dbRemoveTable(db, "average")
dbRemoveTable(db, "average1")
dbRemoveTable(db, "School")
dbRemoveTable(db, "DATASETOFUSER")

dbSendQuery(conn = db, 
            "CREATE TABLE average 
           (songid TEXT, 
           avg TEXT)")
dbSendQuery(conn = db, 
            "CREATE TABLE average1 
            (songid TEXT, 
            avg TEXT, 
            rating TEXT)")

dbWriteTable(conn = db, name = "School", value = "final-user-recommendations.csv", row.names = FALSE, header = TRUE)


dbWriteTable(conn = db, name = "DATASETOFUSER", value = "ydata-ymusic-user-artist-ratings-v1_0.csv", row.names = FALSE, header = TRUE)




# Define server logic for random distribution application
shinyServer(function(input, output) {
  
  # Reactive expression to generate the requested distribution. This is 
  # called whenever the inputs change. The output expressions defined 
  # below then all used the value computed from this expression
  data <- reactive({
    dist <- switch(input$dist,
                   all = runif,
                   norm = rnorm,
                    rating = rlnorm,
                   exp = rexp,
                   rnorm)
    
    
    
    dist(input$n)
  })
  
  data1 <- reactive({
    dist <- switch(input$dist,
                   all = runif,
                   norm = rnorm,
                   rating = rlnorm,
                   exp = rexp,
                   rnorm)
    
    
    
    
  })
  
  
  
  # Generate a plot of the data. Also uses the inputs to build the 
  # plot label. Note that the dependencies on both the inputs and
  # the data reactive expression are both tracked, and all expressions 
  # are called in the sequence implied by the dependency graph
  output$plot <- renderPlot({
   
  })
  
  # Generate a summary of the data
  output$summary <- renderPrint({
    summary(data())
  })
  
  
  output$typeofdrop1 <- renderPrint({
   
    dist <- input$dist
   dist
  })# Generate an HTML table view of the data
  output$table1 <- renderTable({
    t <- input$t
    dist <- input$dist
    dist
    print(dist)
    
    
    
    if(t!=""){
      SQL <- paste("SELECT * from School WHERE user='", t, "';", sep = "")
      print(SQL)
    }
    else
    {
      SQL <- paste("SELECT * from School")
      print(SQL)
    }
   
    
    checking <-dbGetQuery(db,SQL)
    
    
    distinctv2<-dbGetQuery(db, "SELECT * from School")
    distinctv2[is.na(distinctv2)]<-0
    distinctv2
    
    data.frame(x=checking)
    
  })
  
  output$table <- renderTable({
    t <- input$t
    dist <- input$dist
    dist
    print(dist)
  
    Euser <- all.equal(dist,"user")
    Esong <- all.equal(dist,"norm")
    Erating <- all.equal(dist,"rating")
    
    Euser1 <-isTRUE(Euser)
    Esong1 <-isTRUE(Esong)
    Erating1 <-isTRUE(Erating)
    
    
    
    E1 <- all.equal(t,"")
   
    
    E11<-isTRUE(E1)
    
    if(E11){
      SQL <- paste("SELECT * from DATASETOFUSER")
      print(SQL)
    }
    else
    {
      if(Euser1)
      {
        SQL <- paste("SELECT * from DATASETOFUSER WHERE v1='", t, "';", sep = "")
      }
      if(Esong1)
      {
        SQL <- paste("SELECT * from DATASETOFUSER WHERE v2='", t, "';", sep = "")
        SQL1 <- paste("SELECT v2 from DATASETOFUSER WHERE v2='", t, "';", sep = "")
        ct <-dbGetQuery(db,SQL1)
      
        for(i in seq_along(ct)) {
          #  print(v)
          SQLt <- paste("SELECT v3 FROM DATASETOFUSER WHERE v2='", ct[[i]], "';", sep = "")
          print(SQLt)
          checkings <-dbGetQuery(db,SQLt)
          checkings[is.na(checkings)]<-0
          
          
          avgoffirst1=sum(checkings)/length(checkings[[i]])
          
          avgoffirst=length(checkings[[i]])
          sql <- sprintf("insert into average
                         (songid, avg)
                         values ('%s', '%s');",
                         ct[[i]], avgoffirst1)
          
          dbSendQuery(conn = db,sql)
          print(avgoffirst1)
        }
        
        output$typeofdrop11 <- renderPrint({
          SQLt <- paste("SELECT * FROM average WHERE songid='",t, "';", sep = "")
          checking11 <-dbGetQuery(db,SQLt)
          checking11
         
        })
        
        }
      if(Erating1)
      {
        SQL <-  paste("SELECT * from DATASETOFUSER WHERE v3='", t, "';", sep = "")
       
        
       
        
      }
      
     
    #  print(SQL)
      
      
     
    }
    checking1 <-dbGetQuery(db,SQL)
    checking1
    
  
    data.frame(x=checking1)
   # data1.frame(x=checking)
    
  })
  
})
