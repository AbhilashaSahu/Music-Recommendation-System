library(shiny)
library(sqldf) 
db <- dbConnect(SQLite(), dbname="Test.sqlite")
#dbSendQuery(conn = db, 
 #           "CREATE TABLE average 
 #           (songid TEXT, 
   #         avg TEXT)")

distinctv2<-dbGetQuery(db, "SELECT distinct(v2) from School")
distinctv2[is.na(distinctv2)]<-0
distinctv2

for(i in seq_along(distinctv2)) {
  ct<- distinctv2[i]
  ct[is.na(ct)]<-0
  
}
ct

c1 <- paste("SELECT distinct(v2) FROM School ")
print(c1)
c <-dbGetQuery(db,c1)

for (i in seq_along(distinctv2)){
  
  c.element <- c[i]
  
  
}



for(i in seq_along(ct)) {
  #  print(v)
  SQL <- paste("SELECT v3 FROM School WHERE v2='", ct[[i]], "';", sep = "")
  print(SQL)
  checking <-dbGetQuery(db,SQL)
  checking[is.na(checking)]<-0
  
  
  avgoffirst1=sum(checking)/length(checking[[1]])
  avgoffirst=length(checking[[1]])
  sql <- sprintf("insert into average
                 (songid, avg)
                 values ('%s', '%s');",
                 ct[[i]], avgoffirst1)
  
  dbSendQuery(conn = db,sql)
  print(avgoffirst1)
}

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
      SQL <- paste("SELECT distinct(songid),avg from average WHERE songid='", t, "';", sep = "")
      print(SQL)
    }
    else
    {
      SQL <- paste("SELECT distinct(songid),avg from average")
      print(SQL)
    }
   
    
    checking <-dbGetQuery(db,SQL)
    
    
    distinctv2<-dbGetQuery(db, "SELECT distinct(v2) from School")
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
      SQL <- paste("SELECT * from School")
      print(SQL)
    }
    else
    {
      if(Euser1)
      {
        SQL <-  paste("SELECT v1,v2 from School WHERE v1='", t, "';", sep = "")
      }
      if(Esong1)
      {
        SQL <-  paste("SELECT v1,v2 from School WHERE v2='", t, "';", sep = "")
      }
      if(Erating1)
      {
        SQL <-  paste("SELECT v1,v2 from School WHERE v3='", t, "';", sep = "")
      }
      
     
      print(SQL)
      
      
     
    }
    checking1 <-dbGetQuery(db,SQL)
    checking1[is.na(checking1)]<-0
    checking1
    
  
    data.frame(x=checking1)
   # data1.frame(x=checking)
    
  })
  
})
