library(shiny)
library(sqldf) 
db <- dbConnect(SQLite(), dbname="Test.sqlite")
#dbSendQuery(conn = db, 
#           "CREATE TABLE average 
#           (songid TEXT, 
#         avg TEXT)")

dbRemoveTable(db, "School")

dbWriteTable(conn = db, name = "School", value = "final-user-recommendations.csv", row.names = FALSE, header = TRUE)










distinctv2<-dbGetQuery(db, "SELECT distinct(v2) from School")
distinctv2[is.na(distinctv2)]<-0
distinctv2






