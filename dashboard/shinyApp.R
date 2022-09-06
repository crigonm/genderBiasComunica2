# Ejecutar la aplicación 
source("server.R")
source("ui.R")
shinyApp(ui = ui, server = server)