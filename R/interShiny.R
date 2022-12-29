
# interShiny<-function(){
#   # Get the directory from the path of the current file.
#  #library(this.path)
#   #cur_dir2 = dirname(this.path())
#
#   # Set the working directory.
#  #setwd(cur_dir2)
#   #source('app/server.R')
#
# #shinyApp(ui = system.file("R", "ui.R", package = "test"),
#        #  server = system.file("R", "server.R", package = "test"))

#shiny::runApp("R")

  interShiny= function(...) {
    appDir = system.file("interShiny", package = "test")
    if (appDir == "") {
      stop("Could not find example directory. Try re-installing `shinyMlr`.", call. = FALSE)
    }

    shiny::runApp(appDir, display.mode = "normal")
  }

