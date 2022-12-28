
interShiny<-function(){
  # Get the directory from the path of the current file.
 library(this.path)
  cur_dir2 = dirname(this.path())

  # Set the working directory.
 setwd(cur_dir2)
  source('app/server.R')
  shiny::runApp('R/app')

}
