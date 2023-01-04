


  interShiny= function(...) {
    appDir = system.file("interShiny", package = "interCa")
    if (appDir == "") {
      stop("Could not find example directory. Try re-installing `interCa`.", call. = FALSE)
    }

    shiny::runApp(appDir, display.mode = "normal")
  }

