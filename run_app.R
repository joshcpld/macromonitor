################################################################################
# run_app.R
# Launch Macro Monitor in the browser
################################################################################

options(browser = "C:/Program Files/Google/Chrome/Application/chrome.exe")

shiny::runApp(appDir = "app", launch.browser = TRUE, port = 3838)
