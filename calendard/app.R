date_picker <- function(time = FALSE) {

  withr::with_package("shiny",{
    # Define UI for application that draws a histogram
    ui <- fluidPage(
      shinyWidgets::airDatepickerInput(
        inputId = "widget",
        timepicker = time,
        inline = TRUE
      ),
      if(time) actionButton("button", "ok")
    )
    server <- function(input, output, session) {
      my_date <- renderText(input$widget)
      if(time) {
        observeEvent(input$button, {
            stopApp(as.POSIXct(as.numeric(my_date()), origin = "1970-01-01 00::00:00"))
        })
      } else {
        observeEvent(input$widget, {
            stopApp(as.Date(as.numeric(my_date()), origin = "1970-01-01"))
        })
      }
    }
    runGadget(ui, server)
  })
}

x <- date_picker(TRUE)
print(x)
y <- date_picker()
print(y)
