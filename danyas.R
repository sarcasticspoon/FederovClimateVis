setwd("/Users/ivyfan/Yale/Danya")
library(shiny)
library(pracma)
library(shinyWidgets)

#clear all enviroment variables
rm(list=ls())

#function
fun <- function(t,y){ 
  n <- ceil(t-t0+1) 
  toReturn <- -y/tau+a*log(co2[n]/rco2)+b*nino[n]+c*saod[n]+d
  return(toReturn)
}

#global a b c d tau co2 rco2 saod nino t0

myData <- read.table("simple_model_data1880to2015.txt")
time <- myData[,1]
sat_obs <- myData[,2]
co2 <- myData[,3]
nino <- myData[,4]
saod <- myData[,5]
a <- 1.78
b <- 0.122
c <- -1.39

rco2 <- 290.7
tau <- 2
d <- -0.16
t0 <-time[1]
tfin <- max(time)
sat0 <- sat_obs[1]

#create the arrays for the Into the Future tab, with differently named variables
#these will be denoted with 0 after the variable names if supposed to be the same
rco20 <- 320 #this is supposed to be rco2
tau0 <- 4 #this is supposed to be the same as tau, just added a zero to differentiate from above in the case that they're not actually the same
tau1 <- 4
tau2 <- 56
a0 <- 1.21
b0 <- 0.12
c0 <- -1.68
d0 <- 0.03

histTime <- time
sat_obs1 <- sat_obs

GMST <- sat_obs1[1]
GMSTd <- mean(sat_obs1[1:20])

# read in the data for the current time period
for (i in 2:126) {
  f <- co2[i]
  g <- nino[i]
  h <- saod[i]
  GMST[i] <- GMST[i-1] - GMST[i-1]/tau + a*log(f/rco2) + b*g + c*h + d + (GMSTd[i-1]-GMST[i-1])/tau1
  GMSTd[i] <- GMSTd[i-1] + (GMST[i-1] - GMSTd[i-1])/tau2
}
GMSTdstart <- GMSTd

#now we begin the graphs for the future pathways @TOM PLEASE take a look at this and see if there's a better way to do this
#set up the data for eq co2 2.6 pathway
futureTime <- read.table("RCP3PD_MIDYR_CONC.txt")[116:536, 1]
co2eq_26 <- read.table("RCP3PD_MIDYR_CONC.txt")[116:536, 2]
GMST2_26 <- sat_obs1[1:126]
GMSTd_eq_26 <- GMSTdstart
for (i in 127:421) {
  f <- co2eq_26[i]
  g <- 0
  h <- 0
  GMST2_26[i] <- GMST2_26[i-1] - GMST2_26[i-1]/tau + a*log(f/rco2) + b*g + c*h + d + (GMSTd_eq_26[i-1]-GMST2_26[i-1])/tau1
  GMSTd_eq_26[i] <- GMSTd_eq_26[i-1] + (GMST2_26[i-1]-GMSTd_eq_26[i-1])/tau2
}

#pathway 4.5
co2eq_45 <- read.table("RCP45_MIDYR_CONC.txt")[116:536, 2]
GMSTeq_45 <- sat_obs1[1:126]
GMSTd_eq_45 <- GMSTdstart
for (i in 127:421) {
  f <- co2eq_45[i]
  g <- 0
  h <- 0
  GMSTeq_45[i] <- GMSTeq_45[i-1] - GMSTeq_45[i-1]/tau + a*log(f/rco2) + b*g + c*h + d + (GMSTd_eq_45[i-1]-GMSTeq_45[i-1])/tau1
  GMSTd_eq_45[i] <- GMSTd_eq_45[i-1] + (GMSTeq_45[i-1]-GMSTd_eq_45[i-1])/tau2
}

#pathway 6.0
co2eq_6 <- read.table("RCP6_MIDYR_CONC.txt")[116:536, 2]
GMSTeq_6 <- sat_obs1[1:126]
GMSTd_eq_6 <- GMSTdstart
for (i in 127:421) {
  f <- co2eq_6[i]
  g <- 0
  h <- 0
  GMSTeq_6[i] <- GMSTeq_6[i-1] - GMSTeq_6[i-1]/tau + a*log(f/rco2) + b*g + c*h + d + (GMSTd_eq_6[i-1]-GMSTeq_6[i-1])/tau1
  GMSTd_eq_6[i] <- GMSTd_eq_6[i-1] + (GMSTeq_6[i-1]-GMSTd_eq_6[i-1])/tau2
}

#pathway 8.5
co2eq_85 <- read.table("RCP85_MIDYR_CONC.txt")[116:536, 2]
GMSTeq_85 <- sat_obs1[1:126]
GMSTd_eq_85 <- GMSTdstart
for (i in 127:421) {
  f <- co2eq_85[i]
  g <- 0
  h <- 0
  GMSTeq_85[i] <- GMSTeq_85[i-1] - GMSTeq_85[i-1]/tau + a*log(f/rco2) + b*g + c*h + d + (GMSTd_eq_85[i-1]-GMSTeq_85[i-1])/tau1
  GMSTd_eq_85[i] <- GMSTd_eq_85[i-1] + (GMSTeq_85[i-1]-GMSTd_eq_85[i-1])/tau2
}

ui <- navbarPage("Climate Model",
  
  tabPanel("Model",
    "Welcome to the Simple Model!", 
    p(),
    "The Simple Model is a climate model based on one equation. It takes into account three factors that affect our climate: volcanic eruptions, El Nino, and carbon dioxide concentrations. The model reproduces changes in Global Mean Surface Temperature - the average temperature on Earth’s surface.", 
    p(),
    "Using this tool, you can investigate how each of these three factors affects Earth’s temperature. By sliding the tab under each factor, you can make it more or less powerful, or turn it off entirely. 
    The green line represents the actual observed temperatures, and the blue line represents the output of the Simple Model.", 
    p(),
    "For more information, you can click the “more information” button.",
    p(), 
    
    sidebarLayout(
      sidebarPanel(
        sliderInput(inputId = "a", label = "Coefficient for CO2", min = 0, max = 3, value = 1.78, step = 0.01),
        sliderInput(inputId = "b", label = "El Nino Temperatures", min = 0, max = 0.3, value = 0.122, step = 0.001),
        sliderInput(inputId = "c", label = "Volcanic Eruptions", min = 0, max = 3, value = 1.39, step = 0.01),
        actionButton("restore", "Clear")
      ),
      mainPanel(
        plotOutput(outputId = "thisPlot")
      )
    ) 
  ),
  
  #Yes this is terrible programming practice
  #Figure out how to render an equation in here
  tabPanel("More Information",
      h3(em("THE THREE FACTORS")),
      "The simple model takes into account three factors.",
      p(),
      em("Volcanic eruptions: "),
      "The particles spewed into the atmosphere by volcanic eruptions temporarily cool the planet by",
      a(href = "https://scied.ucar.edu/shortcontent/how-volcanoes-influence-climate", "blocking incoming solar radiation."),
      p(),
      em("El Nino: "),
      "Also known as ENSO, or 'El Nino - Southern Oscillation,' this refers to changes in temperatures across the tropical Pacific, in a two-to-seven-year cycle. Each phase in the cycle",
      a(href = "https://www.climate.gov/enso", "triggers changes in climate across the globe. 
"),
      p(),
      em("Carbon dioxide: "),
      "This is a", 
      a(href = "https://www.ncdc.noaa.gov/monitoring-references/faq/greenhouse-gases.php", "greenhouse gas,"),
      "which means that it traps heat released by the Earth, and releases it back into its surroundings, resulting in a warming effect. While other greenhouse gases are also causing temperatures to rise, increases in carbon dioxide concentrations have resulted in ",
      a(href = "https://www.climate.gov/news-features/understanding-climate/climate-change-atmospheric-carbon-dioxide", "two-thirds of the current energy imbalance"), 
      " causing climate change.", 
      p(),
      "The model produces temperature anomalies. This means that the output is differences from a standard mean temperature - not the actual average Earth surface temperature, which is about 15 degrees C.",
      p(),
      "The simple model reproduces changes in Earth’s surface temperature with an error of only 0.08 degrees C. This is pretty cool, considering that we often use much more complicated models to capture the same temperature fluctuations.",
      h3(em("EQUATION")),
      "The simple model is based on the following equation:", 
      p(),
      withMathJax(),
      "$$\\frac{dTg}{dt} = - \\frac{Tg}{\\tau} + a\\cdot\\log\\left(\\frac{CO_2}{CO_{2ref}}\\right)+b\\cdot T_{NINO} + c\\cdot SAOD + d$$",
      p(), 
      withMathJax(),
      "The equation requires data on carbon dioxide, El Nino and volcanic eruptions for each year. In front of each of those terms in the equation is a corresponding coefficient: \\(a\\), \\(b\\), or \\(c\\). This tool adjusts those coefficients to change the power of each factor. ",
      p(), 
      "For more information, you can check out",
      a(href= "https://agupubs.onlinelibrary.wiley.com/doi/pdf/10.1002/2017GL072908", "'The extreme El Nino of 2015-2016 and the end of the global warming hiatus,'"),
      "by Shineng Hu and Alexey Fedorov."
    ), 
  
    #I think if this stays as a separate tab, we should show a zoomed-in thing of this...
    tabPanel("Task", 
             "If you look at the observed Earth surface temperatures (the green line), you can see that in the 2000-2015 time range, our planet did not experience that much warming. (While we did experience the up-and-down cycle caused by El Nino, if you draw a line through the midpoint of those spikes, you can see that it is fairly flat.)", 
            p(),
             "This period has been called the “global warming hiatus,” and ", 
            "the cause of this pause in warming has been a hot topic among climate scientists - but this model may have helped figure it out.",
            "By turning factors on and off (click on the variables below) can you determine what caused the global warming hiatus? Was it the dastardly Carbon Dioxide, the destructive El Nino, or the devious Volcanic Eruptions",
            p(),
            sidebarLayout(
              sidebarPanel(
                h5("Factors in Climate Change Model"),
                p("toggle buttons below"),
                br(),
                checkboxGroupButtons(inputId = "tasks", choices = c("Carbon Dioxide Emissions", "El Nino Temperatures", "Volcanic Eruptions"), selected = c("Carbon Dioxide Emissions", "El Nino Temperatures", "Volcanic Eruptions"), checkIcon = list(
                                     yes = icon("ok", lib = "glyphicon")))
              ),
              mainPanel(
                plotOutput(outputId = "taskPlot")
              ),
              position = "right"
            )     
    ),
  
  tabPanel("Into the Future",
    sidebarLayout(
      mainPanel(
        plotOutput(outputId = "eqCO2Plot")
      ),
      sidebarPanel(
        prettyRadioButtons(inputId = "Pathways", label = "Pathways", choices = c("2.6 Pathway", "4.5 Pathway", "6.0 Pathway", "8.5 Pathway"), status = "info", selected = "2.6 Pathway", shape = "round", fill = TRUE),
        conditionalPanel(
          condition = "input.Pathways == '2.6 Pathway'",
          "Things aren't that bad...look at that line stabilize."
        ),
        conditionalPanel(
          condition = "input.Pathways == '4.5 Pathway'",
          "Alright well things are getting worse already. This is probably where we're at."
        ),
        conditionalPanel(
          condition = "input.Pathways == '6.0 Pathway'",
          "YIKES LET'S NOT DO THIS."
        ),
        conditionalPanel(
          condition = "input.Pathways == '8.5 Pathway'",
          "THINGS HAVE ESCALATED VERY QUICKLY."
        )
      )
    )
  )
)


server <- function(input, output, session) {
  
  #then if we see an inputRestore we're good
  observeEvent(input$restore, {
    updateSliderInput(session, 'a', value = 1.78)
    updateSliderInput(session, 'b', value = 0.122)
    updateSliderInput(session, 'c', value = 1.39)
  })
  
  output$thisPlot <- renderPlot({
    a <<- input$a
    b <<- input$b
    c <<- -input$c
    
    out <- ode23('fun', t0, tfin, sat0) #need to know what ode23 does
    tv_toInterp <- as.numeric(unlist(out[1]))
    sat_mod_toInterp <- as.numeric(unlist(out[2]))
    sat_mod <- interp1(tv_toInterp, sat_mod_toInterp, time)
    
    plot(time,sat_obs,type = "l", main = "Model of Global Mean Surface Temperature", xlab = "Year", ylab = "GMST (°C)", col = "#7bc11f", lwd = 2.5) 
    lines(time, sat_mod, col = "#75AADB", lwd = 5)
    legend("topleft",inset = 0.02, legend=c("Actual Fluctuation in GMST", "Modeled Fluctuation"), col=c("#7bc11f", "#75AADB"), lwd = 2.5)
  })
  
  output$taskPlot <- renderPlot({
    if ('Carbon Dioxide Emissions' %in% input$tasks) {
      a <<- 1.78
    } else {
      a <<- 0
    }
    if ('El Nino Temperatures' %in% input$tasks) {
      b <<- 0.122
    } else {
      b <<- 0
    } 
    if ('Volcanic Eruptions' %in% input$tasks) {
      c <<- -1.39
    } else {
      c <<- 0
    }
    
    out <- ode23('fun', t0, tfin, sat0) 
    tv_toInterp <- as.numeric(unlist(out[1]))
    sat_mod_toInterp <- as.numeric(unlist(out[2]))
    sat_mod <- interp1(tv_toInterp, sat_mod_toInterp, time)
    
    plot(time,sat_obs,type = "l", main = "Model of Global Mean Surface Temperature", xlab = "Year", ylab = "GMST (°C)", col = "#7bc11f", lwd = 2.5) 
    lines(time, sat_mod, col = "#75AADB", lwd = 5)
    legend("topleft",inset = 0.02, legend=c("Actual Fluctuation in GMST", "Modeled Fluctuation"), col=c("#7bc11f", "#75AADB"), lwd = 2.5)
  })
  
  output$eqCO2Plot <- renderPlot ({
    #this plot is only here to ensure that the axes are the right largeness...there must be a better way to do this
    plot(futureTime, GMSTeq_85, type = "l", col = "white", main = "Predicted Global Surface Temperature Fluctations", xlab = "Year", ylab = "GMST (°C)")
  
    switch(input$Pathways,
      "2.6 Pathway" = lines(futureTime, GMST2_26, col = "blue"),
      "4.5 Pathway" = lines(futureTime, GMSTeq_45, col = "pink"),
      "6.0 Pathway" = lines(futureTime, GMSTeq_6, col = "green"),
      "8.5 Pathway" = lines(futureTime, GMSTeq_85, col = "orange")
    )

    lines(histTime[1:126], sat_obs1[1:126])
    legend("topleft", inset = 0.02, legend=c("2.6", "4.5", "6.0", "8.5"), col = c("blue", "pink", "green", "orange"), lwd = 1)
  })
}

shinyApp(ui = ui, server = server)
