setwd("/Users/ivyfan/Yale/Danya")
library(shiny)
library(pracma)

rm(list=ls())

#set up the coefficients first
rco2 <- 320
tau <- 4
tau1 <- 4  
tau2 <- 56
a <- 1.21
b <- 0.12
c <- -1.68
d <- 0.03

#read in the data used for the functions
myData <- read.table("simple_model_data1880to2015.txt")
histTime <- myData[,1]
sat_obs1 <- myData[,2]
co21 <- myData[,3]
nino1 <- myData[,4]
saod1 <- myData[,5]

GMST <- sat_obs1[1]
GMSTd <- mean(sat_obs1[1:20])

# read in the data for the current time period
for (i in 2:126) {
  f <- co21[i]
  g <- nino1[i]
  h <- saod1[i]
  GMST[i] <- GMST[i-1] - GMST[i-1]/tau + a*log(f/rco2) + b*g + c*h + d + (GMSTd[i-1]-GMST[i-1])/tau1
  GMSTd[i] <- GMSTd[i-1] + (GMST[i-1] - GMSTd[i-1])/tau2
}
GMSTdstart <- GMSTd

#now we begin the graphs for the future pathways
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

ui <- fluidPage(
  mainPanel(
    plotOutput(outputId = "mainPlot")
  )
)

server <- function(input, output) {
  output$mainPlot <- renderPlot({
    plot(futureTime, GMSTeq_85, type = "l", col = "orange", main = "Predicted Global Surface Temperature Fluctations", xlab = "Year", ylab = "GMST (°C)")
    lines(futureTime, GMSTeq_6, col = "green")
    lines(futureTime, GMSTeq_45, col = "pink")
    lines(futureTime, GMST2_26, col = "blue")
    lines(histTime[1:126], sat_obs1[1:126])
    legend("topleft", inset = 0.02, legend=c("2.6", "4.5", "6.0", "8.5"), col = c("blue", "pink", "green", "orange"), lwd = 1)
  })
}

shinyApp(ui = ui, server = server)
