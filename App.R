library(shiny)

fillmap<-function(map, figtitle, y , n.col, bk="e", cuts,legendtxt="",
                  leg.loc="bottomright",leg.cex=1.5,main.cex=1.5,main.line=-2,leg.horiz=F,map.lty=1){
  
  if(bk=="q"){if (min(y)<min(y) | max(y)>max(y)){
    print("The minimum or maximum	values of y fall outside of those for y")
  } else {p <- seq(0,1, length=n.col+1)
  br <- round(quantile(y, probs=p),2)}}
  if(bk=="e"){if (min(y)<min(y) | max(y)>max(y)){
    print("The minimum or maximum values of y fall outside of those for y")
  } else {br <- round(seq(min(y), max(y), length=n.col+1),6)}}
  if(bk=="c"){if (length(cuts)!= (n.col+1)) {cat("Cut off and color categories 
	do not match. ", "\n")
    break}  else {br <- cuts}  }
  
  # 0: dark 1: light light Current shading ranges from darkest to light gray white (to distinguish with lakes).
  shading<-gray(rev(0:(n.col-1)/(n.col-1)))
  #shading<-hsv(.6,alpha=0:(n.col-1)/(n.col-1))
  y.grp<-findInterval(y, vec=br, rightmost.closed = TRUE, all.inside = TRUE)
  y.shad<-shading[y.grp]
  
  plot(map,col=y.shad,axes=F, lty=map.lty)
  title(main=figtitle,cex.main=main.cex,line=main.line) 
  
  br<-round(br, 2)
  
  if (is.na(legendtxt[1])){print("No legend specifed")
  } else if (legendtxt[1]==""){
    leg.txt<-paste("[",br[n.col],",",br[n.col+1],"]",sep="")
    for(j in (n.col-1):1){ 
      leg.txt<-append(leg.txt,paste("[",br[j],",",br[j+1],")",sep="")) }
    leg.txt<-rev(leg.txt)
    legend(leg.loc,legend=leg.txt,fill=shading,cex=leg.cex,ncol=1,bty="n",
           horiz=leg.horiz)
  } else if (length(legendtxt) != n.col){cat("Length of lengendtxt must equal 
		n.col", "\n")
    break
  } else {leg.txt<-legendtxt
  legend(leg.loc,legend=leg.txt,fill=shading,cex=leg.cex,ncol=1,bty="n",
         horiz=leg.horiz)}
}


fillmap2<-function(map, figtitle, y , leg.loc="beside", y.scl=NULL,
                   main.cex=1.5,main.line=0,map.lty=1,leg.rnd=0,
                   leg.cex=1){
  
  # 0: dark 1: light light Current shading ranges from darkest to light gray white (to distinguish with lakes).
  y.uq=sort(unique(c(y,y.scl)))
  cols<-viridis(length(y.uq),direction=-1)
  shading=y
  for (i in 1:length(y)){
    shading[i]<-cols[which(y.uq==y[i])]
  }
  
  par(mar=c(0,0,2,0))
  if (leg.loc=="beside"){
    layout(matrix(1:2,ncol=2),width=c(.8,.2))
  } else 
    if (leg.loc=="below"){
      layout(matrix(1:2,nrow=2),height=c(.6,.4))
    } else (print("leg.loc options are below or beside"))
  
  plot(map,col=shading,axes=F, lty=map.lty)
  title(main=figtitle,cex.main=main.cex,line=main.line) 
  
  par(mar=c(5, 4, 4, 2) + 0.1)
  plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = '')
  cols.5=cols[seq(1,length(y.uq),length.out=5)]
  lab.5=cols.5
  for (i in 1:5){lab.5[i]=y.uq[which(cols==cols.5[i])[1]]}
  lab.5=round(as.numeric(lab.5),leg.rnd)
  par(mar=c(0,0,0,0))
  if (leg.loc=="beside"){
    legend_image <- as.raster(matrix(cols, ncol=1))
    text(x=1.6, 
         y = seq(0,length(y.uq),length.out=5)/length(y.uq),
         labels = rev(lab.5), cex=leg.cex)
    rasterImage(legend_image, 0, 0, 1,1)
  } else{
    legend_image <- as.raster(matrix(cols, nrow=1))
    text(y=-0.25, 
         x = seq(0,length(y.uq),length.out=5)/(length(y.uq)*.5),
         labels = lab.5, cex=leg.cex)
    rasterImage(legend_image, 0, 0, 2,1)
  }
}


library(INLA)
#library(fillmap)
library(rgdal)
library(spdep)
library(maptools)
library(corrplot)
library(visdat)
library(viridis)

d.inla = read.csv("data\\d.INLA.csv")
data = read.csv("data\\data1.csv")
NHtracts = readOGR("data\\NHtracts\\NHtracts.shp")



ui = (fluidPage(
  titlePanel("2010-2018 WPD Arrest Data"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year", label="Year", 
                  min=2010,max=2018,value=2010,sep="",animate=animationOptions(interval=500,loop=TRUE)),      
      radioButtons("data",label="Data:",c("Total Arrests","White Only Arrests","Black Only Arrests")),
      radioButtons("adj",label="Data Adjustment:",c("None","As a Percent of the Population","As a Percent of Total Arrests",
                                                    "Standardized Incidence Ratio","Poisson Regression")),
      br(),
      div(img(src = "Handcuffs.jpg", height = 70, width = 150), style="text-align: center;"),
      br(),
      div(p(a("Cape Fear Collective", 
              href = "https://capefearcollective.org/")), style="text-align: center;")
    ),
    mainPanel(
      textOutput("text"),
      plotOutput("map"),
      tableOutput("table"))
  )))
server = function(input,output){
  output$map = renderPlot({ 
    if(input$adj=='None'){
      if(input$data=='Total Arrests'){
        fillmap2(NHtracts, paste(input$year,input$data),data$arrests_total[which(data$year == input$year)], map.lty = 0,leg.rnd = 2,leg.loc = 'below', y.scl = data$arrests_total)
      }
      if(input$data=='White Only Arrests'){
        fillmap2(NHtracts, paste(input$year,input$data),data$arrests_W[which(data$year == input$year)], map.lty = 0,leg.rnd = 2,leg.loc = 'below', y.scl = data$arrests_W)
      }
      if(input$data=='Black Only Arrests'){
        fillmap2(NHtracts, paste(input$year,input$data),data$arrests_B[which(data$year == input$year)], map.lty = 0,leg.rnd = 2,leg.loc = 'below', y.scl = data$arrests_B)
      }
    }
    if(input$adj=='As a Percent of Total Arrests'){
      if(input$data=='Total Arrests'){
        fillmap2(NHtracts, paste(input$year,input$data),data$t_ar_pct[which(data$year == input$year)], map.lty = 0,leg.rnd = 2,leg.loc = 'below', y.scl = data$t_ar_pct)
      }
      if(input$data=='White Only Arrests'){
        fillmap2(NHtracts, paste(input$year,input$data),data$w_ar_pct[which(data$year == input$year)], map.lty = 0,leg.rnd = 2,leg.loc = 'below', y.scl = data$w_ar_pct)
      }
      if(input$data=='Black Only Arrests'){
        fillmap2(NHtracts, paste(input$year,input$data),data$b_ar_pct[which(data$year == input$year)], map.lty = 0,leg.rnd = 2,leg.loc = 'below', y.scl = data$b_ar_pct)
      }
    }
    if(input$adj=='As a Percent of the Population'){
      if(input$data=='Total Arrests'){
        fillmap2(NHtracts, paste(input$year,input$data),data$ttt_ar_pct[which(data$year == input$year)], map.lty = 0,leg.rnd = 2,leg.loc = 'below', y.scl = data$ttt_ar_pct)
      }
      if(input$data=='White Only Arrests'){
        fillmap2(NHtracts, paste(input$year,input$data),data$ww_ar_pct[which(data$year == input$year)], map.lty = 0,leg.rnd = 2,leg.loc = 'below', y.scl = data$ww_ar_pct)
      }
      if(input$data=='Black Only Arrests'){
        fillmap2(NHtracts, paste(input$year,input$data),data$bb_ar_pct[which(data$year == input$year)], map.lty = 0,leg.rnd = 2,leg.loc = 'below', y.scl = data$bb_ar_pct)
      }
    }
    if(input$adj=='Standardized Incidence Ratio'){
      if(input$data=='Total Arrests'){
        fillmap2(NHtracts, paste(input$year,input$data),data$sir_tot[which(data$year == input$year)], map.lty = 0,leg.rnd = 2,leg.loc = 'below', y.scl = data$sir_tot)
      }
      if(input$data=='White Only Arrests'){
        fillmap2(NHtracts, paste(input$year,input$data),data$sir_w[which(data$year == input$year)], map.lty = 0,leg.rnd = 2,leg.loc = 'below', y.scl = data$sir_w)
      }
      if(input$data=='Black Only Arrests'){
        fillmap2(NHtracts, paste(input$year,input$data),data$sir_b[which(data$year == input$year)], map.lty = 0,leg.rnd = 2,leg.loc = 'below', y.scl = data$sir_b)
      }
    }
    if(input$adj=='Poisson Regression'){
      if(input$data=='Total Arrests'){
        fillmap2(NHtracts, paste(input$year,input$data),exp(d.inla$res[which(data$year == input$year)]), map.lty = 0,leg.rnd = 2,leg.loc = 'below', y.scl = exp(d.inla$res))
      }
      if(input$data=='White Only Arrests'){
        fillmap2(NHtracts, paste(input$year,input$data),exp(d.inla$res2[which(data$year == input$year)]), map.lty = 0,leg.rnd = 2,leg.loc = 'below', y.scl = exp(d.inla$res2))
      }
      if(input$data=='Black Only Arrests'){
        fillmap2(NHtracts, paste(input$year,input$data),exp(d.inla$res1[which(data$year == input$year)]), map.lty = 0,leg.rnd = 2,leg.loc = 'below', y.scl = exp(d.inla$res))
      }
    }
  }
  )
}















shinyApp(ui = ui, server = server)