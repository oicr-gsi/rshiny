library(shiny)
library(ggplot2)

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

df <- read.table("/home/ubuntu/data/run_reports/project_only/dxrx.all.lanes.tsv",header=TRUE,sep="\t")
#df <- read.table("/Users/jadams/Test/dxrx.all.lanes.tsv",header=TRUE,sep="\t")

shinyServer(function(input, output) {

  output$percentMappedOnTargetPlot <- renderPlot({
    myseq <- seq(from=1,to=64)
    df$record <- myseq
    
    t <- input$percentMappedOnTargetThreshold
    
    d <- data.frame(df$Library , df$record, as.numeric(gsub("%","",df$Percent.mapped.on.Target)))
    d$group <- as.factor((d[,3] > t)*1)
    colnames(d) <- c("sample" , "library","my_y","threshold")
    
    nFail <- nrow(d[which(d[,4] == 0),])
    
    if (nFail == 0) {
      ggplot(d, aes(x=library, y=my_y)) + geom_bar(stat="identity" , aes(fill=threshold) ) + labs(x="64 libraries", y="percent mapped on target") + scale_y_continuous(limits=c(0.0,100.0)) +
      geom_hline(yintercept=t, color="red", linetype="dashed") +
      geom_text(aes(x=0,y=t+2,label=t ), color="red", size=2) +
      theme(legend.position="none", axis.title.y=element_text(size=6), axis.title.x=element_text(size=6), axis.text.y=element_text(size=4), axis.text.x=element_text(size=4)) +
      scale_fill_manual(values=c("0" = gg_color_hue(2)[1], "1" = gg_color_hue(2)[2]))
    
    } else {
      ggplot(d, aes(x=library, y=my_y)) + geom_bar(stat="identity" , aes(fill=threshold) ) + labs(x="64 libraries", y="percent mapped on target") + scale_y_continuous(limits=c(0.0,100.0)) +
      geom_hline(yintercept=t, color="red", linetype="dashed") +
      geom_text(data=subset(d, my_y<t), aes(x=library,y=my_y-2,label=sample,hjust="right"), color="black", size=1 , angle=90) +
      geom_text(aes(x=0,y=t+2,label=t ), color="red", size=2) +
      theme(legend.position="none", axis.title.y=element_text(size=6), axis.title.x=element_text(size=6), axis.text.y=element_text(size=4), axis.text.x=element_text(size=4)) +
      scale_fill_manual(values=c("0" = gg_color_hue(2)[1], "1" = gg_color_hue(2)[2]))
    }  
    
    
    
  })
  
  output$mapPercentPlot <- renderPlot({
    myseq <- seq(from=1,to=64)
    df$record <- myseq
    
    t <- input$mapPercentThreshold
    
    d <- data.frame(df$Library , df$record, as.numeric(gsub("%","",df$Map.Percent)))
    d$group <- as.factor((d[,3] > t)*1)
    colnames(d) <- c("sample" , "library","my_y","threshold")
    
    nFail <- nrow(d[which(d[,4] == 0),])
    
    if (nFail == 0) {
      ggplot(d, aes(x=library, y=my_y)) + geom_bar(stat="identity" , aes(fill=threshold) ) + labs(x="64 libraries", y="map percent") + scale_y_continuous(limits=c(0.0,100.0)) +
        geom_hline(yintercept=t, color="red", linetype="dashed") +
        geom_text(aes(x=0,y=t+2,label=t ), color="red", size=2) +
        theme(legend.position="none", axis.title.y=element_text(size=6), axis.title.x=element_text(size=6), axis.text.y=element_text(size=4), axis.text.x=element_text(size=4)) +
        scale_fill_manual(values=c("0" = gg_color_hue(2)[1], "1" = gg_color_hue(2)[2]))
      
    } else {
      ggplot(d, aes(x=library, y=my_y)) + geom_bar(stat="identity" , aes(fill=threshold) ) + labs(x="64 libraries", y="map percent") + scale_y_continuous(limits=c(0.0,100.0)) +
        geom_hline(yintercept=t, color="red", linetype="dashed") +
        geom_text(data=subset(d, my_y<t), aes(x=library,y=my_y-2,label=sample,hjust="right"), color="black", size=1 , angle=90) +
        geom_text(aes(x=0,y=t+2,label=t ), color="red", size=2) +
        theme(legend.position="none", axis.title.y=element_text(size=6), axis.title.x=element_text(size=6), axis.text.y=element_text(size=4), axis.text.x=element_text(size=4)) +
        scale_fill_manual(values=c("0" = gg_color_hue(2)[1], "1" = gg_color_hue(2)[2]))
    }  
    
    
  })

})
