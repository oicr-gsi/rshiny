library(shiny)
library(ggplot2)

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

df <- read.table("/home/ubuntu/data/run_reports/project_only/dxrx.all.lanes.tsv",header=TRUE,sep="\t")

shinyServer(function(input,output) {

    output$totalReadsPlot <- renderPlot({
    myseq <- seq(from=1,to=nrow(df))

    t <- input$totalReadsFailThreshold

sortby <- input$totalReadsGroup
    if (sortby != "none") {
        df <- df[order(df[[sortby]]),]
    }

df$record <- myseq
    d <- data.frame(df$Library , df$record, as.numeric(gsub(",","",df$PF.Reads)))
d$group <- as.factor((d[,3] > t)*1)
    
    
    colnames(d) <- c("sample","library","my_y","threshold")
windowHeight <- max(d$my_y) * 1.10
plot <- ggplot(d, aes(x=library, y=my_y)) + geom_bar(stat="identity" , aes(fill=threshold) ) + labs(x="all libraries", y="total reads (pass filter)") + scale_y_continuous(limits=c(0.0,windowHeight))
    plot <- plot + geom_hline(yintercept=t, color="red", linetype="dashed")
    plot <- plot + geom_text(aes(x=0,y=t+2,label=t ), color="red", size=2)
    
    
    
    unique.groupby <- unique(df[[sortby]])
    
    if (sortby != "none") {
        for (unique.group in unique.groupby){
            group.lines <- df[which(df[[sortby]] == unique.group),]
            first.record <- head(group.lines , n=1)$record
            last.record <- tail(group.lines , n=1)$record
    
            plot <- plot + geom_vline(xintercept=last.record+0.5 , color="gray", linetype="dashed")
        }
    }
    
    
    
    nFail <- nrow(d[which(d[,4] == 0),])
    if (nFail > 0) {
        plot <- plot + geom_text(data=subset(d, my_y<t), aes(x=library,y=my_y-2,label=sample,hjust="right"), color="black", size=1 , angle=90)
    }
      
    plot <- plot + theme(legend.position="none", axis.title.y=element_text(size=6), axis.title.x=element_text(size=6), axis.text.y=element_text(size=4), axis.text.x=element_text(size=4))
    plot <- plot + scale_fill_manual(values=c("0" = gg_color_hue(2)[1], "1" = gg_color_hue(2)[2]))
    
    plot
        
        
      
    
    
    })
    output$insertMeanPlot <- renderPlot({
    myseq <- seq(from=1,to=nrow(df))

    t <- input$insertMeanFailThreshold

sortby <- input$insertMeanGroup
    if (sortby != "none") {
        df <- df[order(df[[sortby]]),]
    }

df$record <- myseq
    d <- data.frame(df$Library, df$record, df$Insert_Mean)
d$group <- as.factor((d[,3] > t)*1)
    
    
    colnames(d) <- c("sample","library","my_y","threshold")
windowHeight <- max(d$my_y) * 1.10
plot <- ggplot(d, aes(x=library, y=my_y)) + geom_bar(stat="identity" , aes(fill=threshold) ) + labs(x="all libraries", y="mean insert size") + scale_y_continuous(limits=c(0.0,windowHeight))
    plot <- plot + geom_hline(yintercept=t, color="red", linetype="dashed")
    plot <- plot + geom_text(aes(x=0,y=t+2,label=t ), color="red", size=2)
    
    
    
    unique.groupby <- unique(df[[sortby]])
    
    if (sortby != "none") {
        for (unique.group in unique.groupby){
            group.lines <- df[which(df[[sortby]] == unique.group),]
            first.record <- head(group.lines , n=1)$record
            last.record <- tail(group.lines , n=1)$record
    
            plot <- plot + geom_vline(xintercept=last.record+0.5 , color="gray", linetype="dashed")
        }
    }
    
    
    
    nFail <- nrow(d[which(d[,4] == 0),])
    if (nFail > 0) {
        plot <- plot + geom_text(data=subset(d, my_y<t), aes(x=library,y=my_y-2,label=sample,hjust="right"), color="black", size=1 , angle=90)
    }
      
    plot <- plot + theme(legend.position="none", axis.title.y=element_text(size=6), axis.title.x=element_text(size=6), axis.text.y=element_text(size=4), axis.text.x=element_text(size=4))
    plot <- plot + scale_fill_manual(values=c("0" = gg_color_hue(2)[1], "1" = gg_color_hue(2)[2]))
    
    plot
        
        
      
    
    
    })
    output$percentMappedPlot <- renderPlot({
    myseq <- seq(from=1,to=nrow(df))

    t <- input$percentMappedFailThreshold

sortby <- input$percentMappedGroup
    if (sortby != "none") {
        df <- df[order(df[[sortby]]),]
    }

df$record <- myseq
    d <- data.frame(df$Library , df$record, as.numeric(gsub("%","",df$Map.Percent)))
d$group <- as.factor((d[,3] > t)*1)
    
    
    colnames(d) <- c("sample","library","my_y","threshold")
windowHeight <- 100.0
plot <- ggplot(d, aes(x=library, y=my_y)) + geom_bar(stat="identity" , aes(fill=threshold) ) + labs(x="all libraries", y="percent of reads mapped to hg19") + scale_y_continuous(limits=c(0.0,windowHeight))
    plot <- plot + geom_hline(yintercept=t, color="red", linetype="dashed")
    plot <- plot + geom_text(aes(x=0,y=t+2,label=t ), color="red", size=2)
    
    
    
    unique.groupby <- unique(df[[sortby]])
    
    if (sortby != "none") {
        for (unique.group in unique.groupby){
            group.lines <- df[which(df[[sortby]] == unique.group),]
            first.record <- head(group.lines , n=1)$record
            last.record <- tail(group.lines , n=1)$record
    
            plot <- plot + geom_vline(xintercept=last.record+0.5 , color="gray", linetype="dashed")
        }
    }
    
    
    
    nFail <- nrow(d[which(d[,4] == 0),])
    if (nFail > 0) {
        plot <- plot + geom_text(data=subset(d, my_y<t), aes(x=library,y=my_y-2,label=sample,hjust="right"), color="black", size=1 , angle=90)
    }
      
    plot <- plot + theme(legend.position="none", axis.title.y=element_text(size=6), axis.title.x=element_text(size=6), axis.text.y=element_text(size=4), axis.text.x=element_text(size=4))
    plot <- plot + scale_fill_manual(values=c("0" = gg_color_hue(2)[1], "1" = gg_color_hue(2)[2]))
    
    plot
        
        
      
    
    
    })
    output$percentOntPlot <- renderPlot({
    myseq <- seq(from=1,to=nrow(df))

    t <- input$percentOntFailThreshold

sortby <- input$percentOntGroup
    if (sortby != "none") {
        df <- df[order(df[[sortby]]),]
    }

df$record <- myseq
    d <- data.frame(df$Library , df$record, as.numeric(gsub("%","",df$Percent.mapped.on.Target)))
d$group <- as.factor((d[,3] > t)*1)
    
    
    colnames(d) <- c("sample","library","my_y","threshold")
windowHeight <- 100.0
plot <- ggplot(d, aes(x=library, y=my_y)) + geom_bar(stat="identity" , aes(fill=threshold) ) + labs(x="all libraries", y="percent of mapped reads on target") + scale_y_continuous(limits=c(0.0,windowHeight))
    plot <- plot + geom_hline(yintercept=t, color="red", linetype="dashed")
    plot <- plot + geom_text(aes(x=0,y=t+2,label=t ), color="red", size=2)
    
    
    
    unique.groupby <- unique(df[[sortby]])
    
    if (sortby != "none") {
        for (unique.group in unique.groupby){
            group.lines <- df[which(df[[sortby]] == unique.group),]
            first.record <- head(group.lines , n=1)$record
            last.record <- tail(group.lines , n=1)$record
    
            plot <- plot + geom_vline(xintercept=last.record+0.5 , color="gray", linetype="dashed")
        }
    }
    
    
    
    nFail <- nrow(d[which(d[,4] == 0),])
    if (nFail > 0) {
        plot <- plot + geom_text(data=subset(d, my_y<t), aes(x=library,y=my_y-2,label=sample,hjust="right"), color="black", size=1 , angle=90)
    }
      
    plot <- plot + theme(legend.position="none", axis.title.y=element_text(size=6), axis.title.x=element_text(size=6), axis.text.y=element_text(size=4), axis.text.x=element_text(size=4))
    plot <- plot + scale_fill_manual(values=c("0" = gg_color_hue(2)[1], "1" = gg_color_hue(2)[2]))
    
    plot
        
        
      
    
    
    })
    output$meanCoveragePlot <- renderPlot({
    myseq <- seq(from=1,to=nrow(df))

    t <- input$meanCoverageFailThreshold

sortby <- input$meanCoverageGroup
    if (sortby != "none") {
        df <- df[order(df[[sortby]]),]
    }

df$record <- myseq
    d <- data.frame(df$Library , df$record, as.numeric(gsub("x","",df$Coverage)))
d$group <- as.factor((d[,3] > t)*1)
    
    
    colnames(d) <- c("sample","library","my_y","threshold")
windowHeight <- max(d$my_y) * 1.10
plot <- ggplot(d, aes(x=library, y=my_y)) + geom_bar(stat="identity" , aes(fill=threshold) ) + labs(x="all libraries", y="mean coverage") + scale_y_continuous(limits=c(0.0,windowHeight))
    plot <- plot + geom_hline(yintercept=t, color="red", linetype="dashed")
    plot <- plot + geom_text(aes(x=0,y=t+2,label=t ), color="red", size=2)
    
    
    
    unique.groupby <- unique(df[[sortby]])
    
    if (sortby != "none") {
        for (unique.group in unique.groupby){
            group.lines <- df[which(df[[sortby]] == unique.group),]
            first.record <- head(group.lines , n=1)$record
            last.record <- tail(group.lines , n=1)$record
    
            plot <- plot + geom_vline(xintercept=last.record+0.5 , color="gray", linetype="dashed")
        }
    }
    
    
    
    nFail <- nrow(d[which(d[,4] == 0),])
    if (nFail > 0) {
        plot <- plot + geom_text(data=subset(d, my_y<t), aes(x=library,y=my_y-2,label=sample,hjust="right"), color="black", size=1 , angle=90)
    }
      
    plot <- plot + theme(legend.position="none", axis.title.y=element_text(size=6), axis.title.x=element_text(size=6), axis.text.y=element_text(size=4), axis.text.x=element_text(size=4))
    plot <- plot + scale_fill_manual(values=c("0" = gg_color_hue(2)[1], "1" = gg_color_hue(2)[2]))
    
    plot
        
        
      
    
    
    })

})