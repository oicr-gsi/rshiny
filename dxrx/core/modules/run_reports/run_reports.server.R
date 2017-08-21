library(shiny)
library(ggplot2)

gg_color_hue <- function(n) {
	hues = seq(15, 375, length = n + 1)
	hcl(h = hues, l = 65, c = 100)[1:n]
}

rrdf <- read.table("/home/ubuntu/data/dxrx/core/run_reports/project_only/dxrx.all.lanes.tsv",header=TRUE,sep="\t")

output$runReports_totalReadsPlot <- renderPlot({
	myseq <- seq(from=1,to=nrow(rrdf))
	t <- input$runReports_totalReadsFailThreshold

	sortby <- input$runReports_totalReadsGroup
	if (sortby != "none") {
		rrdf <- rrdf[order(rrdf[[sortby]]),]
	}

	rrdf$record <- myseq
	d <- data.frame(rrdf$Library , rrdf$record, as.numeric(gsub(",","",rrdf$PF.Reads)))
	d$group <- as.factor((d[,3] > t)*1)
	colnames(d) <- c("sample","library","my_y","threshold")
	windowHeight <- max(d$my_y) * 1.10
	plot <- ggplot(d, aes(x=library, y=my_y)) + geom_bar(stat="identity" , aes(fill=threshold) ) + labs(x="all libraries", y="total reads (pass filter)") + scale_y_continuous(limits=c(0.0,windowHeight))
	plot <- plot + geom_hline(yintercept=t, color="red", linetype="dashed")
	plot <- plot + geom_text(aes(x=0,y=t+2,label=t ), color="red")  
	unique.groupby <- unique(rrdf[[sortby]])
    
	if (sortby != "none") {
		for (unique.group in unique.groupby){
			group.lines <- rrdf[which(rrdf[[sortby]] == unique.group),]
			first.record <- head(group.lines , n=1)$record
			last.record <- tail(group.lines , n=1)$record
			plot <- plot + geom_vline(xintercept=last.record+0.5 , color="gray", linetype="dashed")
		}
	}
        
	nFail <- nrow(d[which(d[,4] == 0),])
	if (nFail > 0) {
		plot <- plot + geom_text(data=subset(d, my_y<t), aes(x=library,y=my_y-2,label=sample,hjust="right"), color="black", angle=90)
	}
      
	plot <- plot + theme(legend.position="none")
	plot <- plot + scale_fill_manual(values=c("0" = gg_color_hue(2)[1], "1" = gg_color_hue(2)[2]))  
	plot
})
output$runReports_insertMeanPlot <- renderPlot({
	myseq <- seq(from=1,to=nrow(rrdf))
	t <- input$runReports_insertMeanFailThreshold

	sortby <- input$runReports_insertMeanGroup
	if (sortby != "none") {
		rrdf <- rrdf[order(rrdf[[sortby]]),]
	}

	rrdf$record <- myseq
	d <- data.frame(rrdf$Library, rrdf$record, rrdf$Insert_Mean)
	d$group <- as.factor((d[,3] > t)*1)
	colnames(d) <- c("sample","library","my_y","threshold")
	windowHeight <- max(d$my_y) * 1.10
	plot <- ggplot(d, aes(x=library, y=my_y)) + geom_bar(stat="identity" , aes(fill=threshold) ) + labs(x="all libraries", y="mean insert size") + scale_y_continuous(limits=c(0.0,windowHeight))
	plot <- plot + geom_hline(yintercept=t, color="red", linetype="dashed")
	plot <- plot + geom_text(aes(x=0,y=t+2,label=t ), color="red")  
	unique.groupby <- unique(rrdf[[sortby]])
    
	if (sortby != "none") {
		for (unique.group in unique.groupby){
			group.lines <- rrdf[which(rrdf[[sortby]] == unique.group),]
			first.record <- head(group.lines , n=1)$record
			last.record <- tail(group.lines , n=1)$record
			plot <- plot + geom_vline(xintercept=last.record+0.5 , color="gray", linetype="dashed")
		}
	}
        
	nFail <- nrow(d[which(d[,4] == 0),])
	if (nFail > 0) {
		plot <- plot + geom_text(data=subset(d, my_y<t), aes(x=library,y=my_y-2,label=sample,hjust="right"), color="black", angle=90)
	}
      
	plot <- plot + theme(legend.position="none")
	plot <- plot + scale_fill_manual(values=c("0" = gg_color_hue(2)[1], "1" = gg_color_hue(2)[2]))  
	plot
})
output$runReports_percentMappedPlot <- renderPlot({
	myseq <- seq(from=1,to=nrow(rrdf))
	t <- input$runReports_percentMappedFailThreshold

	sortby <- input$runReports_percentMappedGroup
	if (sortby != "none") {
		rrdf <- rrdf[order(rrdf[[sortby]]),]
	}

	rrdf$record <- myseq
	d <- data.frame(rrdf$Library , rrdf$record, as.numeric(gsub("%","",rrdf$Map.Percent)))
	d$group <- as.factor((d[,3] > t)*1)
	colnames(d) <- c("sample","library","my_y","threshold")
	windowHeight <- 100.0
	plot <- ggplot(d, aes(x=library, y=my_y)) + geom_bar(stat="identity" , aes(fill=threshold) ) + labs(x="all libraries", y="percent of reads mapped to hg19") + scale_y_continuous(limits=c(0.0,windowHeight))
	plot <- plot + geom_hline(yintercept=t, color="red", linetype="dashed")
	plot <- plot + geom_text(aes(x=0,y=t+2,label=t ), color="red")  
	unique.groupby <- unique(rrdf[[sortby]])
    
	if (sortby != "none") {
		for (unique.group in unique.groupby){
			group.lines <- rrdf[which(rrdf[[sortby]] == unique.group),]
			first.record <- head(group.lines , n=1)$record
			last.record <- tail(group.lines , n=1)$record
			plot <- plot + geom_vline(xintercept=last.record+0.5 , color="gray", linetype="dashed")
		}
	}
        
	nFail <- nrow(d[which(d[,4] == 0),])
	if (nFail > 0) {
		plot <- plot + geom_text(data=subset(d, my_y<t), aes(x=library,y=my_y-2,label=sample,hjust="right"), color="black", angle=90)
	}
      
	plot <- plot + theme(legend.position="none")
	plot <- plot + scale_fill_manual(values=c("0" = gg_color_hue(2)[1], "1" = gg_color_hue(2)[2]))  
	plot
})
output$runReports_percentOntPlot <- renderPlot({
	myseq <- seq(from=1,to=nrow(rrdf))
	t <- input$runReports_percentOntFailThreshold

	sortby <- input$runReports_percentOntGroup
	if (sortby != "none") {
		rrdf <- rrdf[order(rrdf[[sortby]]),]
	}

	rrdf$record <- myseq
	d <- data.frame(rrdf$Library , rrdf$record, as.numeric(gsub("%","",rrdf$Percent.mapped.on.Target)))
	d$group <- as.factor((d[,3] > t)*1)
	colnames(d) <- c("sample","library","my_y","threshold")
	windowHeight <- 100.0
	plot <- ggplot(d, aes(x=library, y=my_y)) + geom_bar(stat="identity" , aes(fill=threshold) ) + labs(x="all libraries", y="percent of mapped reads on target") + scale_y_continuous(limits=c(0.0,windowHeight))
	plot <- plot + geom_hline(yintercept=t, color="red", linetype="dashed")
	plot <- plot + geom_text(aes(x=0,y=t+2,label=t ), color="red")  
	unique.groupby <- unique(rrdf[[sortby]])
    
	if (sortby != "none") {
		for (unique.group in unique.groupby){
			group.lines <- rrdf[which(rrdf[[sortby]] == unique.group),]
			first.record <- head(group.lines , n=1)$record
			last.record <- tail(group.lines , n=1)$record
			plot <- plot + geom_vline(xintercept=last.record+0.5 , color="gray", linetype="dashed")
		}
	}
        
	nFail <- nrow(d[which(d[,4] == 0),])
	if (nFail > 0) {
		plot <- plot + geom_text(data=subset(d, my_y<t), aes(x=library,y=my_y-2,label=sample,hjust="right"), color="black", angle=90)
	}
      
	plot <- plot + theme(legend.position="none")
	plot <- plot + scale_fill_manual(values=c("0" = gg_color_hue(2)[1], "1" = gg_color_hue(2)[2]))  
	plot
})
output$runReports_meanCoveragePlot <- renderPlot({
	myseq <- seq(from=1,to=nrow(rrdf))
	t <- input$runReports_meanCoverageFailThreshold

	sortby <- input$runReports_meanCoverageGroup
	if (sortby != "none") {
		rrdf <- rrdf[order(rrdf[[sortby]]),]
	}

	rrdf$record <- myseq
	d <- data.frame(rrdf$Library , rrdf$record, as.numeric(gsub("x","",rrdf$Coverage)))
	d$group <- as.factor((d[,3] > t)*1)
	colnames(d) <- c("sample","library","my_y","threshold")
	windowHeight <- max(d$my_y) * 1.10
	plot <- ggplot(d, aes(x=library, y=my_y)) + geom_bar(stat="identity" , aes(fill=threshold) ) + labs(x="all libraries", y="mean coverage") + scale_y_continuous(limits=c(0.0,windowHeight))
	plot <- plot + geom_hline(yintercept=t, color="red", linetype="dashed")
	plot <- plot + geom_text(aes(x=0,y=t+2,label=t ), color="red")  
	unique.groupby <- unique(rrdf[[sortby]])
    
	if (sortby != "none") {
		for (unique.group in unique.groupby){
			group.lines <- rrdf[which(rrdf[[sortby]] == unique.group),]
			first.record <- head(group.lines , n=1)$record
			last.record <- tail(group.lines , n=1)$record
			plot <- plot + geom_vline(xintercept=last.record+0.5 , color="gray", linetype="dashed")
		}
	}
        
	nFail <- nrow(d[which(d[,4] == 0),])
	if (nFail > 0) {
		plot <- plot + geom_text(data=subset(d, my_y<t), aes(x=library,y=my_y-2,label=sample,hjust="right"), color="black", angle=90)
	}
      
	plot <- plot + theme(legend.position="none")
	plot <- plot + scale_fill_manual(values=c("0" = gg_color_hue(2)[1], "1" = gg_color_hue(2)[2]))  
	plot
})
