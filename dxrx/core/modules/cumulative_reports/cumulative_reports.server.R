library(shiny)
library(ggplot2)

gg_color_hue <- function(n) {
	hues = seq(15, 375, length = n + 1)
	hcl(h = hues, l = 65, c = 100)[1:n]
}

df <- read.table("/home/ubuntu/data/dxrx/core/cumulative_reports/dxrx.cumulative.report.tsv",header=TRUE,sep="\t")

output$cumlativeReports_totalReadsPlot <- renderPlot({
	myseq <- seq(from=1,to=nrow(df))
	t <- input$cumlativeReports_totalReadsFailThreshold

	sortby <- input$cumlativeReports_totalReadsGroup
	if (sortby != "none") {
		df <- df[order(df[[sortby]]),]
	}

	df$record <- myseq
	d <- data.frame(df$Sample , df$record, as.numeric(gsub(",","",df$Total.Reads)))
	d$group <- as.factor((d[,3] > t)*1)
	colnames(d) <- c("sample","library","my_y","threshold")
	windowHeight <- max(d$my_y) * 1.10
	plot <- ggplot(d, aes(x=library, y=my_y)) + geom_bar(stat="identity" , aes(fill=threshold) ) + labs(x="all libraries", y="total reads (pass filter)") + scale_y_continuous(limits=c(0.0,windowHeight))
	plot <- plot + geom_hline(yintercept=t, color="red", linetype="dashed")
	plot <- plot + geom_text(aes(x=0,y=t+2,label=t ), color="red")  
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
		plot <- plot + geom_text(data=subset(d, my_y<t), aes(x=library,y=my_y-2,label=sample,hjust="right"), color="black", angle=90)
	}
      
	plot <- plot + theme(legend.position="none")
	plot <- plot + scale_fill_manual(values=c("0" = gg_color_hue(2)[1], "1" = gg_color_hue(2)[2]))  
	plot
})
output$cumlativeReports_readLengthPlot <- renderPlot({
	myseq <- seq(from=1,to=nrow(df))
	t <- input$cumlativeReports_readLengthFailThreshold

	sortby <- input$cumlativeReports_readLengthGroup
	if (sortby != "none") {
		df <- df[order(df[[sortby]]),]
	}

	df$record <- myseq
	d <- data.frame(df$Sample, df$record, df$Aver..Read.Length)
	d$group <- as.factor((d[,3] > t)*1)
	colnames(d) <- c("sample","library","my_y","threshold")
	windowHeight <- max(d$my_y) * 1.10
	plot <- ggplot(d, aes(x=library, y=my_y)) + geom_bar(stat="identity" , aes(fill=threshold) ) + labs(x="all libraries", y="average read length") + scale_y_continuous(limits=c(0.0,windowHeight))
	plot <- plot + geom_hline(yintercept=t, color="red", linetype="dashed")
	plot <- plot + geom_text(aes(x=0,y=t+2,label=t ), color="red")  
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
		plot <- plot + geom_text(data=subset(d, my_y<t), aes(x=library,y=my_y-2,label=sample,hjust="right"), color="black", angle=90)
	}
      
	plot <- plot + theme(legend.position="none")
	plot <- plot + scale_fill_manual(values=c("0" = gg_color_hue(2)[1], "1" = gg_color_hue(2)[2]))  
	plot
})
output$cumlativeReports_percentMappedPlot <- renderPlot({
	myseq <- seq(from=1,to=nrow(df))
	t <- input$cumlativeReports_percentMappedFailThreshold

	sortby <- input$cumlativeReports_percentMappedGroup
	if (sortby != "none") {
		df <- df[order(df[[sortby]]),]
	}

	df$record <- myseq
	d <- data.frame(df$Sample , df$record, as.numeric(gsub("%","",df$Mapped..)))
	d$group <- as.factor((d[,3] > t)*1)
	colnames(d) <- c("sample","library","my_y","threshold")
	windowHeight <- 100.0
	plot <- ggplot(d, aes(x=library, y=my_y)) + geom_bar(stat="identity" , aes(fill=threshold) ) + labs(x="all libraries", y="percent of reads mapped to hg19") + scale_y_continuous(limits=c(0.0,windowHeight))
	plot <- plot + geom_hline(yintercept=t, color="red", linetype="dashed")
	plot <- plot + geom_text(aes(x=0,y=t+2,label=t ), color="red")  
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
		plot <- plot + geom_text(data=subset(d, my_y<t), aes(x=library,y=my_y-2,label=sample,hjust="right"), color="black", angle=90)
	}
      
	plot <- plot + theme(legend.position="none")
	plot <- plot + scale_fill_manual(values=c("0" = gg_color_hue(2)[1], "1" = gg_color_hue(2)[2]))  
	plot
})
output$cumlativeReports_percentOntPlot <- renderPlot({
	myseq <- seq(from=1,to=nrow(df))
	t <- input$cumlativeReports_percentOntFailThreshold

	sortby <- input$cumlativeReports_percentOntGroup
	if (sortby != "none") {
		df <- df[order(df[[sortby]]),]
	}

	df$record <- myseq
	d <- data.frame(df$Sample , df$record, as.numeric(gsub("%","",df$Reads.on.Target..)))
	d$group <- as.factor((d[,3] > t)*1)
	colnames(d) <- c("sample","library","my_y","threshold")
	windowHeight <- 100.0
	plot <- ggplot(d, aes(x=library, y=my_y)) + geom_bar(stat="identity" , aes(fill=threshold) ) + labs(x="all libraries", y="percent of mapped reads on target") + scale_y_continuous(limits=c(0.0,windowHeight))
	plot <- plot + geom_hline(yintercept=t, color="red", linetype="dashed")
	plot <- plot + geom_text(aes(x=0,y=t+2,label=t ), color="red")  
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
		plot <- plot + geom_text(data=subset(d, my_y<t), aes(x=library,y=my_y-2,label=sample,hjust="right"), color="black", angle=90)
	}
      
	plot <- plot + theme(legend.position="none")
	plot <- plot + scale_fill_manual(values=c("0" = gg_color_hue(2)[1], "1" = gg_color_hue(2)[2]))  
	plot
})
output$cumlativeReports_meanCoveragePlot <- renderPlot({
	myseq <- seq(from=1,to=nrow(df))
	t <- input$cumlativeReports_meanCoverageFailThreshold

	sortby <- input$cumlativeReports_meanCoverageGroup
	if (sortby != "none") {
		df <- df[order(df[[sortby]]),]
	}

	df$record <- myseq
	d <- data.frame(df$Sample , df$record, as.numeric(gsub("x","",df$Aver..Coverage)))
	d$group <- as.factor((d[,3] > t)*1)
	colnames(d) <- c("sample","library","my_y","threshold")
	windowHeight <- max(d$my_y) * 1.10
	plot <- ggplot(d, aes(x=library, y=my_y)) + geom_bar(stat="identity" , aes(fill=threshold) ) + labs(x="all libraries", y="mean coverage") + scale_y_continuous(limits=c(0.0,windowHeight))
	plot <- plot + geom_hline(yintercept=t, color="red", linetype="dashed")
	plot <- plot + geom_text(aes(x=0,y=t+2,label=t ), color="red")  
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
		plot <- plot + geom_text(data=subset(d, my_y<t), aes(x=library,y=my_y-2,label=sample,hjust="right"), color="black", angle=90)
	}
      
	plot <- plot + theme(legend.position="none")
	plot <- plot + scale_fill_manual(values=c("0" = gg_color_hue(2)[1], "1" = gg_color_hue(2)[2]))  
	plot
})
output$cumlativeReports_percentCoverageEightXPlot <- renderPlot({
	myseq <- seq(from=1,to=nrow(df))
	t <- input$cumlativeReports_percentCoverageEightXFailThreshold

	sortby <- input$cumlativeReports_percentCoverageEightXGroup
	if (sortby != "none") {
		df <- df[order(df[[sortby]]),]
	}

	df$record <- myseq
	d <- data.frame(df$Sample , df$record, as.numeric(gsub("%","",df$Base.coverage.at.8x)))
	d$group <- as.factor((d[,3] > t)*1)
	colnames(d) <- c("sample","library","my_y","threshold")
	windowHeight <- 100.0
	plot <- ggplot(d, aes(x=library, y=my_y)) + geom_bar(stat="identity" , aes(fill=threshold) ) + labs(x="all libraries", y="percent of target bases covered at 8x or higher") + scale_y_continuous(limits=c(0.0,windowHeight))
	plot <- plot + geom_hline(yintercept=t, color="red", linetype="dashed")
	plot <- plot + geom_text(aes(x=0,y=t+2,label=t ), color="red")  
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
		plot <- plot + geom_text(data=subset(d, my_y<t), aes(x=library,y=my_y-2,label=sample,hjust="right"), color="black", angle=90)
	}
      
	plot <- plot + theme(legend.position="none")
	plot <- plot + scale_fill_manual(values=c("0" = gg_color_hue(2)[1], "1" = gg_color_hue(2)[2]))  
	plot
})
output$cumlativeReports_minCoverageForNinetyPercentOfTargetPlot <- renderPlot({
	myseq <- seq(from=1,to=nrow(df))
	t <- input$cumlativeReports_minCoverageForNinetyPercentOfTargetFailThreshold

	sortby <- input$cumlativeReports_minCoverageForNinetyPercentOfTargetGroup
	if (sortby != "none") {
		df <- df[order(df[[sortby]]),]
	}

	df$record <- myseq
	d <- data.frame(df$Sample , df$record, as.numeric(gsub("x","",df$X90..covered.at)))
	d$group <- as.factor((d[,3] > t)*1)
	colnames(d) <- c("sample","library","my_y","threshold")
	windowHeight <- 100.0
	plot <- ggplot(d, aes(x=library, y=my_y)) + geom_bar(stat="identity" , aes(fill=threshold) ) + labs(x="all libraries", y="minimum level of coverage for 90% of target bases") + scale_y_continuous(limits=c(0.0,windowHeight))
	plot <- plot + geom_hline(yintercept=t, color="red", linetype="dashed")
	plot <- plot + geom_text(aes(x=0,y=t+2,label=t ), color="red")  
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
		plot <- plot + geom_text(data=subset(d, my_y<t), aes(x=library,y=my_y-2,label=sample,hjust="right"), color="black", angle=90)
	}
      
	plot <- plot + theme(legend.position="none")
	plot <- plot + scale_fill_manual(values=c("0" = gg_color_hue(2)[1], "1" = gg_color_hue(2)[2]))  
	plot
})
