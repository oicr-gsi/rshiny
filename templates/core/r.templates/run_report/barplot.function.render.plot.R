
output$.TAB.PLOT.VAR.Plot <- renderPlotly({
	t <- input$.TAB.PLOT.VAR.FailThreshold

	select_column1 <- input$.TAB.PLOT.VAR.SelectColumn1
	select_type1 <- input$.TAB.PLOT.VAR.SelectType1
	select_value1 <- input$.TAB.PLOT.VAR.SelectValue1
	if (select_column1 != "none") {
		if (select_type1 == "equals") {
			rrdf <- rrdf[which(rrdf[[select_column1]] == select_value1),]
		} else if (select_type1 == "contains") {
			rrdf <- rrdf[which(grepl(select_value1 , rrdf[[select_column1]])),]
		} else if (select_type1 == "does_not_equal") {
			rrdf <- rrdf[which(rrdf[[select_column1]] != select_value1),]
		} else if (select_type1 == "does_not_contain") {
			rrdf <- rrdf[which(!grepl(select_value1 , rrdf[[select_column1]])),]
		}
	}
	groupby <- input$.TAB.PLOT.VAR.Group
	if (groupby != "none") {
		rrdf <- rrdf[order(rrdf[[groupby]]),]
	}

	myseq <- seq(from=1,to=nrow(rrdf))
	rrdf$record <- myseq
	d <- data.frame(rrdf$Library , rrdf$record, .Y.VALUE.VAR. )
	d$is_pass <- as.factor((d[,3] > t)*1)
	colnames(d) <- c("library","record","value","is_pass")
	d <- data.frame(d , rrdf)
	windowHeight <- .WINDOW.HEIGHT.STRING.
	orientation <- input$.TAB.PLOT.VAR.OrientationRadio
	plot_type <- input$.TAB.PLOT.VAR.PlotTypeRadio

	plot <- ggplot(d, aes(x=record, y=value))
	if (plot_type == "bar") {
		plot <- plot + geom_bar(stat="identity" , aes(text=paste("Library:" , library , "
Run:" , Run , "
Lane:" , Lane , "
Barcode:",Barcode) ,fill=is_pass)) 
	} else if (plot_type == "point") {
		plot <- plot + geom_point(stat="identity" , aes(text=paste("Library:" , library , "
Run:" , Run , "
Lane:" , Lane , "
Barcode:",Barcode) , color=is_pass))   
	}

	plot <- plot + labs(x="all libraries", y=".Y.LABEL.")
	plot <- plot + scale_y_continuous(limits=c(0.0,windowHeight))
	plot <- plot + geom_rect(data=NULL , aes(text=paste("threshold:", t) , x = NULL , y = NULL , xmin=0 , xmax=nrow(rrdf) , ymin=t , ymax=t), color="red" , linetype="dashed")
	plot <- plot + geom_text(aes(x=0,y=t+(windowHeight * 0.02),label=t ), color="red")
	unique.groupby <- unique(rrdf[[groupby]])

	first_records <- c()
	last_records <- c()
	my_cols <- c()
	my_cols2 <- c()
	y1 <- c()
	y2 <- c()
	y3 <- c()
	y4 <- c()
    
	if (groupby != "none") {
		for (i in 1:length(unique.groupby)){
			unique.group <- unique.groupby[i]
			group.lines <- rrdf[which(rrdf[[groupby]] == unique.group),]
			first.record <- head(group.lines , n=1)$record - 0.5
			last.record <- tail(group.lines , n=1)$record + 0.5
			color <- (i %% 2) + 10
			color2 <- (i %% 2) + 20
            
			first_records[i] <- first.record
			last_records[i] <- last.record
			my_cols[i] <- color
			my_cols2[i] <- color2
			y1[i] <- windowHeight * 0.98
			y2[i] <- windowHeight
			y3[i] <- 0.0
			y4[i] <- windowHeight
		}
        
		group.df <- data.frame(unique.groupby , first_records , last_records , as.factor(my_cols) , as.factor(my_cols2) , y1 , y2 , y3 , y4)
		colnames(group.df) <- c("unique.groupby" , "first_records" , "last_records" , "color_code" , "color_code_2" , "y1" , "y2" , "y3" , "y4")
		plot <- plot + geom_rect(data=group.df , aes(text=paste(groupby , ":" , unique.groupby) , x = NULL , y = NULL , xmin=first_records , xmax=last_records , ymin=y1 , ymax=y2 , fill=color_code))

		plot <- plot + geom_rect(data=group.df , aes(text=paste(groupby , ":" , unique.groupby) , x = NULL , y = NULL , xmin=first_records , xmax=first_records , ymin=y3 , ymax=y4 , alpha=0.1), color="gray")
		plot <- plot + geom_rect(data=group.df , aes(text=paste(groupby , ":" , unique.groupby) , x = NULL , y = NULL , xmin=tail(last_records,n=1) , xmax=tail(last_records,n=1) , ymin=y3 , ymax=y4 , alpha=0.1), color="gray")  
	}

	plot <- plot + theme(legend.position="none" , axis.text.y = element_text(angle=45))
	plot <- plot + scale_fill_manual(values=c("0" = gg_color_hue(2)[1], "1" = gg_color_hue(2)[2] , "10" = gg_color_hue(6)[3] , "11" = gg_color_hue(10)[9] , "20" = "gray" , "21" = "lightgray"))
	plot <- plot + scale_color_manual(values=c("0" = gg_color_hue(2)[1], "1" = gg_color_hue(2)[2] , "10" = gg_color_hue(6)[3] , "11" = gg_color_hue(10)[9] , "20" = "gray" , "21" = "lightgray"))

	if (orientation == "horizontal") {
		plot <- plot + scale_x_continuous(trans="reverse")
		plot <- plot + coord_flip()    
	}

	ggplotly(plot)
})
