library(ggplot2)
library(grid)

#myPalette = c("#FF0000", "#E69F00", "#00FF00", "#0000FF")

#labels and axis size
text_size=18
#errorbar width
padding=0.5

#create theme
theme_bw_latex=theme_bw()+
	theme(
		plot.background = element_blank()
		,plot.margin=unit(x=c(0.5,0.5,0.5,0.5),"lines")
	#	,legend.margin=unit(0,"lines")
	#	,legend.justification=c(0,1)
		#,legend.position=c(0,1)
	#	,legend.key.height=unit(1.5,"line")
#		,panel.grid.major = element_blank()
#		,panel.grid.minor = element_blank()
#		,panel.border = element_blank()
		,axis.line = element_line()
		,text = element_text(family='Palatino',size=text_size)
		,legend.text = element_text(family='Palatino',size=text_size)
		,axis.text = element_text(family='Palatino',size=text_size)
		,axis.title = element_text(family='Palatino',size=text_size)
		,legend.position = 'right'
		,legend.direction = 'vertical'
	)

