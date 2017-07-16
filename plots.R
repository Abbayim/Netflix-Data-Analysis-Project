df <- read.xlsx("item_tone_racial_aggregates_poverty_analysis.xlsx",1,stringsAsFactors=FALSE)

poverty_scores <- (df$poverty_1_ranked + df$poverty_2_ranked) / 2

keep <- which(!(df$poverty_1_ranked == min(poverty_scores)))
x <- poverty_scores[keep]

x_label <- "Ordered frequency of poverty-related words in synopsis"
ITERATION <- 6

png(paste("./plots/Tone of Poverty/", ITERATION, "-Anger", ".png", sep=""))
y <- df$anger_norm[keep]
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent anger") + ggtitle("Anger") + scale_y_continuous(limits=c(0,1))
dev.off()

png(paste("./plots/Tone of Poverty/", ITERATION, "-Disgust", ".png", sep=""))
y <- df$disgust_norm[keep]
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent disgust") + ggtitle("Disgust") + scale_y_continuous(limits=c(0,1))
dev.off()

png(paste("./plots/Tone of Poverty/", ITERATION, "-Fear", ".png", sep=""))
y <- df$fear_norm[keep]
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent fear") + ggtitle("Fear") + scale_y_continuous(limits=c(0,1))
dev.off()

png(paste("./plots/Tone of Poverty/", ITERATION, "-Joy", ".png", sep=""))
y <- df$joy_norm[keep]
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent joy") + ggtitle("Joy") + scale_y_continuous(limits=c(0,1))
dev.off()

png(paste("./plots/Tone of Poverty/", ITERATION, "-Sadness", ".png", sep=""))
y <- df$sadness_norm[keep]
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent sadness") + ggtitle("Sadness") + scale_y_continuous(limits=c(0,1))
dev.off()



png(paste("./plots/Color of Poverty/", ITERATION, "-whi", ".png", sep=""))
y <- df$p_whi[keep]
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent white") + ggtitle("White") + scale_y_continuous(limits=c(0,1))
dev.off()

png(paste("./plots/Color of Poverty/", ITERATION, "-bla", ".png", sep=""))
y <- df$p_bla[keep]
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Black") + ggtitle("Black") + scale_y_continuous(limits=c(0,1))
dev.off()

png(paste("./plots/Color of Poverty/", ITERATION, "-his", ".png", sep=""))
y <- df$p_his[keep]
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent hispanic") + ggtitle("Hispanic") + scale_y_continuous(limits=c(0,1))
dev.off()

png(paste("./plots/Color of Poverty/", ITERATION, "-asi", ".png", sep=""))
y <- df$p_asi[keep]
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Asian") + ggtitle("Asian") + scale_y_continuous(limits=c(0,1))
dev.off()

png(paste("./plots/Color of Poverty/", ITERATION, "-oth", ".png", sep=""))
y <- df$p_oth[keep]
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Other") + ggtitle("Other") + scale_y_continuous(limits=c(0,1))
dev.off()

# Ranked
png(paste("./plots/Ordered Color of Poverty/", ITERATION, "-whi", ".png", sep=""))
y <- df$p_whi_ranked[keep]
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent white") + ggtitle("White")
dev.off()

png(paste("./plots/Ordered Color of Poverty/", ITERATION, "-bla", ".png", sep=""))
y <- df$p_bla_ranked[keep]
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Black") + ggtitle("Black")
dev.off()

png(paste("./plots/Ordered Color of Poverty/", ITERATION, "-his", ".png", sep=""))
y <- df$p_his_ranked[keep]
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent hispanic") + ggtitle("Hispanic")
dev.off()

png(paste("./plots/Ordered Color of Poverty/", ITERATION, "-asi", ".png", sep=""))
y <- df$p_asi_ranked[keep]
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Asian") + ggtitle("Asian")
dev.off()

png(paste("./plots/Ordered Color of Poverty/", ITERATION, "-oth", ".png", sep=""))
y <- df$p_oth_ranked[keep]
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Other") + ggtitle("Other")
dev.off()

##
# Color of Anger

x <- df$anger_norm
x_label <- "Percent anger"
png(paste("./plots/Color of Anger/", ITERATION, "-whi", ".png", sep=""))
y <- df$p_whi
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent white") + ggtitle("White Anger") + scale_y_continuous(limits=c(0,1))
dev.off()

png(paste("./plots/Color of Anger/", ITERATION, "-bla", ".png", sep=""))
y <- df$p_bla
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Black") + ggtitle("Black Anger") + scale_y_continuous(limits=c(0,1))
dev.off()

png(paste("./plots/Color of Anger/", ITERATION, "-his", ".png", sep=""))
y <- df$p_his
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Hispanic") + ggtitle("Hispanic Anger") + scale_y_continuous(limits=c(0,1))
dev.off()

png(paste("./plots/Color of Anger/", ITERATION, "-asi", ".png", sep=""))
y <- df$p_asi
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Asian") + ggtitle("Asian Anger") + scale_y_continuous(limits=c(0,1))
dev.off()

png(paste("./plots/Color of Anger/", ITERATION, "-oth", ".png", sep=""))
y <- df$p_oth
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Other") + ggtitle("Other Anger") + scale_y_continuous(limits=c(0,1))
dev.off()


# Ranked
x <- df$anger_norm
x_label <- "Percent anger"
png(paste("./plots/Ordered Color of Anger/", ITERATION, "-whi", ".png", sep=""))
y <- df$p_whi_ranked
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent white, Ordered") + ggtitle("White Anger")
dev.off()

png(paste("./plots/Ordered Color of Anger/", ITERATION, "-bla", ".png", sep=""))
y <- df$p_bla_ranked
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Black, Ordered") + ggtitle("Black Anger") 
dev.off()

png(paste("./plots/Ordered Color of Anger/", ITERATION, "-his", ".png", sep=""))
y <- df$p_his_ranked
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Hispanic, Ordered") + ggtitle("Hispanic Anger") 
dev.off()

png(paste("./plots/Ordered Color of Anger/", ITERATION, "-asi", ".png", sep=""))
y <- df$p_asi_ranked
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Asian, Ordered") + ggtitle("Asian Anger") 
dev.off()

png(paste("./plots/Ordered Color of Anger/", ITERATION, "-oth", ".png", sep=""))
y <- df$p_oth_ranked
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Other, Ordered") + ggtitle("Other Anger") 
dev.off()

##
# Color of Disgust

x <- df$disgust_norm
x_label <- "Percent anger"
png(paste("./plots/Color of Disgust/", ITERATION, "-whi", ".png", sep=""))
y <- df$p_whi
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent white") + ggtitle("White Disgust")+ scale_y_continuous(limits=c(0,1))
dev.off()

png(paste("./plots/Color of Disgust/", ITERATION, "-bla", ".png", sep=""))
y <- df$p_bla
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Black") + ggtitle("Black Disgust")+ scale_y_continuous(limits=c(0,1))
dev.off()

png(paste("./plots/Color of Disgust/", ITERATION, "-his", ".png", sep=""))
y <- df$p_his
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Hispanic") + ggtitle("Hispanic Disgust")+ scale_y_continuous(limits=c(0,1))
dev.off()

png(paste("./plots/Color of Disgust/", ITERATION, "-asi", ".png", sep=""))
y <- df$p_asi
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Asian") + ggtitle("Asian Disgust")+ scale_y_continuous(limits=c(0,1))
dev.off()

png(paste("./plots/Color of Disgust/", ITERATION, "-oth", ".png", sep=""))
y <- df$p_oth
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Other") + ggtitle("Other Disgust")+ scale_y_continuous(limits=c(0,1))
dev.off()

# Ranked
x <- df$disgust_norm
x_label <- "Percent disgust"
png(paste("./plots/Ordered Color of Disgust/", ITERATION, "-whi", ".png", sep=""))
y <- df$p_whi_ranked
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent white, Ordered") + ggtitle("White Disgust")
dev.off()

png(paste("./plots/Ordered Color of Disgust/", ITERATION, "-bla", ".png", sep=""))
y <- df$p_bla_ranked
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Black, Ordered") + ggtitle("Black Disgust")
dev.off()

png(paste("./plots/Ordered Color of Disgust/", ITERATION, "-his", ".png", sep=""))
y <- df$p_his_ranked
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Hispanic, Ordered") + ggtitle("Hispanic Disgust")
dev.off()

png(paste("./plots/Ordered Color of Disgust/", ITERATION, "-asi", ".png", sep=""))
y <- df$p_asi_ranked
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Asian, Ordered") + ggtitle("Asian Disgust")
dev.off()

png(paste("./plots/Ordered Color of Disgust/", ITERATION, "-oth", ".png", sep=""))
y <- df$p_oth_ranked
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Other, Ordered") + ggtitle("Other Disgust")
dev.off()

##
# Color of Fear

x <- df$fear_norm
x_label <- "Percent fear"
png(paste("./plots/Color of Fear/", ITERATION, "-whi", ".png", sep=""))
y <- df$p_whi
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent white") + ggtitle("White Fear") + scale_y_continuous(limits=c(0,1))
dev.off()

png(paste("./plots/Color of Fear/", ITERATION, "-bla", ".png", sep=""))
y <- df$p_bla
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Black") + ggtitle("Black Fear") + scale_y_continuous(limits=c(0,1))
dev.off()

png(paste("./plots/Color of Fear/", ITERATION, "-his", ".png", sep=""))
y <- df$p_his
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Hispanic") + ggtitle("Hispanic Fear") + scale_y_continuous(limits=c(0,1))
dev.off()

png(paste("./plots/Color of Fear/", ITERATION, "-asi", ".png", sep=""))
y <- df$p_asi
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Asian") + ggtitle("Asian Fear") + scale_y_continuous(limits=c(0,1))
dev.off()

png(paste("./plots/Color of Fear/", ITERATION, "-oth", ".png", sep=""))
y <- df$p_oth
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Other") + ggtitle("Other Fear") + scale_y_continuous(limits=c(0,1))
dev.off()

# Ranked
x <- df$fear_norm
x_label <- "Percent Fear"
png(paste("./plots/Ordered Color of Fear/", ITERATION, "-whi", ".png", sep=""))
y <- df$p_whi_ranked
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent white, Ordered") + ggtitle("White Fear")
dev.off()

png(paste("./plots/Ordered Color of Fear/", ITERATION, "-bla", ".png", sep=""))
y <- df$p_bla_ranked
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Black, Ordered") + ggtitle("Black Fear")
dev.off()

png(paste("./plots/Ordered Color of Fear/", ITERATION, "-his", ".png", sep=""))
y <- df$p_his_ranked
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Hispanic, Ordered") + ggtitle("Hispanic Fear")
dev.off()

png(paste("./plots/Ordered Color of Fear/", ITERATION, "-asi", ".png", sep=""))
y <- df$p_asi_ranked
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Asian, Ordered") + ggtitle("Asian Fear")
dev.off()

png(paste("./plots/Ordered Color of Fear/", ITERATION, "-oth", ".png", sep=""))
y <- df$p_oth_ranked
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Other, Ordered") + ggtitle("Other Fear")
dev.off()


##
# Color of Joy
x <- df$joy_norm
x_label <- "Percent joy"
png(paste("./plots/Color of Joy/", ITERATION, "-whi", ".png", sep=""))
y <- df$p_whi
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent white") + ggtitle("White Joy")+ scale_y_continuous(limits=c(0,1))
dev.off()

png(paste("./plots/Color of Joy/", ITERATION, "-bla", ".png", sep=""))
y <- df$p_bla
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Black") + ggtitle("Black Joy")+ scale_y_continuous(limits=c(0,1))
dev.off()

png(paste("./plots/Color of Joy/", ITERATION, "-his", ".png", sep=""))
y <- df$p_his
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Hispanic") + ggtitle("Hispanic Joy")+ scale_y_continuous(limits=c(0,1))
dev.off()

png(paste("./plots/Color of Joy/", ITERATION, "-asi", ".png", sep=""))
y <- df$p_asi
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Asian") + ggtitle("Asian Joy")+ scale_y_continuous(limits=c(0,1))
dev.off()

png(paste("./plots/Color of Joy/", ITERATION, "-oth", ".png", sep=""))
y <- df$p_oth
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Other") + ggtitle("Other Joy")+ scale_y_continuous(limits=c(0,1))
dev.off()

# Ranked
x <- df$joy_norm
x_label <- "Percent Joy"
png(paste("./plots/Ordered Color of Joy/", ITERATION, "-whi", ".png", sep=""))
y <- df$p_whi_ranked
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent white, Ordered") + ggtitle("White Joy")
dev.off()

png(paste("./plots/Ordered Color of Joy/", ITERATION, "-bla", ".png", sep=""))
y <- df$p_bla_ranked
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Black, Ordered") + ggtitle("Black Joy")
dev.off()

png(paste("./plots/Ordered Color of Joy/", ITERATION, "-his", ".png", sep=""))
y <- df$p_his_ranked
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Hispanic, Ordered") + ggtitle("Hispanic Joy")
dev.off()

png(paste("./plots/Ordered Color of Joy/", ITERATION, "-asi", ".png", sep=""))
y <- df$p_asi_ranked
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Asian, Ordered") + ggtitle("Asian Joy")
dev.off()

png(paste("./plots/Ordered Color of Joy/", ITERATION, "-oth", ".png", sep=""))
y <- df$p_oth_ranked
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Other, Ordered") + ggtitle("Other Joy")
dev.off()


##
# Color of Sadness
x <- df$sadness_norm
x_label <- "Percent sadness"
png(paste("./plots/Color of Sadness/", ITERATION, "-whi", ".png", sep=""))
y <- df$p_whi
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent white") + ggtitle("White Sadness") + ggtitle("White Sadness") + scale_y_continuous(limits=c(0,1))
dev.off()

png(paste("./plots/Color of Sadness/", ITERATION, "-bla", ".png", sep=""))
y <- df$p_bla
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Black") + ggtitle("Black Sadness") + scale_y_continuous(limits=c(0,1))
dev.off()

png(paste("./plots/Color of Sadness/", ITERATION, "-his", ".png", sep=""))
y <- df$p_his
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Hispanic") + ggtitle("Hispanic Sadness") + scale_y_continuous(limits=c(0,1))
dev.off()

png(paste("./plots/Color of Sadness/", ITERATION, "-asi", ".png", sep=""))
y <- df$p_asi
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Asian") + ggtitle("Asian Sadness") + scale_y_continuous(limits=c(0,1))
dev.off()

png(paste("./plots/Color of Sadness/", ITERATION, "-oth", ".png", sep=""))
y <- df$p_oth
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Other") + ggtitle("Other Sadness") + scale_y_continuous(limits=c(0,1))
dev.off()

# Ranked
x <- df$sadness_norm
x_label <- "Percent sadness"
png(paste("./plots/Ordered Color of Sadness/", ITERATION, "-whi", ".png", sep=""))
y <- df$p_whi_ranked
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent white, Ordered") 
dev.off()

png(paste("./plots/Ordered Color of Sadness/", ITERATION, "-bla", ".png", sep=""))
y <- df$p_bla_ranked
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Black, Ordered") 
dev.off()

png(paste("./plots/Ordered Color of Sadness/", ITERATION, "-his", ".png", sep=""))
y <- df$p_his_ranked
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Hispanic, Ordered") 
dev.off()

png(paste("./plots/Ordered Color of Sadness/", ITERATION, "-asi", ".png", sep=""))
y <- df$p_asi_ranked
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Asian, Ordered") 
dev.off()

png(paste("./plots/Ordered Color of Sadness/", ITERATION, "-oth", ".png", sep=""))
y <- df$p_oth_ranked
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Other, Ordered") + ggtitle("Other Sadness")
dev.off()

