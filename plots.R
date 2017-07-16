poverty_scores <- (df$poverty_1_ranked + df$poverty_2_ranked) / 2

keep <- which(!(df$poverty_1_ranked == min(poverty_scores)))
x <- poverty_scores[keep]

x_label <- "Ordered frequency of poverty-related words in synopsis"
ITERATION <- 3

png(paste("./plots/Tone of Poverty/", ITERATION, "-Anger", ".png", sep=""))
y <- df$anger_norm[keep]
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent anger") + ggtitle("Anger")
dev.off()

png(paste("./plots/Tone of Poverty/", ITERATION, "-Disgust", ".png", sep=""))
y <- df$disgust_norm[keep]
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent disgust") + ggtitle("Disgust")
dev.off()

png(paste("./plots/Tone of Poverty/", ITERATION, "-Fear", ".png", sep=""))
y <- df$fear_norm[keep]
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent fear") + ggtitle("Fear")
dev.off()

png(paste("./plots/Tone of Poverty/", ITERATION, "-Joy", ".png", sep=""))
y <- df$joy_norm[keep]
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent joy") + ggtitle("Joy")
dev.off()

png(paste("./plots/Tone of Poverty/", ITERATION, "-Sadness", ".png", sep=""))
y <- df$sadness_norm[keep]
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent sadness") + ggtitle("Sadness")
dev.off()



png(paste("./plots/Color of Poverty/", ITERATION, "-whi", ".png", sep=""))
y <- df$p_whi[keep]
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent white") + ggtitle("White")
dev.off()

png(paste("./plots/Color of Poverty/", ITERATION, "-bla", ".png", sep=""))
y <- df$p_bla[keep]
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Black") + ggtitle("Black")
dev.off()

png(paste("./plots/Color of Poverty/", ITERATION, "-his", ".png", sep=""))
y <- df$p_his[keep]
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent hispanic") + ggtitle("Hispanic")
dev.off()

png(paste("./plots/Color of Poverty/", ITERATION, "-asi", ".png", sep=""))
y <- df$p_asi[keep]
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Asian") + ggtitle("Asian")
dev.off()

png(paste("./plots/Color of Poverty/", ITERATION, "-oth", ".png", sep=""))
y <- df$p_oth[keep]
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Other") + ggtitle("Other")
dev.off()

##
# Color of Anger

x <- df$anger_norm
x_label <- "Percent anger"
png(paste("./plots/Color of Anger/", ITERATION, "-whi", ".png", sep=""))
y <- df$p_whi
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent white") + ggtitle("White Anger")
dev.off()

png(paste("./plots/Color of Anger/", ITERATION, "-bla", ".png", sep=""))
y <- df$p_bla
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Black") + ggtitle("Black Anger")
dev.off()

png(paste("./plots/Color of Anger/", ITERATION, "-his", ".png", sep=""))
y <- df$p_his
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Hispanic") + ggtitle("Hispanic Anger")
dev.off()

png(paste("./plots/Color of Anger/", ITERATION, "-asi", ".png", sep=""))
y <- df$p_asi
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Asian") + ggtitle("Asian Anger")
dev.off()

png(paste("./plots/Color of Anger/", ITERATION, "-oth", ".png", sep=""))
y <- df$p_oth
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Other") + ggtitle("Other Anger")
dev.off()

##
# Color of Disgust

x <- df$disgust_norm
x_label <- "Percent anger"
png(paste("./plots/Color of Disgust/", ITERATION, "-whi", ".png", sep=""))
y <- df$p_whi
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent white") + ggtitle("White Disgust")
dev.off()

png(paste("./plots/Color of Disgust/", ITERATION, "-bla", ".png", sep=""))
y <- df$p_bla
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Black") + ggtitle("Black Disgust")
dev.off()

png(paste("./plots/Color of Disgust/", ITERATION, "-his", ".png", sep=""))
y <- df$p_his
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Hispanic") + ggtitle("Hispanic Disgust")
dev.off()

png(paste("./plots/Color of Disgust/", ITERATION, "-asi", ".png", sep=""))
y <- df$p_asi
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Asian") + ggtitle("Asian Disgust")
dev.off()

png(paste("./plots/Color of Disgust/", ITERATION, "-oth", ".png", sep=""))
y <- df$p_oth
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Other") + ggtitle("Other Disgust")
dev.off()

##
# Color of Fear

x <- df$fear_norm
x_label <- "Percent anger"
png(paste("./plots/Color of Fear/", ITERATION, "-whi", ".png", sep=""))
y <- df$p_whi
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent white") + ggtitle("White Fear")
dev.off()

png(paste("./plots/Color of Fear/", ITERATION, "-bla", ".png", sep=""))
y <- df$p_bla
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Black") + ggtitle("Black Fear")
dev.off()

png(paste("./plots/Color of Fear/", ITERATION, "-his", ".png", sep=""))
y <- df$p_his
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Hispanic") + ggtitle("Hispanic Fear")
dev.off()

png(paste("./plots/Color of Fear/", ITERATION, "-asi", ".png", sep=""))
y <- df$p_asi
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Asian") + ggtitle("Asian Fear")
dev.off()

png(paste("./plots/Color of Fear/", ITERATION, "-oth", ".png", sep=""))
y <- df$p_oth
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Other") + ggtitle("Other Fear")
dev.off()

##
# Color of Joy
x <- df$joy_norm
x_label <- "Percent anger"
png(paste("./plots/Color of Joy/", ITERATION, "-whi", ".png", sep=""))
y <- df$p_whi
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent white") + ggtitle("White Joy")
dev.off()

png(paste("./plots/Color of Joy/", ITERATION, "-bla", ".png", sep=""))
y <- df$p_bla
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Black") + ggtitle("Black Joy")
dev.off()

png(paste("./plots/Color of Joy/", ITERATION, "-his", ".png", sep=""))
y <- df$p_his
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Hispanic") + ggtitle("Hispanic Joy")
dev.off()

png(paste("./plots/Color of Joy/", ITERATION, "-asi", ".png", sep=""))
y <- df$p_asi
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Asian") + ggtitle("Asian Joy")
dev.off()

png(paste("./plots/Color of Joy/", ITERATION, "-oth", ".png", sep=""))
y <- df$p_oth
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Other") + ggtitle("Other Joy")
dev.off()

##
# Color of Sadness
x <- df$sadness_norm
x_label <- "Percent anger"
png(paste("./plots/Color of Sadness/", ITERATION, "-whi", ".png", sep=""))
y <- df$p_whi
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent white") + ggtitle("White Sadness")
dev.off()

png(paste("./plots/Color of Sadness/", ITERATION, "-bla", ".png", sep=""))
y <- df$p_bla
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Black") + ggtitle("Black Sadness")
dev.off()

png(paste("./plots/Color of Sadness/", ITERATION, "-his", ".png", sep=""))
y <- df$p_his
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Hispanic") + ggtitle("Hispanic Sadness")
dev.off()

png(paste("./plots/Color of Sadness/", ITERATION, "-asi", ".png", sep=""))
y <- df$p_asi
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Asian") + ggtitle("Asian Sadness")
dev.off()

png(paste("./plots/Color of Sadness/", ITERATION, "-oth", ".png", sep=""))
y <- df$p_oth
ggplot(data.frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method='auto') + xlab(x_label) + ylab("Percent Other") + ggtitle("Other Sadness")
dev.off()

