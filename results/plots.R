library(ggplot2)
library(ggthemes)
library(extrafont)
library(plyr)
library(scales)
library(reshape)


df <- read.csv(file="res_petstore.csv", header=TRUE, sep=",")
df <- melt(df, id = 'n')
df <- arrange(df, n, -value) 
df <- ddply(df, .(n),
                     transform, pos = cumsum(value) - 0.5 *value)
df2 = subset(df, variable == "With.hidden.deps")
df2

                     
df$variable <- factor(df$variable, levels = c("With.hidden.deps","No.hidden.deps"), labels = c("Hidden dependencies","No hidden pendencies"))
                              
#p <- ggplot(data = df, aes(x = n, y = value, fill = variable, group=variable)) + geom_bar(width = 0.65, stat = 'identity', position = 'stack')
p <- ggplot() + theme_bw() + geom_bar(aes(y = value, x = n, fill = variable), data = df, stat="identity") + guides(fill=FALSE)
p <- p + geom_text(data=df2, aes(x = n, y = pos, label =  paste0(round((value/(pos-0.5*value))*100,digits=0),"%")), size=5)
#round(value/(pos-0.5*value)*100,digits=2))
p <- p + theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank())
p <- p + labs(x="Depth (n)", y="Average numbers of classes explored") + theme(axis.title.x=element_blank())
p <- p + scale_x_continuous(breaks=seq(2,14,1))
p




df <- read.csv(file="res_changeset.csv", header=TRUE, sep=",")
df <- melt(df, id = 'n')
df <- arrange(df, n, -value) 
df <- ddply(df, .(n),
                     transform, pos = cumsum(value) - 0.5 *value)
df2 = subset(df, variable == "With.hidden.deps")
df2

                     
df$variable <- factor(df$variable, levels = c("With.hidden.deps","No.hidden.deps"), labels = c("Hidden dependencies","No hidden pendencies"))
                              
#p <- ggplot(data = df, aes(x = n, y = value, fill = variable, group=variable)) + geom_bar(width = 0.65, stat = 'identity', position = 'stack')
p <- ggplot() + theme_bw() + geom_bar(aes(y = value, x = n, fill = variable), data = df, stat="identity")
p <- p + geom_text(data=df2, aes(x = n, y = pos, label =  paste0(round((value/(pos-0.5*value))*100,digits=0),"%")), size=6)
#round(value/(pos-0.5*value)*100,digits=2))
p <- p + theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank())
p <- p + labs(x="Depth (n)", y="Average numbers of classes explored")
p <- p + scale_x_continuous(breaks=seq(2,14,1))
p
