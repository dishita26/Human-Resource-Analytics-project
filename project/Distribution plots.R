# Histograms #
par(mfrow=c(1,3))
hist(HR_analytics$satisfaction_level, col="green")
hist(HR_analytics$last_evaluation, col="red")
hist(HR_analytics$average_montly_hours, col="blue")

# Salary vs TURNOVER#
vis_1<-table(HR_analytics$salary,HR_analytics$left)
#print(vis_1)
d_vis_1<-as.data.frame(vis_1)
print(d_vis_1)
library(ggplot2)
p<-ggplot(d_vis_1, aes(x=Var1,y=Freq,fill=Var2)) + geom_bar(position="dodge",stat='identity') + coord_flip()

print(p)

#Department vs turnover 
vis_2<-table(HR_analytics$roles,HR_analytics$left)
d_vis_2<-as.data.frame(vis_2)
d_vis_2<-subset(d_vis_2,Var2==1)
#print(d_vis_2)
library(ggplot2)
d_vis_2$Var1 <- factor(d_vis_2$Var1, levels = d_vis_2$Var1[order(-d_vis_2$Freq)])
p<-ggplot(d_vis_2, aes(x=Var1,y=Freq,fill=Var1)) + geom_bar(stat='identity') +theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(p)

# Turnover vs project count#
vis_3 <- table(HR_analytics$number_project, HR_analytics$left)
d_vis_3 <- as.data.frame(vis_3)
p <- ggplot(d_vis_3, aes(x= Var1, y= Freq, fill= Var2))+
  geom_bar(position = "dodge",stat = 'identity') + coord_flip()
print(p)

# Turnover vs evaluation #
# Kernel Density plot
left_data <- subset(HR_analytics, left ==1)
stay_data <- subset(HR_analytics, left==0)
ggplot()+geom_density(aes(x = last_evaluation), colour = "green", data = left_data)+
  geom_density(aes(x= last_evaluation), colour = "orange", data = stay_data)

# Turnover vs Average Monthly hours #
ggplot() + geom_density(aes(x=average_montly_hours), colour="red", data=left_data)+
  geom_density(aes(x=average_montly_hours),colour = "blue", data = stay_data)

#Turnover vs satisfaction #
#KDEPlot: Kernel Density Estimate Plot
ggplot() + geom_density(aes(x=satisfaction_level), colour="red", data=left_data) + 
  geom_density(aes(x=satisfaction_level), colour="blue", data=stay_data)

#ProjectCount VS AverageMonthlyHours [BOXPLOT]
library(ggplot2)
p<-ggplot(HR_analytics, aes(x = factor(number_project), y = average_montly_hours, fill = factor(left))) +
  geom_boxplot() + scale_fill_manual(values = c("yellow", "orange"))
print(p)

#ProjectCount VS Evaluation
p<-ggplot(HR_analytics, aes(x = factor(number_project), y = last_evaluation, fill = factor(left))) +
  geom_boxplot() + scale_fill_manual(values = c("yellow", "orange"))
print(p)

# Satisfaction vs Evaluation 
library(ggplot2)
ggplot(HR_analytics, aes(satisfaction_level, last_evaluation, color = left)) +
  geom_point(shape = 16, size = 5, show.legend = FALSE) +
  theme_minimal() +
  scale_color_gradient(low = "#0091ff", high = "#f0650e")