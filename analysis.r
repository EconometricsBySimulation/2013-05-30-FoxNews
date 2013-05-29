# But you can download a table from the following link.
approval.rating <- read.csv("http://stata-bloggers.com/EconometricsBySimulation/2013-05-29-approval.csv")

head(approval.rating)

# Rename from Poll to Pollster
names(approval.rating)[1] <- "Pollster"

# I will add an PollNumber that will help keep track of the dates since the values do not include the year.
npolls <- nrow(approval.rating)
approval.rating$PollNumber <- npolls:1

# There is a lot of different pollsters
ggplot(approval.rating, aes(Pollster), aes(color, fill=Approve) ) + geom_bar() + coord_flip()

# We want to create approval and dissaproval bins for ploting
approval.rating$approve.bins <- as.factor(round(approval.rating$Approve/10)*10)
approval.rating$disapprove.bins <- as.factor(round(approval.rating$Disapprove/10)*10)

# Let's take a closer look at the top polsters
poll.table <- table(approval.rating$Pollster)
top <- names(poll.table[poll.table>60])

top.pollsters <- approval.rating[approval.rating$Pollster %in% top,]

require(ggplot2)
require("gridExtra")

# Bar plot of disaproval values
p1 <- ggplot(top.pollsters, aes(Pollster, fill=disapprove.bins)) + 
  coord_flip() + scale_fill_brewer(palette=7) + geom_bar(position="dodge") + 
  scale_y_reverse() + ylab("") + xlab("") + labs(fill="Disapprove")

# Bar plot of approval values
p2 <- ggplot(top.pollsters, aes(Pollster, fill=approve.bins) ) + coord_flip() + 
  scale_fill_brewer() + geom_bar(position="dodge") + ylab("") + xlab("") + 
  labs(fill="Approve")

# Combining the two.
grid.arrange(p1, p2, ncol=1, main="Obama Approval and Disapproval Ratings")

# Let's look at how the polls have moved over time.

p3 <- ggplot(top.pollsters, aes(group=Pollster, y=Disapprove, x=PollNumber) ) + geom_line() + aes(colour = Pollster) + geom_point(size = 3)

p4 <- ggplot(top.pollsters, aes(group=Pollster, y=Approve, x=PollNumber) )  + aes(colour = Pollster) + geom_point(size = 3) + geom_line()

# Combining the two.
grid.arrange(p3, p4, ncol=1, main="Obama Approval and Disapproval Ratings")

# Looking at only recent polls
p5 <- ggplot(top.pollsters[top.pollsters$PollNumber>600,], aes(group=Pollster, y=Disapprove, x=PollNumber) ) + geom_line() + aes(colour = Pollster) + geom_point(size = 3)

p6 <- ggplot(top.pollsters[top.pollsters$PollNumber>600,], aes(group=Pollster, y=Approve, x=PollNumber) )  + aes(colour = Pollster) + geom_point(size = 3) + geom_line()

# Combining the two.
grid.arrange(p5, p6, ncol=1, main="Obama Approval and Disapproval Ratings-Recent Polls")

# Find the averages for each 25 poll period
approval.rating$Group <- ceiling(approval.rating$PollNumber/25)
approval.rating$ApprAve <- ave(approval.rating$Approve, approval.rating$Group, FUN=mean)
approval.rating$DisAve <- ave(approval.rating$Disapprove, approval.rating$Group, FUN=mean)

# Calculate difference relative to average
approval.rating$AppDiff = approval.rating$Approve-approval.rating$ApprAve
approval.rating$DisDiff = approval.rating$Disapprove-approval.rating$DisAve

# Let's look again only at our top
top.pollsters <- approval.rating[approval.rating$Pollster %in% top,]
top.pollsters$Pollster <- factor(top.pollsters$Pollster)

# Now let's see what they look like plotted
p7 <- ggplot(top.pollsters, aes(group=Pollster, y=DisDiff, x=PollNumber) ) + geom_line() + aes(colour = Pollster) + geom_point(size = 3)

p8 <- ggplot(top.pollsters, aes(group=Pollster, y=AppDiff, x=PollNumber) )  + aes(colour = Pollster) + geom_point(size = 3) + geom_line()

# Combining the two.
grid.arrange(p7, p8, ncol=1, main="Obama Approval and Disapproval Ratings")

# I will use this vector
top.pollsters$unit <- 1
numb.polls <- tapply(top.pollsters$unit, top.pollsters$Pollster, sum)

top.pollsters$AppAbove <- (top.pollsters$AppDiff>0)
top.pollsters$DisAbove <- (top.pollsters$DisDiff>0)

# To see how the average different polls are relative to the mean polling.
top.results <- rbind(Approval.Gap = tapply(top.pollsters$AppDiff, top.pollsters$Pollster, mean) ,
                     Disapproval.Gap = tapply(top.pollsters$DisDiff, top.pollsters$Pollster, mean) ,
                     Approval.Above.Count = tapply(top.pollsters$AppAbove, top.pollsters$Pollster, sum)/numb.polls ,
                     Disapproval.Above.Count = tapply(top.pollsters$DisAbove, top.pollsters$Pollster, sum)/numb.polls
                     )
                     
write.csv(t(top.results), "topresults.csv")

setwd("C:\\Dropbox\\Econometrics by Simulation\\2013-05-May")

write.csv(summary(lm(formula = top.pollsters$AppDiff ~ top.pollsters$Pollster - 1))$coefficients, "lm1.csv")
write.csv(summary(lm(formula = top.pollsters$DisDiff ~ top.pollsters$Pollster - 1))$coefficients, "lm2.csv")
