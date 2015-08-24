dataset.email <- sapply(data[which(data$spam == "email"), 1:54], function(x) ifelse(is.numeric(x), 
                                                                                          round(mean(x), 2), NA))
dataset.spam <- sapply(data[which(data$spam == "spam"), 1:54], function(x) ifelse(is.numeric(x), 
                                                                                        round(mean(x), 2), NA))

dataset.email.order <- dataset.email[order(-dataset.email)[1:10]]
dataset.spam.order <- dataset.spam[order(-dataset.spam)[1:10]]

par(mfrow = c(1, 2))
par(mar = c(8, 4, 4, 2) + 0.1)  # increase y-axis margin.
plot <- barplot(dataset.email.order, col = CUSTOM_COLORS_PLOT(10), main = "Email: Average Percentage", 
                names.arg = "", ylab = "Percentage Relative (%)")
text(x=plot,y=dataset.email.order-0.1, labels=dataset.email.order, cex=0.6)
vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)
grid.text(names(dataset.email.order), x = unit(plot, "native"), y = unit(-1, "lines"), just = "right", rot = 50)
popViewport(3)

plot <- barplot(dataset.spam.order, col = CUSTOM_COLORS_PLOT(10), main = "Spam: Average Percentage", 
                names.arg = "", ylab = "Percentage Relative (%)")
text(x=plot,y=dataset.spam.order-0.1, labels=dataset.spam.order, cex=0.6)
vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)
grid.text(names(dataset.spam.order), x = unit(plot, "native"), y = unit(-1, 
                                                                        "lines"), just = "right", rot = 50)
popViewport(3)

