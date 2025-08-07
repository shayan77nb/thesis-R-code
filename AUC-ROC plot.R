r = roc(observation2,as.numeric(predicted_rf2))
p <- ggroc(r)
p <- p + ggtitle("ROS: Receiver Operating Characteristic Curve")
p <- p + ylab("Sensitivity")
p <- p + xlab("Specificity")
p <- p + geom_segment(
  aes(x = 1, xend = 0, y = 0, yend = 1), color = "grey", linetype = "dashed"
)
p <- p + scale_y_continuous(expand = c(0, 0))
p <- p + scale_x_reverse(expand = c(0, 0))
print(p)
ggroc(roc_score_rf1)
