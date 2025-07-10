deer.df <- read.table("RM_deer.txt", header = TRUE)
ddf <- subset(deer.df, Tag == 204)
attach(ddf)

library(splines)

pred.dfr <- data.frame(Agewks = seq(29, 88, 1))

plot(ME ~ Agewks, data = ddf, main = "B-spline regression")

for (i in c(2, 4, 8)) {
  ME.bs <- lm(ME ~ bs(Agewks, df = i))
  preds.bs <- predict(ME.bs, newdata = pred.dfr, se = TRUE)
  lines(pred.dfr$Agewks, preds.bs$fit, lwd = 2, lty = i)
}

legend(35, 250, legend = c("df=2", "df=4", "df=8"), lwd = 2, lty = c(1, 2, 3))

savePlot("MEbspline", type = "ps")

detach(ddf)


###
y <- c(13.9, 13.81, 14.08, 13.99, 13.75, 13.60, 13.32, 13.39, 13.45, 13.53, 13.59, 13.64)
x <- c(0.5, 0.5, 1.0, 1.0, 1.5, 1.5, 2.0, 2.0, 2.5, 2.5, 3.0, 3.0)

skin.df <- data.frame(x, y)
names(skin.df) <- c("Conc", "SkinResp")

attach(skin.df)

conc.rs <- lm(data = skin.df, SkinResp ~ Conc + I(Conc^2) + I(Conc^3))
preds.rs <- predict(conc.rs, newdata = skin.df)

skin.df$preds.rs <- preds.rs
# Prediction grid
newdata <- data.frame(Conc = seq(min(skin.df$Conc), max(skin.df$Conc), length.out = 100))
newdata$pred <- predict(conc.rs, newdata = newdata)

plot(skin.df$Conc, skin.df$SkinResp,
     main = "Cubic Polynomial Regression",
     xlab = "Concentration",
     ylab = "Skin Response",
     pch = 19, col = "blue", cex = 1.5)
lines(newdata$Conc, newdata$pred, col = "red", lwd = 2)


for (i in c(4,4.5,5)){  
Skin.bs <- lm(data = skin.df, SkinResp ~ bs(Conc, df = i))
preds.bs <- predict(Skin.bs, newdata = newdata, se = TRUE)
lines(newdata$Conc, preds.bs$fit, lwd = 2, lty = i)
  
}

legend("topright",
       legend = c("df = 2", "df = 3", "df = 4"),
       lwd = 2,
       lty = c(1, 5, 10)) 
              