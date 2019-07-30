


library(gbm)

m <- gbm( temperature_c ~ depth_scaled + X + Y, data = d_trawl1, n.trees = 2000, interaction.depth = 3, shrinkage = 0.02)

m2 <- gbm( temperature_c ~ SST_scaled + depth_scaled + X + Y, data = d_trawl1, n.trees = 2000, interaction.depth = 3, shrinkage = 0.02)

plot(m,i.var=2)
plot(m,i.var=3)
plot(m,i.var=4)
plot(m,i.var=5)


plot(m2,i.var=1)
plot(m2,i.var=2)
plot(m,i.var=3)
plot(m,i.var=4)

plot(m2,i.var=1:2)
plot(m2,i.var=c(1,3))
plot(m2,i.var=c(1,4))
d_trawl$r <- predict(m, n.trees = 2000) - d_trawl1$temperature_c

ggplot(d_trawl, aes(depth_scaled, r)) +
  geom_point(alpha=0.4) +
  ylim(-8,8) +
  geom_smooth()
