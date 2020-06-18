library(sdmTMB)
library(ggplot2)
pred <- readRDS("~/github/dfo/gfranges/analysis/VOCC/data/predicted-DO-new.rds")

do <- readRDS("~/github/dfo/gfranges/analysis/tmb-sensor-explore/models/model-do-without-wcvi2016.rds")
predictions <- predict(do)
predictions$residuals <- residuals(do)


temp <- readRDS("~/github/dfo/gfranges/analysis/tmb-sensor-explore/models/model-temp-all-years-500kn.rds")
predictions2 <- predict(temp)
predictions2$residuals <- residuals(temp)

# plot(residuals~depth_scaled, data = predictions2)

ggplot(predictions2, aes(depth_scaled, residuals)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~year)



pred2 <- select(predictions2, fishing_event_id, t_est = est, t_residuals = residuals, t_depth = depth)

fit_checks <- left_join(predictions, pred2)


plot(residuals~depth, data = fit_checks)
plot(t_residuals~depth, data = fit_checks)

# residuals from both models not correlated
plot(t_residuals~residuals, data = fit_checks)

fits <- select(fit_checks, fishing_event_id, t_est, t_resids = t_residuals, d_est = est, d_resids = residuals)

saveRDS(fits, "climate-fits.rds")
