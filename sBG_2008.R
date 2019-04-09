####################################################################################
# Customer Lifetime Projection with the discrete sBG Model                         #
# Based on "How to Project Customer Retention (2007)"                              #
# by Peter S. Fader and Bruce G.S. Hardie                                          #
# available at https://www.sciencedirect.com/science/article/pii/S1094996807700233 #
#                                                                                  #
# v1.0                                                                             #
#                                                                                  #
# Alexander Kulumbeg                                                               #
# alexander.kulumbeg@wu.ac.at                                                      #
# Service Marketing and Tourism Institute                                          #
# Vienna University of Economics and Business                                      #
####################################################################################

# install.packages("ggplot2")
# install.packages("plotly")
# install.packages("readxl")
# install.packages("RColorBrewer")

library(ggplot2)      # main plots
library(plotly)       # interactive plots
library(readxl)       # reading in .xlsx files
library(RColorBrewer) # Color pallettes for plotly

##### Functions ---------------------------------------------------------

# Churn rate function, P(T = t)
# Refer to Figure 7 in the paper
# Returns a vector with churn probabilities for each period
sBG_Churn <- Vectorize(function(alpha, beta, period) {
  if (period == 1) {
    churn <- alpha / (alpha + beta)
  } else if (period > 1) {
    churn <- sBG_Churn(alpha, beta, period - 1) * (beta + period - 2) / (alpha + beta + period - 1)
  } else {
    churn <- stop("Incorrect argument 'period', cannot be less than 1")
  }
  return(churn)
})

# Survivor function, S(t | alpha, beta)
# Calculated as 
# Returns a vector with survival probabilities for each period
sBG_Survival <- Vectorize(function(alpha, beta, period) {
  if (period == 1) {
    survival <- 1 - sBG_Churn(alpha, beta, 1)
  } else if (period > 1) {
    survival <- sBG_Survival(alpha, beta, period - 1) - sBG_Churn(alpha, beta, period)
  } else {
    survival <- stop("Incorrect argument 'period', cannot be less than 1")
  }
  return(survival)
})

# Log-likelihood function
# Refer to figure B2 (Appendix B) in the paper
# Returns a single numeric value representing the computed log-likelihood
# Note: the log-likelihood value is in fact negative, but for other computations, we need the absolute value
sBG_LL <- function(paramsAB, active, lost) {
  if (length(active) != length(lost)) {
    stop("'active' and 'lost' do not have the same length: ", length(active), " and ", length(lost), ".")
  }
  alpha <- paramsAB[1]
  beta <- paramsAB[2]
  t <- length(active) # number of periods
  LL <- (-as.numeric(sum(lost*log(sBG_Churn(alpha, beta, 1:t))) + active[t]*log(sBG_Survival(alpha, beta, t))))
  return(LL)
}

# Randomizing function that creates new random data
# Default: 6 to 12 years (including), arguments can be changed
# Output: List with three elements - random active customers, derived lost customers, t (period)
randomCust <- function(init = 1000, min = 6, max = 12, perc_min = 0.75, perc_max = 0.90) {
  length <- as.integer(sample(min:max, 1))
  randomActiveCust <- numeric(length)
  randomActiveCust[1] <- init #cohort starts with 1000 customers
  for (i in 2:length) {
    randomActiveCust[i] <- round(randomActiveCust[i - 1]*runif(1, perc_min, perc_max), 0)
  }
  randomLostCust <- c(1:(length - 1))
  for (i in 1:length(randomLostCust)) {
    randomLostCust[i] <- randomActiveCust[i] - randomActiveCust[i + 1]
  }
  randomActiveCust <- randomActiveCust[-1] #delete the first value
  randomCustList <- list(randomActiveCust, randomLostCust, length - 1)
  names(randomCustList) <- c("active", "lost", "t")
  randomCustList$t <- c(1:randomCustList$t)
  randomCustList <- as.data.frame(randomCustList)
  return(randomCustList)
}

##### Data --------------------------------------------------------------

t <- 0:12

# HIGHEND COHORT

highend_cust <- c(1000, 869, 743, 653, 593, 551, 517, 491, 468, 445, 427, 409, 394)
df <- data.frame("t" = t[1:8], "high" = highend_cust[1:8])

pred_lin_high <- round(predict(lm(high ~ t, df), data.frame(t = 0:12)), 3)
pred_quad_high <- round(predict(lm(high ~ poly(t, 2, raw = TRUE), df), data.frame(t = c(0:12))), 3)
pred_exp_high <- round(exp(predict(lm(log(high) ~ t, df), data.frame(t = c(0:12)))), 3)

df_highend <- data.frame(t, highend_cust)
colnames(df_highend)[2] <- "alive"
df_highend$lost[1] <- 0
for (i in 2:nrow(df_highend)) {
  df_highend$lost[i] <- df_highend$alive[i-1] - df_highend$alive[i]
}

df_highend <- cbind(df_highend, pred_lin_high, pred_quad_high, pred_exp_high)

rm(pred_exp_high, pred_lin_high, pred_quad_high, df, i, highend_cust)

# REGULAR COHORT

regular_cust <- c(1000, 631, 468, 382, 326, 298, 262, 241, 223, 207, 194, 183, 173)
df <- data.frame("t" = t[1:8], "reg" = regular_cust[1:8])

pred_lin_reg <- round(predict(lm(reg ~ t, df), data.frame(t = 0:12)), 3)
pred_quad_reg <- round(predict(lm(reg ~ poly(t, 2, raw = TRUE), df), data.frame(t = c(0:12))), 3)
pred_exp_reg <- round(exp(predict(lm(log(reg) ~ t, df), data.frame(t = c(0:12)))), 3)

df_regular <- data.frame(t, regular_cust)
colnames(df_regular)[2] <- "alive"
df_regular$lost[1] <- 0
for (i in 2:nrow(df_regular)) {
  df_regular$lost[i] <- df_regular$alive[i-1] - df_regular$alive[i]
}

df_regular <- cbind(df_regular, pred_lin_reg, pred_quad_reg, pred_exp_reg)

rm(pred_lin_reg, pred_quad_reg, pred_exp_reg, df, i, regular_cust)

##### Plotting ----------------------------------------------------------
# Figure 1 and Figure 2 respectively, recreated in (ggplot and) plotly

# # ggplot highend (Fig. 1)
# ggplot(df_highend, aes(x = t)) +
#   geom_line(aes(y = alive)) +
#   geom_line(aes(y = pred_lin_high)) +
#   geom_line(aes(y = pred_quad_high)) +
#   geom_line(aes(y = pred_exp_high)) +
#   labs(title = "Actual Versus Regression-Model-Base Estimates of the percentage of High End Customers Surviving at least 0-12 Years (Figure 1)")
# 
# # ggplot regular (Fig. 2)
# ggplot(df_regular, aes(x = t)) +
#   geom_line(aes(y = alive)) +
#   geom_line(aes(y = pred_lin_reg)) +
#   geom_line(aes(y = pred_quad_reg)) +
#   geom_line(aes(y = pred_exp_reg)) +
#   labs(title = "Actual Versus Regression-Model-Base Estimates of the percentage of High End Customers Surviving at least 0-12 Years (Figure 1)")

# !!! From now only using plotly due to its interactivity. Make sure to check whether JavaScript is enabled. !!!

# plotly highend (Fig. 1)
plot_ly(df_highend, x = t) %>%
  add_trace(y = ~alive, name = "Actual", mode = "lines+markers") %>%
  add_trace(y = ~pred_lin_high, name = "Prediction: Linear Model", mode = "lines+markers") %>%
  add_trace(y = ~pred_quad_high, name = "Prediction: Quadratic Model", mode = "lines+markers") %>%
  add_trace(y = ~pred_exp_high, name = "Prediction: Exponential Model", mode = "lines+markers") %>%
  add_segments(x = 7, xend = 7, y = 0, yend = 1000, line = list(dash = "dot"), name = "Calibration") %>%
  layout(title = 'Actual Versus Regression-Model-Base Estimates of the Percentage of \n High End Customers Surviving at least 0-12 Years (Figure 1)',
         xaxis = list(title = 'Period (t)',
                      zeroline = TRUE,
                      range = c(0, 13),
                      fixedrange=TRUE),
         yaxis = list(title = 'Surviving Customers',
                      range = c(0,1000),
                      (fixedrange=TRUE)
                      ))
  

# plotly regular (Fig. 2)
plot_ly(df_regular, x = t, colorscale = "Viridis") %>%
  add_trace(y = ~alive, name = "Actual", mode = "lines+markers", type = "scatter") %>%
  add_trace(y = ~pred_lin_reg, name = "Prediction: Linear Model", mode = "lines+markers", type = "scatter") %>%
  add_trace(y = ~pred_quad_reg, name = "Prediction: Quadratic Model", mode = "lines+markers", type = "scatter") %>%
  add_trace(y = ~pred_exp_reg, name = "Prediction: Exponential Model", mode = "lines+markers", type = "scatter") %>%
  add_segments(x = 7, xend = 7, y = -400, yend = 1200, line = list (dash = "dot"), name = "Calibration", type = "scatter") %>%
  layout(title = 'Actual Versus Regression-Model-Base Estimates of the percentage of \n High End Customers Surviving at least 0-12 Years (Figure 1)',
         xaxis = list(title = 'Period (t)',
                      zeroline = TRUE,
                      range = c(0, 13),
                      fixedrange=TRUE),
         yaxis = list(title = 'Surviving Customers',
                      range = c(-400,1200),
                      (fixedrange=TRUE)
         ))




##### Adding sBG Predictions --------------------------------------------

# HIGHEND

# Table 1, High-end, first 7 years (calibration period, t1 -> t7)
activeCust_high = df_highend$alive[2:8] # we skip period 0, so we need to shift indices by 1
lostCust_high = df_highend$lost[2:8]

sBG_LL(c(1,1), activeCust_high, lostCust_high)

optimParam_high <- optim(c(1,1), sBG_LL, active = activeCust_high, lost = lostCust_high, method = "L-BFGS-B", lower = 0.0001)

# Visualize the churn rate, survival rate and likelihood levels
# plotly requires dataframes, so we will create one
time <- 1:7 # 7 periods / years; t = 7
churnValues <- sBG_Churn(alpha = optimParam_high$par[1], # Alpha param obtained by finding the maximum log-likelihood
                        beta = optimParam_high$par[2],   # Beta param obtained by finding the maximum log-likelihood
                        period = time)
churnDF <- data.frame(x = time, 
                      y = churnValues)


# Churn plot
plot_ly(data = churnDF, x = ~x) %>%
  add_trace(y = ~y, type = "scatter", mode = "lines+markers") %>%
  layout(title = paste0("Visualization of Churn rate with alpha = ", 
                        round(optimParam_high$par[1], 3), 
                        " and beta = ",
                        round(optimParam_high$par[2], 3)),
         xaxis = list(title = "Period (t)",
                      fixedrange = TRUE),
         yaxis = list(title = "Churn probability; P(T=t | alpha, beta)",
                      fixedrange = TRUE))



survivalValues <- sBG_Survival(alpha = optimParam_high$par[1], # Alpha param obtained by finding the maximum log-likelihood
                              beta = optimParam_high$par[2],  # Beta param obtained by finding the maximum log-likelihood
                              period = time)
survivalDF <- data.frame(x = time, 
                         y = survivalValues)

# Survival plot
plot_ly(data = survivalDF, x = ~x) %>%
  add_trace(y = ~y, type = "scatter", mode = "lines+markers") %>%
  layout(title = paste0("Visualization of Survival rate with alpha = ", 
                        round(optimParam_high$par[1], 3), 
                        " and beta = ",
                        round(optimParam_high$par[2], 3)),
         xaxis = list(title = "Period (t)",
                      fixedrange = TRUE),
         yaxis = list(title = "Survival probability; P(T=t | alpha, beta)",
                      fixedrange = TRUE))


# Log-Likelihood contour plot
x <- seq(from = optimParam_high$par[1] - 0.5, to = optimParam_high$par[1]+3, length.out = 50) # play around with the params
y <- seq(from = optimParam_high$par[2] - 0.5, to = optimParam_high$par[2]+3, length.out = 50) # play around with the params
u <- expand.grid(x, y) #df of all parameter combinations
colnames(u) <- c("alpha", "beta")
for (i in 1:nrow(u)) {
  u$LL[i] <- sBG_LL(c(u$alpha[i], u$beta[i]), activeCust_high, lostCust_high)
}

plot_ly(data = u,
        x = ~alpha, 
        y = ~beta, 
        z = ~LL, type = "contour",
        reversescale = T,
        colorscale = "Jet",
        autocontour = F,
        contours = list(coloring = 'heatmap',
                        showlabels = TRUE,
                        start = min(u$LL),
                        end = max(u$LL),
                        size = 50)) %>%
  layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))


# With the optimized parameters, we attempt to predict the survival for 12 years
pred_sBG_high <- c(1000, sBG_Survival(optimParam_high$par[1], optimParam_high$par[2], 1:12) * 1000)

df_highend <- cbind(df_highend, pred_sBG_high)

plot_ly(df_highend, x = t) %>%
  add_trace(y = ~alive, name = "Actual", mode = "lines+markers", type = "scatter") %>%
  add_trace(y = ~pred_lin_high, name = "Prediction: Linear Model", mode = "lines+markers", type = "scatter") %>%
  add_trace(y = ~pred_quad_high, name = "Prediction: Quadratic Model", mode = "lines+markers", type = "scatter") %>%
  add_trace(y = ~pred_exp_high, name = "Prediction: Exponential Model", mode = "lines+markers", type = "scatter") %>%
  add_trace(y = ~pred_sBG_high, name = "Prediction: sBG Model", mode = "lines+markers", type = "scatter") %>%
  add_segments(x = 7, xend = 7, y = 0, yend = 1000, line = list(dash = "dot"), name = "Calibration") %>%
  layout(title = 'Actual Versus Regression-Model-Base Estimates of the Percentage of \n High End Customers Surviving at least 0-12 Years (Figure 1)',
         xaxis = list(title = 'Period (t)',
                      zeroline = TRUE,
                      range = c(0, 13),
                      fixedrange=TRUE),
         yaxis = list(title = 'Surviving Customers',
                      range = c(0,1000),
                      (fixedrange=TRUE)
         ))

# REGULAR
activeCust_reg = df_regular$alive[2:8] # we skip period 0, so we need to shift indices by 1
lostCust_reg = df_regular$lost[2:8]

sBG_LL(c(1,1), activeCust_reg, lostCust_reg)

optimParam_reg <- optim(c(1,1), sBG_LL, active = activeCust_reg, lost = lostCust_reg, method = "L-BFGS-B", lower = 0.0001)

# Visualize the churn rate, survival rate and likelihood levels
# plotly requires dataframes, so we will create one
time <- 1:7 # 7 periods / years; t = 7
churnValues_reg <- sBG_Churn(alpha = optimParam_reg$par[1], # Alpha param obtained by finding the maximum log-likelihood
                         beta = optimParam_reg$par[2],   # Beta param obtained by finding the maximum log-likelihood
                         period = time)
churnDF_reg <- data.frame(x = time, 
                      y = churnValues_reg)

# Churn plot
plot_ly(data = churnDF_reg, x = ~x) %>%
  add_trace(y = ~y, type = "scatter", mode = "lines+markers") %>%
  layout(title = paste0("Visualization of Churn rate with alpha = ", 
                        round(optimParam_reg$par[1], 3), 
                        " and beta = ",
                        round(optimParam_reg$par[2], 3)),
         xaxis = list(title = "Period (t)",
                      fixedrange = TRUE),
         yaxis = list(title = "Churn probability; P(T=t | alpha, beta)",
                      fixedrange = TRUE))

survivalValues_reg <- sBG_Survival(alpha = optimParam_reg$par[1], # Alpha param obtained by finding the maximum log-likelihood
                               beta = optimParam_reg$par[2],  # Beta param obtained by finding the maximum log-likelihood
                               period = time)
survivalDF_reg <- data.frame(x = time, 
                         y = survivalValues_reg)

# Survival plot
plot_ly(data = survivalDF_reg, x = ~x) %>%
  add_trace(y = ~y, type = "scatter", mode = "lines+markers") %>%
  layout(title = paste0("Visualization of Survival rate with alpha = ", 
                        round(optimParam_reg$par[1], 3), 
                        " and beta = ",
                        round(optimParam_reg$par[2], 3)),
         xaxis = list(title = "Period (t)",
                      fixedrange = TRUE),
         yaxis = list(title = "Survival probability; P(T=t | alpha, beta)",
                      fixedrange = TRUE))


# Log-Likelihood contour plot
x <- seq(from = optimParam_reg$par[1] - 0.5, to = optimParam_reg$par[1]+3, length.out = 50) # play around with the params
y <- seq(from = optimParam_reg$par[2] - 0.5, to = optimParam_reg$par[2]+3, length.out = 50) # play around with the params
u <- expand.grid(x, y) #df of all parameter combinations
colnames(u) <- c("alpha", "beta")
for (i in 1:nrow(u)) {
  u$LL[i] <- sBG_LL(c(u$alpha[i], u$beta[i]), activeCust_reg, lostCust_reg)
}

plot_ly(data = u,
        x = ~alpha, 
        y = ~beta, 
        z = ~LL, type = "contour",
        reversescale = T,
        colorscale = "Jet",
        autocontour = F,
        contours = list(coloring = 'heatmap',
                        showlabels = TRUE,
                        start = min(u$LL),
                        end = max(u$LL),
                        size = 50)) %>%
  layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))


# With the optimized parameters, we attempt to predict the survival for 12 years
pred_sBG_reg <- c(1000, sBG_Survival(optimParam_reg$par[1], optimParam_reg$par[2], 1:12) * 1000)

df_regular <- cbind(df_regular, pred_sBG_reg)

plot_ly(df_regular, x = t) %>%
  add_trace(y = ~alive, name = "Actual", mode = "lines+markers", type = "scatter") %>%
  add_trace(y = ~pred_lin_reg, name = "Prediction: Linear Model", mode = "lines+markers", type = "scatter") %>%
  add_trace(y = ~pred_quad_reg, name = "Prediction: Quadratic Model", mode = "lines+markers", type = "scatter") %>%
  add_trace(y = ~pred_exp_reg, name = "Prediction: Exponential Model", mode = "lines+markers", type = "scatter") %>%
  add_trace(y = ~pred_sBG_reg, name = "Prediction: sBG Model", mode = "lines+markers", type = "scatter") %>%
  add_segments(x = 7, xend = 7, y = 0, yend = 1000, line = list(dash = "dot"), name = "Calibration") %>%
  layout(title = 'Actual Versus Regression-Model-Base Estimates of the Percentage of \n High End Customers Surviving at least 0-12 Years (Figure 2)',
         xaxis = list(title = 'Period (t)',
                      zeroline = TRUE,
                      range = c(0, 13),
                      fixedrange=TRUE),
         yaxis = list(title = 'Surviving Customers',
                      range = c(0,1000),
                      (fixedrange=TRUE)
         ))


##### Extrapolating Predictions to 16 years -----------------------------

# HIGHEND only

m <- as.data.frame(matrix(nrow = 4, ncol = 3))

df_highend_16 <- df_highend[, c("t", "alive", "lost")]
colnames(m) <- c("t", "alive", "lost")
df_highend_16 <- rbind(df_highend_16, m)
rm(m)
rownames(df_highend_16) <- 1:nrow(df_highend_16)
df_highend_16$t <- 0:16

alive <- na.omit(df_highend_16$alive)
alive <- alive[-1]
lost <- na.omit(df_highend_16$lost)
lost <- lost[-1]

optimParam_16 <- optim(c(1,1), sBG_LL, active = alive, lost = lost, method = "L-BFGS-B", lower = 0.0001)

pred_sBG_16 <- c(1000, round(sBG_Survival(optimParam_16$par[1], optimParam_16$par[2], 1:16), 3) * 1000)

df_highend_16 <- cbind(df_highend_16, pred_sBG_16)


plot_ly(df_highend_16, x = ~t) %>%
  add_trace(y = ~alive, name = "Actual", mode = "lines+markers", type = "scatter") %>%
  add_trace(y = ~pred_sBG_16, name = "Prediction: sBG Model", mode = "lines+markers", type = "scatter") %>%
  add_segments(x = 12, xend = 12, y = 0, yend = 1000, line = list(dash = "dot"), name = "Calibration") %>%
  layout(title = 'Actual Versus Regression-Model-Base Estimates of the Percentage of \n High End Customers Surviving at least 0-12 Years (Figure 2)',
         xaxis = list(title = 'Period (t)',
                      zeroline = TRUE,
                      range = c(0, 18),
                      fixedrange=TRUE),
         yaxis = list(title = 'Surviving Customers',
                      range = c(0,1000),
                      (fixedrange=TRUE)
         ))




