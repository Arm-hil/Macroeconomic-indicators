library(fredr)
library(tidyverse)
library(readxl)
library(Hmisc)
library(here)
library(quantmod)
library(tidymodels)
tidymodels_prefer()
library(car)
library(ggpubr)
library(glmnet)
library(DescTools)
library(GGally)
library(reshape2)
library(qgraph)
library(MASS)

rm(list = ls())  # Clear all objects

#Leading indicators
# UMCSENT - Consumer Confidence Index (CCI)
# T10Y2Y - Yield Curve (10-Year Treasury - 2-Year Treasury Spread)
# PERMIT - New Privately-Owned Housing Units Authorized in Permit-Issuing Places
# M2SL - M2 Money Supply
# DGORDER - New Orders for Durable Goods. Business investment in machinery signals economic strength.
# BSCICP03USM665S - Business Confidence Index. Business future outlook
# STLFSI4 - St. Louis Fed Financial Stress Index. Measures financial market stress, which predicts recessions.
# USEPUINDXD - Economic Policy Uncertainty Index

#Coincident indicators
# INDPRO - Industrial Production Index. Measures output of factories, mines, and utilities.
# PCE - Personal Consumption Expenditures
# RSAFS - Retail Sales
# PAYEMS - Non-Farm Payrolls. More jobs → More income → Higher GDP.

#Lagging indicators
# UNRATE - Unemployment Rate
# CPIAUCSL - CPI. (Inflation values were yearly)
# CP - Corporate Profits

#Factors affecting GDP
# DSPIC96 - Disposable Personal Income
# GCEC1 - Real Government Consumption Expenditures & Gross Investment
# BOPGSTB - Trade Balance (Net Exports = X - M)
# GDPC1 - Real GDP 

fredr_set_key("68089fd8eb70628fda2a18887b481dbc")

indicators <- c("STLFSI4", "USEPUINDXD", "PAYEMS", "GDPC1",
                "UMCSENT", "T10Y2Y", "PERMIT", "DGORDER", 
                "M2SL", "INDPRO", "PCE", "RSAFS", "UNRATE", "CPIAUCSL", 
                "BSCICP03USM665S", "DSPIC96", "BOPGSTB", "GCEC1", "CP")

data_list <- list()

for (indicator in indicators) {
  df <- fredr(series_id = indicator, 
              observation_start = as.Date("1995-01-01"),
              observation_end = as.Date("2020-12-31"), 
              frequency = "q") %>%
    select(date, value) %>%
    rename(!!indicator := value) # Rename the value column to indicator name
  
  data_list[[indicator]] <- df
}

# Merging all data frames by date
data.fred <- Reduce(function(x, y) merge(x, y, by = "date", all = TRUE), data_list)

col_names <- c("date", "FSI", "EPUI", "NF_Payroll", "Real_GDP", "CCI", "T10Y2Y",
               "PERMIT", "DGORDER", "M_Supply", "IPI", "PCE",
               "Retail_Sales", "Unemp", "CPI", "BCI", "DPI",
               "Trade_Bal", "Gov_exp_inv", "Corp_Profit")
colnames(data.fred) <- col_names

# View the merged data
head(data.fred)
tail(data.fred)
describe(data.fred)

#Getting Geopolitical risk index
data.gpr <- read_excel(here("Data\\Global_GPR.xls")) %>% select(1,5)
colnames(data.gpr) <- c("date", "GPR")
View(data.gpr)

data.gpr$date <- as.Date(data.gpr$date, format="%Y-%m-%d")
data.gpr <- data.gpr %>% 
  filter(format(date, "%Y") %in% c(1995:2020) &
           format(date, "%m") %in% c("01", "04", "07", "10") & 
           format(date, "%d") == "01")
str(data.gpr)
head(data.gpr)

#Taking SP500 and VIX data from yahoo finance
tickers <- c("^GSPC", "^VIX")
names <- c("SP500", "VIX")

data_stock <- list()

for (i in seq_along(tickers)) {
  data <- getSymbols.yahoo(tickers[i], from = "1995-01-01", to = "2020-12-31",
                           periodicity = "monthly", auto.assign = FALSE)[,4]
  data <- data.frame(date = index(data), coredata(data))
  data$date <- as.Date(data$date, format="%Y-%m-%d")
  data <- data %>% 
    filter(format(date, "%m") %in% c("01", "04", "07", "10") &
             format(date, "%d") == "01")
  colnames(data)[2] <- names[i]
  
  data_stock[[names[i]]] <- data
}

# Converting list to data frame for merging later
data.index <- Reduce(function(x, y) merge(x, y, by = "date", all = TRUE), data_stock)
data.index <- as.data.frame(data.index)

head(data.index)
str(data.index)

#Merging all data sets together
data <- merge(data.fred, data.gpr, by = "date", all.x = TRUE)
data <- merge(data, data.index, by = "date", all.x = TRUE)

data.mlr.d <- data %>% select(-1)
str(data.mlr.d)

#Delving into the data set
sum(is.na(data.mlr.d)) # no NAs

#Visualizing linearity
data.viz.lin <- data.mlr.d %>% pivot_longer(cols = -Real_GDP, 
                                            names_to = "Predictor", 
                                            values_to = "Value")
ggplot(data.viz.lin, aes(x = Value, y = Real_GDP)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  facet_wrap(~ Predictor, scales = "free_x") +
  labs(title = "Linearity Check: Scatter Plots of Real GDP vs Predictors",
       x = "Predictor Value",
       y = "Real GDP") +
  theme_minimal()

#Some variables need to be looked into --- log() and ^2 have been used  
data.viz.lin.plot <- data.mlr.d

#BCI
BCI.outlier <- tail(sort(as.numeric(data.viz.lin.plot$BCI), decreasing = TRUE), n = 2)
data.BCI.plot <- data.viz.lin.plot %>% filter(!BCI %in% BCI.outlier)
ggplot(data.BCI.plot, aes(x = BCI, y = Real_GDP)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Linearity Check: Scatter Plots of Real GDP vs BCI",
      x = "BCI",
      y = "Real GDP") +
  theme_minimal()
#Does not show linearity 

data.BCI.winsorized <- data.viz.lin.plot
data.BCI.winsorized$BCI <- Winsorize(data.viz.lin.plot$BCI, 
                                     quantile(data.viz.lin.plot$BCI, 
                                              probs = c(0.05, 0.95), 
                                              na.rm = FALSE))
data.BCI.winsorized$Real_GDP <- Winsorize(data.viz.lin.plot$Real_GDP, 
                                     quantile(data.viz.lin.plot$Real_GDP, 
                                              probs = c(0.05, 0.95), 
                                              na.rm = FALSE))
ggplot(data.BCI.winsorized, aes(x = BCI, y = Real_GDP)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Linearity Check: Scatter Plots of Real GDP vs BCI",
       x = "BCI",
       y = "Real GDP") +
  theme_minimal()
#Non - linearity still exists

#CCI
data.viz.lin.plot %>% 
  ggplot(aes(x = CCI, y = Real_GDP)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Linearity Check: Scatter Plots of Real GDP vs CCI",
       x = "CCI",
       y = "Real GDP") +
  theme_minimal()
#Does not show linearity 

#EPUI
data.viz.lin.plot %>% 
  ggplot(aes(x = log(EPUI), y = Real_GDP)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Linearity Check: Scatter Plots of Real GDP vs EPUI",
       x = "EPUI",
       y = "Real GDP") +
  theme_minimal()
#Does not show linearity

data.EPUI.winsorized <- data.viz.lin.plot
data.EPUI.winsorized$EPUI <- Winsorize(data.viz.lin.plot$EPUI, 
                                     quantile(data.viz.lin.plot$EPUI, 
                                              probs = c(0.05, 0.95), 
                                              na.rm = FALSE))

data.EPUI.winsorized %>% 
  ggplot(aes(x = EPUI, y = Real_GDP)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Linearity Check: Scatter Plots of Real GDP vs EPUI",
       x = "EPUI",
       y = "Real GDP") +
  theme_minimal()
#Non - linearity still exists

#FSI
data.viz.lin.plot %>% 
  ggplot(aes(x = FSI, y = Real_GDP)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Linearity Check: Scatter Plots of Real GDP vs FSI",
       x = "FSI",
       y = "Real GDP") +
  theme_minimal()
#Does not show linearity (need to fix outliers)

data.FSI.winsorized <- data.viz.lin.plot
data.FSI.winsorized$FSI <- Winsorize(data.viz.lin.plot$FSI, 
                                       quantile(data.viz.lin.plot$FSI, 
                                                probs = c(0.05, 0.95), 
                                                na.rm = FALSE))

data.FSI.winsorized %>% 
  ggplot(aes(x = FSI, y = log(Real_GDP))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Linearity Check: Scatter Plots of Real GDP vs FSI",
       x = "FSI",
       y = "Real GDP") +
  theme_minimal()
#Non - linearity still exists

#GPR
data.viz.lin.plot %>% 
  ggplot(aes(x = log(GPR), y = Real_GDP)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Linearity Check: Scatter Plots of Real GDP vs GPR",
       x = "GPR",
       y = "Real GDP") +
  theme_minimal()
#Does not show linearity

#PERMIT
data.viz.lin.plot %>% 
  ggplot(aes(x = PERMIT^2, y = Real_GDP)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Linearity Check: Scatter Plots of Real GDP vs PERMIT",
       x = "PERMIT",
       y = "Real GDP") +
  theme_minimal()
#Does not show linearity

#T10Y2Y
data.viz.lin.plot %>% 
  ggplot(aes(x = T10Y2Y, y = log(Real_GDP))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Linearity Check: Scatter Plots of Real GDP vs T10Y2Y",
       x = "T10Y2Y",
       y = "Real GDP") +
  theme_minimal()
#Does not show linearity

#Trade balance
data.viz.lin.plot %>% 
  ggplot(aes(x = log(Trade_Bal), y = Real_GDP)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Linearity Check: Scatter Plots of Real GDP vs Trade_Bal",
       x = "Trade_Bal",
       y = "Real GDP") +
  theme_minimal()
#Does show linearity

#Unemployment
data.viz.lin.plot %>% 
  ggplot(aes(x = log(Unemp), y = Real_GDP)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Linearity Check: Scatter Plots of Real GDP vs Unemployment",
       x = "Unemployment",
       y = "Real GDP") +
  theme_minimal()
#Does not show linearity

#VIX
data.viz.lin.plot %>% 
  ggplot(aes(x = VIX^2, y = Real_GDP)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Linearity Check: Scatter Plots of Real GDP vs VIX",
       x = "VIX",
       y = "Real GDP") +
  theme_minimal()
#Does not show linearity

#Removing the non-linear variables
data.lin.clean <- data.mlr.d %>% select(-c(BCI, CCI, EPUI, FSI, GPR, PERMIT,
                                           T10Y2Y, Unemp, VIX))

#Visualizing correlation
data.cor.plot <- data.lin.clean
data.cor.plot %>% cor() %>% corrplot::corrplot()

#Removing extremely correlated values - trial and error
data.cor.rm <- data.cor.plot %>% select(-c(Real_GDP, DPI, M_Supply, PCE, 
                                           Retail_Sales, Corp_Profit, IPI))
data.cor.rm %>% cor()
ggpairs(data.cor.rm, upper = list(continuous = wrap("cor", size = 4))) +
  labs(title = "Pairwise Scatterplots with Correlation Coefficients")

qgraph(cor(data.cor.rm), layout = "spring", labels = colnames(data.cor.rm))

correlations <- sapply(data.cor.rm, function(x) cor(x, data.cor.rm$Real_GDP))
cor_df <- data.frame(Variable = names(correlations), Correlation = correlations)
ggplot(cor_df, aes(x = reorder(Variable, Correlation), y = Correlation, fill = Correlation)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  labs(title = "Correlation of Each Predictor with Real GDP", x = "Predictor", y = "Correlation")


#Train and test split
set.seed(18925)
data.split <- data.lin.clean %>% initial_split(prop = 0.8)
data.train <- data.split %>% training()
data.test <- data.split %>% testing()

linear.model <- linear_reg() %>% set_engine("lm") %>%  
  set_mode("regression")
data.recipe <- recipe(Real_GDP ~ ., 
                      data = data.train) %>%  
  step_corr(all_numeric_predictors(), threshold = 0.9) %>% #Since there was correlation present
  step_zv(all_predictors())

data.wkfl <- workflow() |> add_model(linear.model) |> 
  add_recipe(data.recipe)

cv.splits <- vfold_cv(data.train, v = 10)
cv.results <- fit_resamples(
  data.wkfl,
  resamples = cv.splits,
  metrics = metric_set(rmse, rsq))
cv.summary <- collect_metrics(cv.results)
print(cv.summary)

data.cv.fit <- data.wkfl %>% fit(data.train)
data.cv.fit |> extract_fit_parsnip() |> tidy()

###Residual plots
#normality of residuals
mlr.model <- data.cv.fit %>% extract_fit_parsnip() %>% pluck("fit")

residuals <- residuals(mlr.model)
ggqqplot(residuals, title = "QQ Plot of Residuals")
shapiro.test(residuals) #p-value > 0.05, residuals do not deviate significantly from normality.

par(mfrow = c(1, 2))  # Arrange plots side by side
plot(mlr.model, which = 1:2)

#Getting predictions and collecting metrics
data.pred.cv <- data.cv.fit %>%  predict(new_data = data.test) %>%  
  bind_cols(Actual = data.test$Real_GDP)

data.metrics.cv <- data.pred.cv %>%  metrics(truth = Actual, estimate = .pred)
print(data.metrics.cv)


data.fit <- data.wkfl |> fit(data = data.train)
data.fit |> extract_fit_parsnip() |> tidy()

#Getting VIF
vif(mlr.model) #VIF > 10 for NF_Payroll and IPI --- further improvements can be made



#####Attempt to make the model better -------------------------------------------

###Taking interaction terms
set.seed(18935)
base.model <- lm(Real_GDP ~ ., data = data.train)
interaction.model <- stepAIC(base.model, scope = ~.^2, direction = "forward")
summary(interaction.model)

#Getting predictions and collecting metrics
data.pred.intr <- data.frame(Actual = data.test$Real_GDP,
                        .pred = predict(interaction.model, newdata = data.test))

data.metrics.intr <- data.pred.intr %>%  metrics(truth = Actual, estimate = .pred)
print(data.metrics.intr)

###Doing lasso regression 
lasso.spec <- linear_reg(penalty = tune(), mixture = 1) %>% 
  set_engine("glmnet") %>%
  set_mode("regression")

lasso.wkfl <- workflow() %>% add_recipe(data.recipe) %>%
  add_model(lasso.spec)

set.seed(18930)
lasso.res <- tune_grid(
  lasso.wkfl,
  resamples = vfold_cv(data.train, v = 5),
  grid = 20,
  metrics = metric_set(rmse))

best.lasso <- select_best(lasso.res, metric = "rmse")

final.lasso.wkfl <- finalize_workflow(lasso.wkfl, best.lasso)

final.lasso.fit <- final.lasso.wkfl %>% fit(data.train)

final.lasso.fit %>% extract_fit_parsnip() %>% tidy() %>%
  arrange(desc(abs(estimate)))

#Getting prediction and collecting metrics
data.pred.lasso <- final.lasso.fit %>%  predict(new_data = data.test) %>%  
  bind_cols(Actual = data.test$Real_GDP)

data.metrics.lasso <- data.pred.lasso %>%  metrics(truth = Actual, estimate = .pred)
print(data.metrics.lasso)

###Performing PCA
pca.recipe <- recipe(Real_GDP ~ .,data = data.train) %>%  
  step_corr(all_numeric_predictors(), threshold = 0.9) %>% 
  step_normalize(all_predictors()) %>% 
  step_pca(all_predictors(), num_comp = 4, id = "pca") %>% 
  step_zv(all_predictors())
pca.train.prep <- pca.recipe %>% prep()

tidy(pca.train.prep, id="pca", type="variance")
pca.loadings <- tidy(pca.train.prep, id="pca", type="coef")
pca.loadings

pca.loadings |> 
  filter(component%in%c("PC1","PC2","PC3","PC4")) %>%   
  group_by(component) %>%   slice_max(abs(value), n=4) %>%   
  ungroup() %>%   
  ggplot(aes(terms, abs(value), fill=value>0)) +
  geom_col()+
  coord_flip()+
  facet_wrap(~ component, scales="free_y") +
  scale_y_continuous(labels = scales::percent) + 
  labs(title="Components extracted",
       subtitle="with 5 highest loadings of variables",
       y="Loadings (absolute values)",
       x="Variables")+ 
  scale_fill_discrete("Loading sign", labels=
                        c("Negative","Positive"))

pca.loadings %>%   
  filter(component%in%c("PC1","PC2","PC3","PC4")) %>%  
  ggplot(aes(terms, value, fill=component)) +
  geom_col(show.legend=FALSE)+
  coord_flip()+
  facet_wrap(~ component) +
  scale_y_continuous(labels = scales::percent)

pca.wkfl <- workflow() %>%  add_model(linear.model) %>%  
  add_recipe(pca.recipe)
pca.fit <- pca.wkfl %>% fit(data = data.train)
pca.fit %>% extract_fit_parsnip() %>% tidy()


#Getting predictions and collecting metrics
data.pred.pca <- pca.fit %>%  predict(new_data = data.test) %>%  
  bind_cols(Actual = data.test$Real_GDP)

data.metrics.pca <- data.pred.pca %>%  metrics(truth = Actual, estimate = .pred)
print(data.metrics.pca)

###Summary of the results
data.metrics.cv <- data.metrics.cv %>% mutate(Model = "Model_cv")
data.metrics.intr <- data.metrics.intr %>% mutate(Model = "Model_intr")
data.metrics.lasso <- data.metrics.lasso %>% mutate(Model = "Model_lasso")
data.metrics.pca <- data.metrics.pca %>% mutate(Model = "Model_pca")

all.metrics <- bind_rows(data.metrics.cv, data.metrics.intr, data.metrics.lasso, data.metrics.pca) %>%
  select(Model, .metric, .estimate) %>%
  pivot_wider(names_from = Model, values_from = .estimate)
print(all.metrics)


test.dates <- data$date[(nrow(data) - nrow(data.test) + 1):nrow(data)]

predictions <- data.frame(Date = test.dates,
                      MLR_pred = data.pred.cv$.pred,
                      Intr_pred = data.pred.intr$.pred,
                      Lasso_pred = data.pred.lasso$.pred,
                      PCA_pred = data.pred.pca$.pred,
                      Actual = data.test$Real_GDP)

#plotting interaction model and the pca model
ggplot(predictions, aes(x = Date)) +
  geom_point(aes(y = Actual, color = "Actual"), size = 2) +  
  geom_point(aes(y = Intr_pred, color = "Intr_pred"), size = 2) +  
  labs(title = "Predictions vs Actual Values",
       x = "Date",
       y = "Value",
       color = "Legend") +
  theme_minimal()

ggplot(predictions, aes(x = Date)) +
  geom_point(aes(y = Actual, color = "Actual"), size = 2) +  
  geom_point(aes(y = PCA_pred, color = "PCA_pred"), size = 2) +  
  labs(title = "Predictions vs Actual Values",
       x = "Date",
       y = "Value",
       color = "Legend") +
  theme_minimal()
