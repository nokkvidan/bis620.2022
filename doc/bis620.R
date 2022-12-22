## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#"
)

## ----setup, message = FALSE---------------------------------------------------
library(bis620.2022)
library(dplyr)

# Load the data
data(diabetes)

# Splitting the dataset for train and test
set.seed(1)
sample <- sample(c(1, 2), nrow(diabetes), replace = TRUE, prob = c(.8, .2))

# convert all variables to categorical, except for Age and BMI
train <- diabetes[sample == 1, ] %>%
  mutate_at(vars(-c("Age", "BMI")), as.factor)
test <- diabetes[sample == 2, ] %>%
  mutate_at(vars(-c("Age", "BMI")), as.factor)

## ----vis2, warnings = FALSE, fig.width=7, fig.height=7------------------------
# Factor v continuous produces a violin plot
# Continuous v factor also produces a violin plot
p1 <- vis_2vars(diabetes, "Diabetes_binary", "BMI")
# Continuous v continuous produces a scatter plot
p2 <- vis_2vars(diabetes, "MentHlth", "BMI")
# Factor v factor produces a side by side bar plot
p3 <- vis_2vars(diabetes, "Diabetes_binary", "Sex")
p4 <- vis_2vars(diabetes, "Age", "Diabetes_binary")
gridExtra::grid.arrange(p1, p2, p3, p4)

## ----visnum, fig.width=7------------------------------------------------------
p5 <- vis_num(diabetes)

## ----visdist, fig.width=7, fig.height = 6-------------------------------------
df_dist <- diabetes[, c("BMI", "MentHlth", "Sex")]
vis_dist(df_dist)

## ---- eval = FALSE------------------------------------------------------------
#  # Fit a GLM to the diabetes data
#  glm_fit0 <- glm_model("Diabetes_binary", diabetes)
#  # Fit a random forest model to the diabetes data
#  rf_fit0 <- rf_model("Diabetes_binary", diabetes)
#  # Fit an XGBoost model to the diabetes data
#  xg_fit0 <- boost_model("Diabetes_binary", diabetes)

## ---- results = "hide", fig.show = "hide", message = FALSE, warning = FALSE----
# Fit and evaluate the performance of the GLM model
glm_fit <- glm_model("Diabetes_binary", train, test, optimize = "manual")
# Fit and evaluate the performance of the random forest model
rf_fit <- rf_model("Diabetes_binary", train, test, optimize = TRUE)
# Fit and evaluate the performance of the XGBoost model
xg_fit <- boost_model("Diabetes_binary", train, test)

## ---- results = "hide", fig.show = "hide", message = FALSE, warning = FALSE----
glm_fit <- glm_model("Diabetes_binary", diabetes, optimize = "manual")

## -----------------------------------------------------------------------------
glm_fit <- glm_model("Diabetes_binary", train, test, optimize = "manual")
summary(glm_fit)

## ---- fig.width=7, fig.height=7-----------------------------------------------
p1 <- vis_2vars(diabetes, "Diabetes_binary", "HighBP")
p2 <- vis_2vars(diabetes, "Diabetes_binary", "HighChol")
p4 <- vis_2vars(diabetes, "Sex", "Diabetes_binary")
p3 <- vis_2vars(diabetes, "Diabetes_binary", "BMI")
gridExtra::grid.arrange(p1, p2, p3, p4)

## ---- results = "hide", fig.show = "hide", message = FALSE, warning = FALSE----
rf_fit <- rf_model("Diabetes_binary", train, test, optimize = TRUE)

## -----------------------------------------------------------------------------
print(rf_fit[[1]])
rf_fit[[6]]

## -----------------------------------------------------------------------------
library(randomForest, include.only = "importance")
importance(rf_fit[[1]])
rf_fit[[7]]

## ---- results = "hide", fig.show = "hide", message = FALSE, warning = FALSE----
xg_fit <- boost_model("Diabetes_binary", train, test)

## -----------------------------------------------------------------------------
print(xg_fit[[1]])
print(xg_fit[[2]])
rf_fit[[7]]

## -----------------------------------------------------------------------------
print(xg_fit[[3]])

## -----------------------------------------------------------------------------
# xg boost counterfactual
get_counterfactual(model = xg_fit[[1]], data = test)

## -----------------------------------------------------------------------------
# random forest counterfactual
get_counterfactual(model = rf_fit[[1]], data = test)

## -----------------------------------------------------------------------------
# glm counterfactual
get_counterfactual(model = glm_fit[[1]], data = test)

