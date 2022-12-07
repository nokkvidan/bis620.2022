
#df <- read.csv("diabetes_binary_5050split_health_indicators_BRFSS2015.csv")
df <- read.csv("~/Desktop/diabetes_binary.csv")


library(ggplot2)

#' @param d diabetes data frame
#' @param y an arbitrary variable in diabetes data frame
#' @param x an arbitrary variable in diabetes data frame
#' @example \dontrun{
#' vis.2vars(df, 'Diabetes_binary', 'BMI')
#' }
vis.2vars <- function(d, y, x) {
  col1 <- d[, x]
  col2 <- d[, y]
  
  fac1 <- length(unique(col1)) <= 15
  fac2 <- length(unique(col2)) <= 15
  
  if (fac1 & fac2) {
    p <- ggplot(d, aes(x=eval(str2lang(x)), fill=eval(str2lang(y)))) + 
      geom_bar(position="dodge")
    subtitle <- 'Barplot visulization'
  }
  
  if (fac1 & !fac2) {
    p <- ggplot(d) + geom_violin(aes(x=eval(str2lang(x)), y=eval(str2lang(y))))
    subtitle <- 'Violinplot visulization'
  } 
  
  if (!fac1 & fac2) {
    p <- ggplot(d) + geom_violin(aes(x=eval(str2lang(x)), y=eval(str2lang(x))))
    subtitle <- 'Violinplot visulization'
  }
  
  if (!fac1 & !fac2) {
    p <- ggplot(d) + geom_point(aes(x=eval(str2lang(x)), y=eval(str2lang(y))))
    subtitle <- 'Scatterplot visulization'
  }
  
  title <- paste0('Visulizing the variables: ', x, ' & ', y)
  
  p <- p + labs(title = title, subtitle = subtitle, x = x, y = y) + theme_bw()
  
  return(p)
}

d <- df
x <- 'Diabetes_binary'
y <- 'BMI'
vis.2vars(d, x, y)


#' @param d diabetes data frame
#' @importFrom ggplot2 aes_string ggplot geom_line facet_grid aes
#' @importFrom gridExtra grid.arrange
#' @example \dontrun{
#' vis.num(df)
#' }
vis.num <- function(d) {
  d.num <- d[, sapply(d, is.numeric)]
  d.num.scaled <- data.frame(apply(d.num, 2, function(x) (x-min(x))/(max(x)-min(x))))
  
  p1 <- ggplot(stack(d.num), aes(x = ind, y = values)) +
    geom_boxplot() + 
    labs(title = "Visulizing Numeric Variables", 
         subtitle = 'Boxplot visulization',
         x = 'Values', 
         y = 'Variables') + 
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  p2 <- ggplot(stack(d.num.scaled), aes(x = ind, y = values)) +
    geom_boxplot() + 
    labs(title = "Visulizing Scaled Numeric Variables", 
         subtitle = 'Boxplot visulization',
         x = 'Scaled Values', 
         y = 'Variables') +
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

  return(gridExtra::grid.arrange(p1, p2, ncol = 2))
}

vis.num(df)


#' @param d diabetes data frame
#' @importFrom car qqPlot
#' @example \dontrun{
#' vis.dist(df)
#' }
vis.dist <- function(d) {
  df_num <- df[, sapply(df, is.numeric)]
  
  par(mfrow = c(2,2))
  # Plotting for loop
  for (i in (1:(ncol(df_num)))){
    hist(df_num[,i], col='red', pch=19, xlab = names(df_num)[i], 
         main = paste('Histogram of ', names(df_num)[i], sep = ''))
    qqPlot(df_num[,i], col='red', pch=19, ylab = names(df_num)[i], 
           main = paste('QQ-Plot for ', names(df_num)[i], sep = ''))
  }
}

#vis.dist(df)


#' @param d data
#' @param optimize
#' @importFrom xgboost xgb.DMatrix xgboost xgb.importance
#' @importFrom caret 
#' @importFrom car qqPlot
#' @importFrom pROC
#' @importFrom PRROC
#' @importFrom ROCR
#' @importFrom MASS stepAIC
#' @example \dontrun{
#' glm.model(df, optimize = 'manual')
#' glm.model(df, optimize = 'stepAIC')
#' }
glm.model <- function(d, optimize = NA) {
  
  fit <- glm(Diabetes_binary ~., data = d)
  sum <- summary(fit)
  
  if (is.na(optimize)) {
    
    final.fit <- fit
    
  } else if (optimize == 'manual') {
    
    sig <- which(sum$coef[, 4] < 0.1)[-1]
    sigvars <- rownames(sum$coefficients)[sig]
    
    form1 = as.formula(paste('Diabetes_binary', "~", 
                             paste(sigvars, collapse = '+')))
    
    fit1 <- glm(form1, data=d)
    sum1 <- summary(fit1)
    
    sig1 <- which(sum1$coef[, 4] < 0.05)[-1]
    sigvars1 <- rownames(sum1$coefficients)[sig1]
    
    form2 = as.formula(paste('Diabetes_binary', "~", 
                             paste(sigvars1, collapse = '+')))
    
    final.fit <- glm(form2, data=d)
    
  } else if (optimize == 'stepAIC') {
    step <- MASS::stepAIC(fit)
    
    sigvars <- names(step$coefficients[-1])
    form2 = as.formula(paste('Diabetes_binary', "~", 
                             paste(sigvars, collapse = '+')))
    final.fit <- glm(form2, data = d)
    
  }
  
  return(final.fit)
}

#glm.model(df, 'stepAIC')

################################################################################

library(car)
library(xgboost)
library(caret)
# Splitting up the data into train and test data sets
set.seed(1)
sample <- sample(c(1, 2), nrow(df), replace=TRUE, prob=c(0.8, 0.2))
train <- df[sample == 1, ]
test <- df[sample == 2, ]

y <- 'Diabetes_binary'

x.train <- data.matrix(train[, !colnames(train) %in% y])
y.train <- data.matrix(as.numeric(train[, colnames(train) %in% y]))
x.test <- data.matrix(test[, !colnames(test) %in% y])
y.test <- data.matrix(as.numeric(test[, colnames(test) %in% y]))

# convert the train and test data into xgboost matrix type.
boost.train <- xgb.DMatrix(data=x.train, label=y.train)
boost.test <- xgb.DMatrix(data=x.test, label=y.test)

# train a model using our training data
model <- xgboost(data = boost.train, max.depth = 15, nrounds = 30)

# use model to make predictions on test data
pred_test <- predict(model, boost.test)

pred_test.b <- as.factor(ifelse(pred_test > 0.5, 1, 0))
y.test <- factor(y.test)
pred_test <- as.numeric(pred_test)

conf_mat = confusionMatrix(y.test, pred_test.b)

# Compute feature importance matrix
importance_matrix = xgb.importance(colnames(boost.train), model = model)
xgb.plot.importance(importance_matrix[1:20,])

roc_test <- roc(y.test, pred_test, algorithm = 2,
                plot = TRUE, print.auc = TRUE) 


library(pROC)
library(PRROC)
library(ROCR)
# ROC and PRAUC
score1 = pred_test[y.test == 1]
score0 = pred_test[y.test == 0]

roc = roc.curve(score1, score0, curve = T)
pr = pr.curve(score1, score0, curve = T)

roc$auc
pr

plot(pr, main="Out-Of-sample PR curve")





