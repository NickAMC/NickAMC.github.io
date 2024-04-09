```{r}
library(mlbench)

set.seed(200)

simulated <- mlbench.friedman1(200, sd = 1)

simulated <- cbind(simulated$x, simulated$y)

simulated <- as.data.frame(simulated)

colnames(simulated)[ncol(simulated)] <- 'y'

simulated$duplicate1 <- simulated$V1 + rnorm(200) * .1

cor(simulated$duplicate1, simulated$V1)

```

```{r}
library(party)
bagCtrl <- cforest_control(mtry = ncol(simulated) - 1)
model <- cforest(y ~ ., data = simulated, controls = bagCtrl)
```

```{r}
# Standard importance
imp <- varimp(model)
print(imp)
```

```{r}
# Optional: Conditional importance
cond_imp <- varimp(model, conditional = TRUE) 
print(cond_imp)
```