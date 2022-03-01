# Old Code from Lab1 Part 1

```{r, echo=FALSE, comment="", fig.width=4, fig.height=2, fig.align='center'}
# Check whether the variables have the similar shape/spread - box whisker plot or histogram 
gghistogram(dem_sen, main = 'Age, Democrat Senators', x = 'age', fill= 'steelblue', bins = 5, xlab = 'Age', ylab = 'Count')
```

```{r, echo=FALSE, comment="", fig.width=4, fig.height=2, fig.align='center'}
# Histogram of Republican senator ages 
gghistogram(rep_sen, main = 'Age, Republican Senators', x = 'age', fill= 'steelblue', bins = 5, xlab = 'Age', ylab = 'Count')
# Check for some metric on similarities between the two variable distributions
```

```{r, echo=FALSE, comment="", fig.width=4, fig.height=2, fig.align='center'}
boxplot(x=rep_sen$age, data = rep_sen,
        main="Age, Republican Senators",
        ylab="Age",
        col = 'steelblue',
        horizontal = TRUE)
```

```{r, echo=FALSE, comment="", fig.width=4, fig.height=2, fig.align='center'}
gghistogram(wine_data, main = 'g1', x = 'difference', y = '..density..', fill= 'steelblue', bins = 5, add_density = TRUE)

```

```{r, echo=FALSE, comment="", fig.width=4, fig.height=2, fig.align='center'}
gghistogram(rel_data$prottemp, fill= 'steelblue', bins = 5, xlab = 'Feeling Thermometer Rating', ylab = 'Count', main = 'Distribution of U.S. Feelings, Protestants')
```

```{r, echo=FALSE, comment="", fig.width=4, fig.height=2, fig.align='center'}
diff_prottemp_cathtemp <- as.numeric(rel_data$prottemp - rel_data$cathtemp)
gghistogram(diff_prottemp_cathtemp, fill= 'steelblue', bins = 5, xlab = 'Feeling Thermometer Rating', ylab = 'Count', main = 'Distribution of Feelings Difference')
```

```{r, echo=FALSE, comment=""}
# Conduct a Shapiro test to also help determine normality 
shapiro.test(diff_prottemp_cathtemp)
```

