##Course Project for Statistical Inference course on Coursera

###Problem 1
In this project we will investigate the exponential distribution in R and compare it with the Central Limit Theorem. The exponential distribution can be simulated in R with rexp(n, lambda) where lambda is the rate parameter. The mean of exponential distribution is 1/lambda and the standard deviation is also 1/lambda. Set lambda = 0.2 for all of the simulations. You will investigate the distribution of averages of 40 exponentials. Note that you will need to do a thousand simulations.

Illustrate via simulation and associated explanatory text the properties of the distribution of the mean of 40 exponentials.  You should
1. Show the sample mean and compare it to the theoretical mean of the distribution.
2. Show how variable the sample is (via variance) and compare it to the theoretical variance of the distribution.
3. Show that the distribution is approximately normal.

### Problem 2
In this project we analyze the ToothGrowth dataset in the R package. We first give a brief summary of the data. We then propose a hypothesis saying that the OJ supplement is more efficient than VJ supplement on the tooth growth. We then extract the t confidence interval of the data of each dose and supplement averaged over the sample size of each specific dose.