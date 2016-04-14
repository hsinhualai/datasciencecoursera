## Shiny App for the Course Project of Developing Data Products on Coursera

### Random sampling distributions for a proof of central limit theorem
In this Course Project, we develop a Shiny app for numerically proving the Central Limit Theorem (CLT). CLT in a nutshell states that the sampling distribution of the mean of any independent, random variable will be **normal** or nearly normal, if the sample size is large enough. We choose three sampling distributions as follows,

1. Exponential Distribution
2. Poisson Distribution
3. Uniform Distribution

During the simulations, we first generate 40 values out of random distribution functions and obtain the mean value. We then repeat doing this. In the shiny app, the slider gives the number of iterations ranging from 1 to 10000. We can see that all the sampling distribution of the mean values are normal. In the three slides later, I will introduce in order the Exponential Distribution, Poisson Distribution, and Uniform Distribution, and how to extract the theoretical mean and variance of the final normal distribution for a proof of CLT.

The Shiny App is at: https://hsinhualai.shinyapps.io/Course_Project_Develop_Dat_Products/

The Presentation is at: http://rpubs.com/hsinhua/170906