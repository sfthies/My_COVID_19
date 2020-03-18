# My_COVID_19

Some very basic (!) simulations and empirical analyses of the current COVID-19 developments. 
Empirical analyses are based on the reported COVID-19 cases by John Hopkins University (JHU), updated daily and available at the following github: [https://github.com/CSSEGISandData/COVID-19](https://github.com/CSSEGISandData/COVID-19)

- Covid_19_spread_simulation_heto_3.R contains the current stochastic simulation (based on binomial & poisson distributions for infection rates & actual infected people).  The simulations allow for varying population parameters, city and family clusters (further improvements required here) and include the possibility to introduce a lockdown
- Empirical analysis.R contains the code for the analysis of the daily updated JHU data: estimation of country specific growth rates (log-lin regression), estimation of time until recovery or death, fatality rates, ... (generally uncertainty estimates are not provided sufficiently here - further improvements required) 

Additional explanations of the simulation & assumptions are in the code and follow
