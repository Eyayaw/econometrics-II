cps_85 <- haven::read_dta("https://www.stata.com/data/jwooldridge/eacsap/cps78_85.dta")

# table(cps_85$y85)
# 0   1 
# 550 534 

# keep only year 1985 
cps_85 <- subset(cps_85, y85 == 1 )

# table(cps_85$y85)
# 1 
# 534 

# run OLS : lwage = a +b1female + b2union + b3nonwhite + b4educ + b5exper + b6exper2 + u

ols <- lm(lwage ~ female + union + nonwhite + educ + exper + I(exper**2), data = cps_85)

summary(ols)
