## Create data frame to hold slope and y coord location for annotating linear fit lines later
slopedata <- function(x, splitBy, formulaSlope){
  newdata <- ddply(x,splitBy,function(df)  ## Split sensi dataset by store location and transaction type and apply function
    data.frame(slope=format(signif(coef(lm(formulaSlope,data=df))[2],2), ## Record slope
                            scientific=-2), y = ifelse(coef(lm(formulaSlope,data=df))[2]<0,  ## Record y coord location
                                                       min(predict(lm(formulaSlope,data=df))),
                                                       max(predict(lm(formulaSlope,data=df))))))
  newdata <- newdata %>%
    mutate(slope = as.numeric(slope)*100) %>% ## Transform slope into percentage
    mutate(slope = as.character(slope)) ## Transform back to character
  
  return(newdata)
}