#’ @title Calculate total excretion
#’ @description This function calculates excretion rate over time
#’ @details This function takes into account the amount of time between nutrient density measurements, the size of the excretion chamber, and the size of the samples taken from the excretion chamber to calculate individual excretion rate given nutrient density measurements
#’
#’ @param interval this parameter is the amount of time between each sample taken
#’ @param volume this parameter is the total volume of the excretion chamber at the time the organism(s) are first placed within
#‘ @param sample this parameter is the volume of the sample taken from the excretion chamber at each time interval
#' @param density1 this parameter is the total nutrient concentration of the first sample taken from the excretion chamber
#' @param density2 this parameter is the total nutrient concentration of the second sample taken from the excretion chamber
#' @param density3 this parameter is the total nutrient concentration of the third sample taken from the excretion chamber
#‘
#‘ @returns The output from this function is a data frame containing four columns: Time, Raw_Nutrients, Density, and Rate. In the column labelled Rate, the newly calculated excretion rates will appear in row 2 and 3 - note that row 1 will appear as NA because no excretion will have taken place before the first time interval. Row 2 will display the excretion rate in the time between the first and second samples, and row 3 will display the excretion rate in the time between the second and third samples. For those interested solely in the individual excretion rate and not the dataframe which facilitates calculation of the mass-specific excretion rate, a printout of these values will also appear in the console.
#' @examples
#' ##### Example 1 #####
#' library(xcrete)
#'
#' # Enter parameters and measured nutrient densities
#' total.xcrete(1,100,1,0.5,15.5,27)
#'
#' @export

total.xcrete <- function(interval, volume, sample, density1, density2, density3){
  raw1 <- density1/volume
  raw2 <- density2/(volume-sample)
  raw3 <- density3/(volume-sample*2)
  change1 <- raw2-raw1
  change2 <- raw3-raw2
  rate1 <- change1/interval
  rate2 <- change2/interval

  Raw_Nutrients = c(raw1, raw2, raw3)
  Rate = c(NA, rate1, rate2)
  Density = c(density1, density2, density3)
  Time = c(NA, interval, interval*2)

  xcrete_output <<- data.frame(Time, Raw_Nutrients, Density, Rate)

  print(paste("Time interval 1 excretion rate:",rate1))
  print(paste("Time interval 2 excretion rate:",rate2))
}

#’ @title Calculate mass-specific excretion rate
#’ @description This function calculates mass-specific excretion rate
#’ @details This function takes organism mass and previously calculated excretion rates from the data frame that results from the total.xcrete function and calculates mass-specific excretion rates
#’
#’ @param mass this parameter is the mass of the organism(s) in the excretion chamber
#‘
#‘ @returns The outputs from this function are the mass-specific excretion rates for each time interval, both printed in the console and added to the previously-existing dataframe
#' @examples
#' ##### Example 1 #####
#' library(xcrete)
#'
#' # After calculating the individual excretion rates with the total.xcrete function, enter the organisms mass as the only parameter
#' mse.xcrete(2)
#'
#' @export

mse.xcrete <- function(mass){
  Rate <- c(xcrete_output$Rate)
  MSE <- c()
  j <- 0
  for(i in Rate){
    x=i/mass
    MSE = c(MSE,x)
    print(paste("Time interval",j,"mass-specific excretion rate:",x))
    j <- j+1}
  xcrete_output <<- cbind(xcrete_output,MSE)
}

#’ @title Graphs mass-specific excretion rate
#’ @description This function graphs mass-specific excretion rate
#’ @details This function takes the mass-specific excretion rates from the mse.xcrete function and graphs them on the y-axis with time on the x-axis
#’
#’ @param df_name this parameter is the exact name of the dataframe within which the MSE had been calculated - if unaltered after the mse.xcrete function, this will be "xcrete_output"
#‘
#‘ @returns The output of this function is a dotplot with time on the x-axis and mass-specific excretion rate on the y-axis
#' @examples
#' ##### Example 1 #####
#' library(xcrete)
#'
#' # After calculating the mass-specific excretion rates with the mse.xcrete function, enter the dataframe name as the only parameter
#' graph.xcrete(xcrete_output)
#'
#' @export

graph.xcrete <- function(df_name){
  ggplot(df_name,aes(Time,MSE))+
    geom_point()+
    stat_summary(fun="mean",geom="point",size=3)+
    theme(axis.title=element_text(size=14))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    labs(y="MSE")
}
