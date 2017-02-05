<<<<<<< HEAD
#' Rio Negro floods
#' 
#' Years of major floods between 1892-1992 (inclusive) on the Rio Negro River in Brazil
=======
#' description
#' years of major floods between 1892-1992 (inclusive) on the Rio Negro River in Brazil
>>>>>>> origin/master
#'
#' @format A dataframe with 101 rows and 2 variables:
#' \describe{
#'  \item{Year}{Year of reference}
#'  \item{Counts}{Count of floods in a given year, note that the variance is .166 and the mean is .208}
#' }
#'
#' @source (Brillinger , 1995; Guttorp , 1995)
"floodcount"

#' Rio Negro flood wait times
#' 
#' Time between major floods between 1892-1992 (inclusive) on the Rio Negro River in Brazil
#'
#' @format A dataframe with 20 rows and 1 variable:
#' \describe{
#'  \item{WT}{Number of years between floods}
#' }
#'
#' @source (Brillinger , 1995; Guttorp , 1995)
"floodwait"

#' Fetal lamb movements
#'
#' Number of movements by a fetal lamb observed by ultrasound and counted in successive 5-second intervals
#'
#' @format A dataframe with 224 rows and 1 variable
#' \describe{
#'  \item{Counts}{Number of movements, note that the variance is .693 and the mean is .382}
#' }
#'
#' @source (Guttorp , 1995)
"fetalcount"

#' Fetal lamb movement wait times 
#'
#' Time between movements by a fetal lamb observed by ultrasound
#'
#' @format A dataframe with 58 rows and 1 variable
#' \describe{
#'  \item{WT}{Number of 5-second intervals between fetal movements}
#' }
#'
#' @source
"fetalwait"
