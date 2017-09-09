#' Household data frame from the 2009 NTHS data.
#'
#' Household data frame from the 2009 NTHS data..
#'
#' @format The main data frame \code{Hh_df} has 130509 rows and 78 variables:
#' \describe{
#'   \item{AADVMT}{Annual Average Daily VMT, cacluated from BESTMILE}
#'   \item{AADVMT.int}{Annual Average Daily VMT, rounded and converted to integer from AADVMT}
#'   \item{Age0to14}{}
#'   \item{Age65Plus}{}
#'   \item{ANNMILES}{}
#'   \item{AutoAvgTripDist}{}
#'   \item{BikeAvgTripDist}{}
#'   \item{TransitAvgTripDist}{}
#'   \item{WalkAvgTripDist}{}
#'   \item{BESTMILE}{}
#'   \item{BESTMILEcap}{}
#'   \item{CENSUS_D}{}
#'   \item{CENSUS_R}{}
#'   \item{Drivers}{}
#'   \item{DrvAgePop}{}
#'   \item{DVMT}{}
#'   \item{DVMTcap}{}
#'   \item{DVMT.int}{}
#'   \item{FLAG100}{}
#'   \item{HBHTNRNT}{}
#'   \item{HBHUR}{}
#'   \item{HHFAMINC}{}
#'   \item{HHFAMINC10k}{}
#'   \item{HHFAMINC20k}{}
#'   \item{HHFAMINCVAL}{}
#'   \item{HhSize}{}
#'   \item{HOUSEID}{}
#'   \item{HTHTNRNT}{}
#'   \item{HTPPOPDN}{}
#'   \item{LifeCycle}{}
#'   \item{lnBESTMILE}{}
#'   \item{lnPMTap}{log of PMT per capita}
#'   \item{LogIncome}{}
#'   \item{MSACAT}{}
#'   \item{MSASIZE}{}
#'   \item{Trips}{Total number of trips}
#'   \item{AutoTrips}{Number of Driving Trips}
#'   \item{BikeTrips}{Number of Biking Trips}
#'   \item{TransitTrips}{Number of Transit Trips}
#'   \item{WalkTrips}{Number of Walking Trips}
#'   \item{NUMADLT}{}
#'   \item{powBESTMILE}{}
#'   \item{powDVMT}{}
#'   \item{RAIL}{}
#'   \item{TDAYDATE}{}
#'   \item{PMT}{Total person miles travelled}
#'   \item{PMTCap}{Total person miles travelled per capita}
#'   \item{AutoPMT}{}
#'   \item{BikePMT}{}
#'   \item{TransitPMT}{}
#'   \item{WalkPMT}{}
#'   \item{AutoPMTPct}{}
#'   \item{BikePMTPct}{}
#'   \item{TransitPMTPct}{}
#'   \item{WalkPMTPct}{}
#'   \item{TRAVDAY}{}
#'   \item{PTT}{}
#'   \item{AutoPTT}{}
#'   \item{BikePTT}{}
#'   \item{TransitPTT}{}
#'   \item{WalkPTT}{}
#'   \item{AutoPTTPct}{}
#'   \item{BikePTTPct}{}
#'   \item{TransitPTTPct}{}
#'   \item{WalkPTTPct}{}
#'   \item{URBAN}{}
#'   \item{URBANSIZE}{}
#'   \item{URBRUR}{}
#'   \item{Vehicles}{}
#'   \item{VehPerDriver}{}
#'   \item{VehPerDrvAgePop}{}
#'   \item{WEEKDAY}{}
#'   \item{WEEKEND}{}
#'   \item{Workers}{}
#'   \item{WTHHFIN}{}
#'   \item{ZeroAADVMT}{}
#'   \item{ZeroDVMT}{}
#'   \item{ZeroVeh}{}
#'   }
#'
#'
#' @source \url{http://nhts.ornl.gov/download.shtml#2009}
#' @import tidyverse
#' @examples
#' str(Hh_df)
#' head(Hh_df)
#' summary(Hh_df)
#'
"Hh_df"
