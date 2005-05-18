\name{utInvCalendar}
\alias{utInvCalendar}
\title{Convert a Calendar Date into a Temporal Amount}
\description{
 Converts a given calendar date into an amount of the specified temporal units.
}
\usage{
 utInvCalendar( date, unit )
}
\arguments{
  \item{date}{An object of class 'utDate', which has the following fields:
  year, month, day, hour, minute, second.  This can also be a list of
  objects of class 'utDate'.}
  \item{unit}{A temporal unit that has an origin.}
}
\value{The amount of the temporal units that the date corresponds to.
  If the input 'date' is a list of objects of class 'utDate', this is an
  double precision array, otherwise it's a double precision scalar.}
}
\references{
\url{http://www.unidata.ucar.edu/packages/udunits/}
}
\details{
 This routine converts a date, given by the fields in a "utDate"
 class object passed in as the first argument (and which your code needs explicitly construct), 
 into the amount of a temporal unit specified by the unit "unit".
 The fields in "utDate" are: year [integer], month [integer], 
 day [integer], hour [integer], minute [integer], and second [double precision].
 Note that every field in class "utDate" except the "second" field
 is an integer -- don't try to set these to floating point values
 or it won't work (i.e., setting date\$year to "1990.5" will NOT indicate a date
 in the middle of 1990!).

 Example: if "unit" is the internally-formatted version of the
 units string "days since 1900-01-01" (i.e., returned by the
 call to utScan("days since 1900-01-01"), and this routine is
 passed the date 1900-01-03, then it will return the value
 2, since the passed date is 2 days since 1900-01-01.
}
\author{Library routines by Unidata; interface glue by David W. Pierce \email{dpierce@ucsd.edu}}
\seealso{ \code{\link[udunits]{utInit}}, \code{\link[udunits]{utScan}}, 
 \code{\link[udunits]{utCalendar}}, 
 \code{\link[udunits]{utFormatDate}}, \code{\link[udunits]{utDayOfWeek}}, \code{\link[udunits]{utIsTime}},
 \code{\link[udunits]{utHasOrigin}}, \code{\link[udunits]{utConvert}} }
\examples{
# Initialize the udunits library
utInit()

# Set our calendar units
unitstring <- "days since 1900-01-01"
u <- utScan(unitstring)

# Set our date.  NOTE you must do this 'by hand'
date <- list()
class(date) <- "utDate"
date$year   <-  1990
date$month  <- 6
date$day    <- 15
date$hour   <- 12
date$minute <- 30
date$second <- 10.0

val <- utInvCalendar( date, u )

# Note use of 'print' with 'quiet=TRUE' here to format the date.  This
# is necessary because it appears *inside* a 'paste' call.
print(paste("the date",print(date,quiet=TRUE),"is",val,unitstring))

# You can also give the units string directly to the utInvCalendar
# function, but this is slower if you have multiple conversions to do.
# However it does work:
val2 <- utInvCalendar( date, unitstring )
print(paste("should be same as previous value:",val2))
}
\keyword{utilities}
