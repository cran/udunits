\name{utCalendar}
\alias{utCalendar}
\title{Convert Temporal Amounts to Calendar Date}
\description{
 Converts a given amount of a temporal unit into a UTC-reference date and time.
}
\usage{
 utCalendar( value, unit )
}
\arguments{
  \item{value}{An amount (quantity) of the given temporal unit.}
  \item{unit}{A temporal unit that has an origin.}
}
\value{An object of class 'utDate', with the following fields:
 year, month, day, hour, minute, second.
}
\references{
\url{http://www.unidata.ucar.edu/packages/udunits/}
}
\details{
 This routine converts an amount "value" of the temporal unit given
 by the unit "unit" into a date and time.
 For example, if "unit" is the internally-formatted version of the
 units string "days since 1900-01-01" (i.e., returned by the
 call to utScan("days since 1900-01-01"), then a value of "2"
 means "two days since 1900-01-01", and hence has the date 1900-01-03.
}
\author{Library routines by Unidata; interface glue by David W. Pierce \email{dpierce@ucsd.edu}}
\seealso{ \code{\link[udunits]{utInit}}, \code{\link[udunits]{utScan}}, 
 \code{\link[udunits]{utInvCalendar}},
 \code{\link[udunits]{utFormatDate}}, \code{\link[udunits]{utDayOfWeek}}, \code{\link[udunits]{utIsTime}},
 \code{\link[udunits]{utHasOrigin}}, \code{\link[udunits]{utConvert}} }
\examples{
# Initialize the udunits library
utInit()

# Set our calendar units
unitstring <- "days since 1900-01-01"
u <- utScan(unitstring)

# Set our value
val <- 31038
date <- utCalendar( val, u )
print(date)

# Print it out "by hand"
print(paste("The date corresponding to",val,unitstring,"is:"))
print(paste("year=",date$year))
print(paste("month=",date$month))
print(paste("day=",date$day))
print(paste("hour=",date$hour))
print(paste("minute=",date$minute))
print(paste("second=",date$second))

# Some alternate ways of printing the date
print(date,format="unix")
print(date,format="slashes")
print(date,format="sci")

# Can also call utCalendar directly with the units string, but
# this is slower for multiple conversions than calling utScan()
# only once, as illustrated above.  But it works:
date2 <- utCalendar( val, unitstring )

# Note that to format a date INSIDE a paste call, you can 
# use the print function with quiet=TRUE, like this:
print(paste("This should be the same as the previous date:",print(date,format="sci",quiet=TRUE)))

}
\keyword{utilities}
