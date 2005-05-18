\name{utCalendar}
\alias{utCalendar}
\title{Convert Temporal Amounts to Calendar Date}
\description{
 Converts a given amount of a temporal unit into a UTC-reference date and time.
}
\usage{
 utCalendar( value, unit, style='list' )
}
\arguments{
  \item{value}{An amount (quantity) of the given temporal unit, or a vector thereof.}
  \item{unit}{A temporal unit that has an origin.}
  \item{style}{Specifies the style of returned value when a vector of input values is given. 
  Can be either 'list' or 'array'.  See below for details.}
}
\value{If the input 'value' is a scalar, returns an object of class 'utDate'.
If the input is a vector of N values, the returned value depends on the
'style' argument.  
For the default style='list', this returns a list of N objects of class
'utDate'.  
So in this case, the year of the n'th input value is given by retval[[n]]$year.
If style=='array', this instead returns a single object with fields
year, month, day, hour, minute, and second, each of which is an array
with N elements.  So in this case the year of the n'th input
value is given by retval$year[[n]].
In either event, class 'utDate' has the following fields:
 year, month, day, hour, minute, second.
}
\references{
\url{http://www.unidata.ucar.edu/packages/udunits/}
}
\details{
 This routine converts an amount "value" (or vector of values) of the temporal unit given
 by the unit "unit" into a date and time.
 For example, if "unit" is the internally-formatted version of the
 units string "days since 1900-01-01" (i.e., returned by the
 call to utScan("days since 1900-01-01"), then a value of "2"
 means "two days since 1900-01-01", and hence has the date 1900-01-03.

 The style of the returned value depends on how many input values are given,
 and on the 'style' argument.

 If only one input value is given, then the returned value is 
 an an object of class 'utDate',
 which has the following fields: year, month, day, hour, minute, seconds.

 If a vector of N input values is given, and style=='list' (the default), 
 the result is a list of N objects, each of which is of class 'utDate'.

 If a vector of N input values is given, and style=='array',
 the result is a single object with fields year, month, day, hour, minute, 
 and seconds, each of which is an array of length N.
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
