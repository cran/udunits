\name{print.utDate}
\alias{print.utDate}
\title{Print a Formatted Calendar Date}
\description{
 Prints a formatted version of a 'utDate' object, which holds a calendar date.
}
\usage{
 print.utDate( x, quiet=FALSE, format="full", ... )
}
\arguments{
  \item{x}{A utDate class object, with fields year, month, day, hour, minute, second.}
  \item{quiet}{if TRUE, then returns the formatted string without printing it}
  \item{format}{Controls the printed format. Can be "full" (the default), "slashes", 
  "unix", "sci", or "underscore".}
  \item{...}{Other arguments are passed back to 'print' when the formatted date is printed.}
}
\value{
 Ordinarly, nothing.  However, if quiet=TRUE, returns the formatted character
 string version of the date instead of printing it.
}
\details{
 Dates in the udunits package are described by an object with class 'utDate'.
 A 'utDate' is a list with fields year, month, day, hour, minute, and second.
 Year is a 4-digit integer, month is a 2-digit integer (1=Jan, 12=Dec), day is
 a two digit integer (1-31), hour is a 2-digit integer (0-23), minute is a
 two digit integer (0-59), and second is a floating point in the range
 [0.,60.). The routine utCalendar() returns an object of class utDate.  Typically,
 if a user's code wants to create a utDate, it must do so by hand, as illustrated
 in the example below.

 This function formats a utDate object into a human-readable character string, and
 usually prints it (alternatively, if quiet=TRUE, it returns the string without printing
 it).  Various formats are supported.  For example, given the date June 15th, 1990,
 at 12:43 PM, the various formats give the following: "full" gives "1990/06/15 12:43";
 "unix" gives "Fri Jun 15 12:43 1990"; "sci" gives "15 Jun 1990"; "slashes"
 gives "1990/06/15"; "underscore" gives "1990\_Jun\_15".
}
\author{Library code by Unidata; interface glue implemented by David W. Pierce \email{dpierce@ucsd.edu}}
\seealso{ \code{\link[udunits]{utInit}}, \code{\link[udunits]{utScan}}, 
 \code{\link[udunits]{utCalendar}}, \code{\link[udunits]{utInvCalendar}},
 \code{\link[udunits]{utFormatDate}}, \code{\link[udunits]{utDayOfWeek}}, \code{\link[udunits]{utIsTime}},
 \code{\link[udunits]{utHasOrigin}}, \code{\link[udunits]{utConvert}} }
\examples{
# Make a 'utDate' object by hand.  This class of object is also
# returned by function 'utCalendar()'
date <- list() 
class(date) <- "utDate"
date$year   <- 1990
date$month  <- 6
date$day    <- 15
date$hour   <- 12
date$minute <- 30 
date$second <- 10.0

print(date)	# default format is "full"

# Turn the date into a character string without printing it
s <- print(date,format="unix",quiet=TRUE)

print(paste("here is that date one more time:",s))

# Note that if you want a formatted date INSIDE a paste call, you 
# have to call print with quiet=TRUE, like this:
print(paste("Yet another way of formatting the date:",print(date,format="sci",quiet=TRUE)))
}
\keyword{utilities}
