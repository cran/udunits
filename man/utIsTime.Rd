\name{utIsTime}
\alias{utIsTime}
\title{Determines if Unit is Temporal}
\description{
 Determines whether the unit is temporal (i.e., has units of time).
}
\usage{
 utIsTime( unit )
}
\arguments{
  \item{unit}{A unit, in internal format as returned by utScan() or as a human-readable string.}
}
\value{TRUE if the unit has units of time, and FALSE otherwise.
}
\references{
\url{http://www.unidata.ucar.edu/packages/udunits/}
}
\details{
 This routine determines whether a unit has units of only time.
 If it does, the routine returns TRUE, and returns FALSE otherwise.
 The passed argument can be either an internally-formatted units
 structure created by utScan(), or a human-readable units string
 (in which case this routine passes the string to utScan() itself).
}
\author{Library routines by Unidata; interface glue by David W. Pierce \email{dpierce@ucsd.edu}}
\seealso{ \code{\link[udunits]{utInit}}, \code{\link[udunits]{utScan}}, 
 \code{\link[udunits]{utCalendar}}, \code{\link[udunits]{utInvCalendar}},
 \code{\link[udunits]{utFormatDate}}, \code{\link[udunits]{utDayOfWeek}}, 
 \code{\link[udunits]{utHasOrigin}}, \code{\link[udunits]{utConvert}} }
\examples{
# Initialize the udunits library
utInit()

# Set our calendar units
unitstring <- "days since 1900-01-01"

u <- utScan(unitstring)

print(paste("Unit string",unitstring,"is temporal:",utIsTime(u)))

# Here is another way of doing it:
utIsTime("meters")
}
\note{
 To be a calendar, it is necessary but not sufficient for a unit string to
 be temporal.  It must also have an origin.  See utHasOrigin().
}
\keyword{utilities}
