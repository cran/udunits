\name{utHasOrigin}
\alias{utHasOrigin}
\title{Determines if Unit has an Origin}
\description{
 Determines whether the unit has an origin (i.e., defines a scale).
}
\usage{
 utHasOrigin( unit )
}
\arguments{
  \item{unit}{A unit, in internal format as returned by utScan() or as a human-readable string.}
}
\value{TRUE if the unit has an origin, and FALSE otherwise.
}
\references{
\url{http://www.unidata.ucar.edu/packages/udunits/}
}
\details{
 This routine determines whether a unit has an origin.  For example,
 the unit "days" does not have an origin, but the unit "days since 1900-01-01"
 does.  In particular, a calendar must have an origin as well as being
 in temporal units.
}
\author{Library routines by Unidata; interface glue by David W. Pierce \email{dpierce@ucsd.edu}}
\seealso{ \code{\link[udunits]{utInit}}, \code{\link[udunits]{utScan}}, 
 \code{\link[udunits]{utCalendar}}, \code{\link[udunits]{utInvCalendar}},
 \code{\link[udunits]{utFormatDate}}, \code{\link[udunits]{utDayOfWeek}}, \code{\link[udunits]{utIsTime}},
 \code{\link[udunits]{utConvert}} }
\examples{
# Initialize the udunits library
utInit()

# Test to see if various proposed units would be suitable calendars
unitstring <- "days"
print(paste("Units",unitstring,"would work as a calendar:",
	utIsTime(unitstring)&&utHasOrigin(unitstring)))

unitstring <- "days since 1980-01-01"
print(paste("Units",unitstring,"would work as a calendar:",
	utIsTime(unitstring)&&utHasOrigin(unitstring)))
}
\keyword{utilities}
