\name{utScan}
\alias{utScan}
\title{Convert Units String to Internal Format}
\description{
 Converts a human-readable units string into the internal udunits library format.
}
\usage{
 utScan( unitstring )
}
\arguments{
  \item{unitstring}{A human-readable string describing physical units.}
}
\value{An internally-formatted version of the passed units string,
 suitable for use with the other udunits functions (utConvert,
 utCalendar, etc.)}
\references{
\url{http://www.unidata.ucar.edu/packages/udunits/}
}
\details{
 The function utScan converts a human-readable
 string into the required internal format.  Some possible examples of
 acceptable strings include "10 kilogram.meters/seconds2",
 "10 kg-m/sec2", "10 kg m/s\^{ }2", "(PI radian)2", "degF", "degC", "100rpm",
 "geopotential meters", and "33 feet water".  A complete list of
 known units, and ways the unit string can be specified, can be
 found at the udunits web site referenced below.

 The fastest way to do multiple conversions with the same units string
 is to first use this function (utScan) to convert the units string into its
 internal format, then use that internally-formatted units object in
 the library calls (utConvert, utCalendar, etc).  
 However, it is also possible to pass the units
 string directly to the library calls.  Just be aware that doing so
 can be significantly slower, because a human-readable units string must be parsed
 each time it is given to a library function.  Much faster to just convert
 it once, then pass the internally-formatted units object.

 Remember that utInit() must be called sometime prior to this function
 being called.
}
\author{Library routines by Unidata; interface glue by David W. Pierce \email{dpierce@ucsd.edu}}
\seealso{ \code{\link[udunits]{utInit}}, 
 \code{\link[udunits]{utCalendar}}, \code{\link[udunits]{utInvCalendar}},
 \code{\link[udunits]{utFormatDate}}, \code{\link[udunits]{utDayOfWeek}}, \code{\link[udunits]{utIsTime}},
 \code{\link[udunits]{utHasOrigin}}, \code{\link[udunits]{utConvert}} }
\examples{
# Initialize the udunits library
utInit()

# We will use meters as our units
unitstring <- "m"
u <- utScan(unitstring)
}
\note{The unit string "C" refers to coulombs, not degrees Celsius (which 
 is given by the unit string "degC").}
\keyword{utilities}
