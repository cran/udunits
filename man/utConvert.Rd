\name{utConvert}
\alias{utConvert}
\title{Convert Values Between Units}
\description{
 Converts values between units, or returns the coefficients necessary for doing so.
}
\usage{
 utConvert( unit.from, unit.to, val=NA )
}
\arguments{
  \item{unit.from}{The units to convert from, either in the internal format returned by utScan(), or 
  in a human-readable string that this routine passes to utScan().}
  \item{unit.to}{The units to convert to, either internal or human-readable format.}
  \item{val}{If supplied, the given values are converted and returned.  Otherwise, the
  coefficients needed to perform the conversion are returned.}
}
\value{
 If no 'val' argument is provided, this routine returns a list with elements 
 'slope' and 'intercept'.  If values are provided through argument 'val', then
 they are assumed to be in the 'unit.from' units, converted to the 'units.to'
 units, and returned.
}
\references{
\url{http://www.unidata.ucar.edu/packages/udunits/}
}
\details{
 This routine can either convert values from one set of units to another (compatible!)
 set of units, or return the coefficients necessary for the user to do so.
 If given values in passed argument 'val', these are assumed to be in the old
 units and converted to the new units.
 If not given an argument in 'val', then the slope and intercept needed to
 convert from the old to the new units are returned.  The conversion can
 then be accomplished as newval <- slope*oldval + intercept.
}
\author{Library routines by Unidata; interface glue by David W. Pierce \email{dpierce@ucsd.edu}}
\seealso{ \code{\link[udunits]{utInit}}, \code{\link[udunits]{utScan}}, 
 \code{\link[udunits]{utCalendar}}, \code{\link[udunits]{utInvCalendar}},
 \code{\link[udunits]{utFormatDate}}, \code{\link[udunits]{utDayOfWeek}}, \code{\link[udunits]{utIsTime}},
 \code{\link[udunits]{utHasOrigin}} }
\examples{
# Initialize the udunits library
utInit()

# As an example, convert meters to inches
unitstrfrom <- "m"
unitstrto   <- "in"

unitfrom    <- utScan(unitstrfrom)
unitto      <- utScan(unitstrto)

res         <- utConvert( unitfrom, unitto )
print(paste("slope=",res$slope," intercept=",res$intercept))

val <- c(3.0,4.)
newval <- res$slope*val + res$intercept
print(paste(val,unitstrfrom,"is equivalent to",newval,unitstrto))

# Another example: convert degC (celsius) to degF (farenheit)
val    <- c(0,10,20,30,40)	# these are in degC
newval <- utConvert( "degC", "degF", val )	# newval is now in degF
print(paste(val,"degC is equivalant to",newval,"degF"))
}
\keyword{utilities}
