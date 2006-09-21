\name{udunits}
\alias{udunits}
\title{A Library for Handling and Converting Units}
\description{
 This is the udunits package, version 1.3 (31 January 2006). 

 Unidata's udunits library is a suite of routines for manipulating physical units.
 Using it, you can convert values that are given in one unit (for example,
 meters) to another compatible unit (for example, inches).
 Another popular use of the udunits library, especially in the earth sciences,
 is to handle dates that are given in units such as "days since 1900-01-01".
 When confronted with the value "31080" given in these units, how do you know 
 what date that corresponds to?  Routines are provided for turning such value/unit
 pairs into more readable calendar dates.

 This package supplies the "glue" routines so that the udunits library calls
 can be made from R.  The udunits library must already be installed on your
 machine for these calls to work.  The udunits library source can be downloaded
 from the URL referenced below.

 Any R program that uses the udunits library must call \code{utInit()} before
 any other udunits routines are called.  This reads in the units database and
 initializes the library.
 Then, typically, the human readable units strings (such as "meters" or "kg" -- see
 \code{utScan()} for more examples) are turned into the internally-formatted
 units structure using \code{utScan()}.
 After this, values given in different units can be converted using 
 \code{utConvert()}.
 Or, values given in a unit such as "days since 1900-01-01" can be converted
 to a date using \code{utCalendar()}.
 There is also a routine to convert from a calendar date to a value in
 units such as "days since 1900-01-01"; this routine is \code{utInvCalendar}.

 These routines are based on the "C" version of the udunits library, and so follow
 that naming convention.  The manual for the "C" version of the library is online
 at the URL referenced below.

 Note that it is not actually necessary to use \code{utScan()} to convert a
 human-readable string to the internally-formatted string before calling
 the udunits library functions.  Most of them will accept the human-readable
 string to begin with, and call \code{utScan()} for you.  However, if you are
 doing multipile conversions, it can be much faster to convert the string once
 by an explicit call to \code{utScan()} rather than having the library routines
 call it over and over for you.
}
\references{
 http://www.unidata.ucar.edu/packages/udunits/
}
\author{Library routines by Unidata; interface glue by David W. Pierce \email{dpierce@ucsd.edu}}
\seealso{ \code{\link[udunits]{utInit}}, \code{\link[udunits]{utScan}}, 
 \code{\link[udunits]{utCalendar}}, \code{\link[udunits]{utInvCalendar}},
 \code{\link[udunits]{utFormatDate}}, \code{\link[udunits]{utDayOfWeek}}, \code{\link[udunits]{utIsTime}},
 \code{\link[udunits]{utHasOrigin}}, \code{\link[udunits]{utConvert}} }
\keyword{utilities}
