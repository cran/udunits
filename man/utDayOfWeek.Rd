\name{utDayOfWeek}
\alias{utDayOfWeek}
\title{Convert Date to Day of Week}
\description{
 Converts a calendar date (in the Gregorian calendar) to a day of the week.
}
\usage{
 utDayOfWeek( year, month, day, aschar=FALSE )
}
\arguments{
  \item{year}{Integer 4-digit year to convert (Gregorian calendar only, i.e., year >1752)}
  \item{month}{Integer month to convert, 1=Jan, 12=Dec}
  \item{day}{Integer day of month to convert, 1-31}
  \item{aschar}{If TRUE, returns the 3-letter abbreviation of the day's name instead of integer
  (1=Mon, 7=Sun)}
}
\value{
 If aschar=FALSE (the default), returns a integer between 1 and 7, with
 1=Monday, 7=Sunday.  If aschar=TRUE, returns the 3-letter abbreviation of
 the day's name.
}
\details{
 This function computes the day of week for a given date.  Only Gregorian
 calendars are supported, which 
 means only dates after 1752 or so (depending on what country you live in;
 different countries switched to the Gregorian calendar at different times).
}
\author{Implemented by David W. Pierce \email{dpierce@ucsd.edu}}
\seealso{ \code{\link[udunits]{utInit}}, \code{\link[udunits]{utScan}}, 
 \code{\link[udunits]{utCalendar}}, \code{\link[udunits]{utInvCalendar}},
 \code{\link[udunits]{utFormatDate}}, \code{\link[udunits]{utIsTime}},
 \code{\link[udunits]{utHasOrigin}}, \code{\link[udunits]{utConvert}} }
\examples{
# What day did the beginning of year 2000 fall on?
utDayOfWeek(2000,1,1,aschar=TRUE)
}
\note{
 Don't think that this routine works for dates during the 1700's when
 countries were switching between the Julian and Gregorian calendars; 
 it doesn't.  
}
\keyword{utilities}
