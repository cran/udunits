# This provides an interface to UCAR's 'udunits' functions.
#
# David W. Pierce
# Climate Research Division
# Scripps Institution of Oceanography
# dpierce@ucsd.edu
# 18-June-2001
#
#==========================================================================
version.udunits <- function() {
	
	return("1.2")
}

#==========================================================================
# Initialize the udunits library.  This should be called exactly once.
#
utInit <- function() {
	
	rv       <- list()
	rv$error <- -1

	rv <- .C("R_utInit",
		error=as.integer(rv$error),
		PACKAGE="udunits")
	if( rv$error != 0 ) 
		stop("Error in utInit")
}

#==========================================================================
# Converts a formatted string unit specification into an internal 
# list that is used to manipulate units.  Returns that list.
#
utScan <- function( unitstring ) {

	if( ! is.character(unitstring) ) 
		stop("error in utScan: was not passed a character string")

	rv       <- list()
	rv$error <- -1
	rv$originfactor <- double(2)
	rv$hasoriginpowers <- integer(20)	# must be at least UT_MAXNUM_BASE_QUANTITIES, which is currently 10

	rv <- .C("R_utScan",
		as.character(unitstring),
		originfactor=as.double(rv$originfactor),
		hasoriginpowers=as.integer(rv$hasoriginpowers),
		error=as.integer(rv$error),
		PACKAGE="udunits")
	if( rv$error != 0 ) 
		stop("Error in utInit")

	class(rv) <- "udUnits"
	return(rv)
}

#==========================================================================
# Converts the amount (value) of the temporal unit (unit) into
# a UTC-referenced date and time.  The reference unit must be a
# time unit and have an origin; it is returned by a call to
# 'utScan'.  Returns a 'utDate' class object.
#
# Added in version 1.2: value can be a numeric vector of values.
# Return in this event depends on 'style' arg, which can be either
# 'list' or 'array'.  If it is 'list', then a list of utDate objects 
# is returned, and each element is itself a utDate object.  (i.e., 
# the n'th year is retval[[n]]$year, etc.).  If style is 'array',
# then retval$year is an array of N years, retval$month is an
# array of N months, etc.
#
utCalendar <- function( value, unit, style='list' )
{
	if( is.character(unit) )
		unit <- utScan( unit )

	if( class(unit) != "udUnits" )
		stop("utCalendar: I was passed a unit that is NOT of class 'udUnits'!")

	if( ! is.double(value) )
		value <- as.double(value)

	if( style == 'list' )
		istyle <- 1
	else if( style == 'array' )
		istyle <- 2
	else
		stop(paste("Error, style arg must be either 'list' or 'array'.  Passed value:",style))

	rv <- .Call("R_utCalendar_v1p2",
		value,
		as.double(unit$originfactor),
		as.integer(unit$hasoriginpowers),
		as.integer(istyle),
		PACKAGE="udunits")

	return(rv)
}

#==========================================================================
# Convers a calendar date specified by a 'utDate' class object
# into a value (amount) given in the units specified by 'unit' (which is a 
# unit-spec string or 'udUnits' class object).  Returns that value.
# Added in version 1.2: date can be a list of objects of class 'utDate',
# in which case this returns a vector of values rather than a scalar.
#
utInvCalendar <- function( date, unit )
{
	if( is.character(unit) )
		unit <- utScan( unit )

	if( class(unit) != "udUnits" ) 
		stop("utInvCalendar: I was passed a unit that is NOT of class 'udUnits'!")

	if( class(date) == "utDate" ) 
		nn <- 1
	else
		{
		if( class(date) == "list") {
			nn <- length(date)
			if( (nn < 1) || (class(date[[1]]) != "utDate"))
				stop("utInvCalendar: I was passed a date that is NOT of class 'utDate' (or, not a list of objects of class 'utDate')!")
			}
		}

	rv <- list()
	rv$year   <- array(0,nn)
	rv$month  <- array(0,nn)
	rv$day    <- array(0,nn)
	rv$hour   <- array(0,nn)
	rv$minute <- array(0,nn)
	rv$secod  <- array(0.0,nn)
	rv$error  <- -1
	rv$value  <- double(nn)
	
	if( nn == 1 ) {
		rv$year   <- date$year
		rv$month  <- date$month
		rv$day    <- date$day
		rv$hour   <- date$hour
		rv$minute <- date$minute
		rv$second <- date$second
		}
	else
		{
		for( i in 1:nn ) {
			rv$year[i]   <- date[[i]]$year
			rv$month[i]  <- date[[i]]$month
			rv$day[i]    <- date[[i]]$day
			rv$hour[i]   <- date[[i]]$hour
			rv$minute[i] <- date[[i]]$minute
			rv$second[i] <- date[[i]]$second
			}
		}
	
	rv <- .C("R_utInvCalendar",
		as.integer(nn),
		as.integer(rv$year),
		as.integer(rv$month),
		as.integer(rv$day),
		as.integer(rv$hour),
		as.integer(rv$minute),
		as.double (rv$second),
		as.double (unit$originfactor),
		as.integer(unit$hasoriginpowers),
		value=as.double(rv$value),
		error=as.integer(rv$error),
		PACKAGE="udunits")
	if( rv$error != 0 ) 
		stop("Error in utInvCalendar")

	return(rv$value)
}

#==========================================================================
#int dayofweek(int day, int month, int year)
#/* Mon. = 0, Sun. = 6 */
#{
#if (month < 3) {
#month += 12;
#year--;
#}
#return ((13*month+3)/5 + day + year + year/4 - year/100 +
#year/400) % 7;
#}
#
# 
# Input: year (4-digit, e.g., 1998), month (1=Jan, 12=Dec), day (1-31).
# Output: day of week, 1=Monday, 7=Sunday
#
utDayOfWeek <- function( year, month, day, aschar=FALSE ) {

	days <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")

	year  <- as.integer(year)
	month <- as.integer(month)
	day   <- as.integer(day)

	if( year < 1753 ) 
		stop("Works for dates after 1752 (Gregorian calendar dates) only")

	if( month < 3 ) {
		month <- month + 12
		year <- year - 1
		}

	dow <- (as.integer((13*month+3)/5) + day + year + as.integer(year/4) - as.integer(year/100) + as.integer(year/400)) %% 7

	dow <- dow + 1

	if( aschar ) 
		return( days[dow] )
	else
		return( dow )
}

#==========================================================================
# Possible formats, and example for each:
#
#	"full" 		2003/09/28 10:14	(default)
#	"slashes"	2003/09/28 
#	"unix"		Sun Sep 28 10:14 2003
#	"sci"		28 Sep 2003
#	"underscores"	2003_Sep_28
#
print.utDate <- function( x, quiet=FALSE, format="full", ... ) {

	date <- x

	if( class(date) != "utDate" )
		stop("print.utDate: passed a date that is NOT a utDate object!")

	months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", 
	        "Sep", "Oct", "Nov", "Dec")

	if( format == "full" ) 
		res <- paste(formatC(date$year, width=4,flag='0'), '/',
			     formatC(date$month,width=2,flag='0'), '/',
			     formatC(date$day,width=2,flag='0'), ' ',
			     formatC(date$hour,width=2,flag='0'), ':',
			     formatC(date$minute,width=2,flag='0'), sep='')

	else if( format == "unix" )
		res <- paste(utDayOfWeek(date$year,date$month,date$day,aschar=TRUE)," ",
			months[date$month]," ",date$day," ",
			formatC(date$hour,width=2,flag='0'), ':',
			formatC(date$minute,width=2,flag='0'), " ",
			formatC(date$year, width=4,flag='0'), sep='')

	else if( format == "slashes" )
		res <- paste(formatC(date$year, width=4,flag='0'), '/',
			     formatC(date$month,width=2,flag='0'), '/',
			     formatC(date$day,width=2,flag='0'), sep='' )

	else if( format == "sci" )
		res <- paste( formatC(date$day,width=2,flag='0'), ' ',
			      months[date$month]," ",
			      formatC(date$year, width=4,flag='0'), sep='' )

	else if( format == "underscore" )
		res <- paste( formatC(date$year, width=4,flag='0'), "_",
				 months[date$month], "_", 
				 formatC(date$day,width=2,flag='0'), sep="" )

	else
		stop(paste("print.utDate: Unrecognized format >",format,
			"<.  Recognized values: full, slashes, unix, sci, underscore.",sep=""))

	if( quiet )
		return(res)
	else
		print(res,...)
}

#==========================================================================
# Returns TRUE if the unit is temporal, and FALSE otherwise.  NOTE
# that a calendar must be temporal *and* have an origin.
#
utIsTime <- function( unit ) {

	if( is.character(unit) )
		unit <- utScan( unit )

	if( class(unit) != "udUnits" )
		stop("utIsTime: I was passed a unit that is NOT of class 'udUnits'!")

	rv <- list()
	rv$retval <- 0

	rv <- .C("R_utIsTime",
		as.double(unit$originfactor),
		as.integer(unit$hasoriginpowers),
		retval=as.integer(rv$retval),
		PACKAGE="udunits")

	if( rv$retval == 0 )
		return(FALSE)
	else
		return(TRUE)
}

#==========================================================================
# Returns TRUE if the unit is temporal, and FALSE otherwise.  NOTE
# that a calendar must be temporal *and* have an origin.
#
utHasOrigin <- function( unit ) {

	if( is.character(unit) )
		unit <- utScan( unit )

	if( class(unit) != "udUnits" )
		stop("utIsTime: I was passed a unit that is NOT of class 'udUnits'!")

	rv <- list()
	rv$retval <- 0

	rv <- .C("R_utHasOrigin",
		as.double(unit$originfactor),
		as.integer(unit$hasoriginpowers),
		retval=as.integer(rv$retval),
		PACKAGE="udunits")

	if( rv$retval == 0 )
		return(FALSE)
	else
		return(TRUE)
}

#==========================================================================
# Returns the slope and intercept necessary to convert from unit "unit.from"
# to unit "unit.to".  Optionally, if given a "val" (value) in "unit.from"
# units, will return the value in the "unit.to" units.
#
utConvert <- function( unit.from, unit.to, val=NA ) {

	if( is.character(unit.from) )
		unit.from <- utScan( unit.from )

	if( class(unit.from) != "udUnits" )
		stop("utConvert: I was passed a unit to convert from that is NOT of class 'udUnits'!")

	if( is.character(unit.to) )
		unit.to <- utScan( unit.to )

	if( class(unit.to) != "udUnits" )
		stop("utConvert: I was passed a unit to convert to that is NOT of class 'udUnits'!")

	rv <- list()
	rv$error     <- -1
	rv$slope     <- double(1)
	rv$intercept <- double(1)
	
	rv <- .C("R_utConvert",
		as.double(unit.from$originfactor),
		as.integer(unit.from$hasoriginpowers),
		as.double(unit.to$originfactor),
		as.integer(unit.to$hasoriginpowers),
		slope=as.double(rv$slope),
		intercept=as.double(rv$intercept),
		error=as.integer(rv$error),
		PACKAGE="udunits")
	if( rv$error != 0 ) 
		stop("Error in utConvert")

	if( ! is.na(val) ) {
		convval <- rv$slope*val + rv$intercept
		return( convval )
		}

	return(rv)
}

#==========================================================================
# Test code in R
#

#dyn.load("/path/to/udunits.so")

#utInit()
#u <- utScan("days since 1900-01-01")
#print(u)

#seconds <- 33037.53
#date <- utCalendar( seconds, u )
#print("date corresponding to seconds=32570:")
#print(date)

#date <- list()
#class(date) <- "utDate"
#date$year <- 1990
#date$month <- 6
#date$day <- 15
#date$hour <- 12
#date$minute <- 30
#date$second <- 10.0

#val <- utInvCalendar( date, u )
#print("value corresponding to 6-12-1990:")
#print(val)

#utInit()
#calval <- 34851.5
#units  <- "days since 1900-01-01"
#d <- uduFormatDate( calval, units )
#print(d)

#print(date)
#print(date,format="unix")
#print(date,format="slashes")
#print(date,format="sci")

# Test utConvert -- convert 3 meters to inches.  Should be 118 or so
#unitstrfrom <- "m"
#unitstrto   <- "in"
#val <- c(3.0,4.)
#utInit()
#unitfrom <- utScan(unitstrfrom)
#unitto <- utScan(unitstrto)
#res <- utConvert( unitfrom, unitto ) 
#print(paste("slope=",res$slope))
#print(paste("intercept=",res$intercept))
#res <- utConvert( unitfrom, unitto, val=val ) 
#print(paste(val,unitstrfrom,"equals",res,unitstrto))

# convert degC (celsius) to degF (farenheit)
#unitstrfrom <- "degC"
#unitstrto <- "degF"
#val <- c(0,10,20,30,40)
#unitfrom <- utScan(unitstrfrom)
#unitto <- utScan(unitstrto)
#res <- utConvert( unitfrom, unitto )
#print(paste("slope=",res$slope))
#print(paste("intercept=",res$intercept))
#res <- utConvert( unitfrom, unitto, val=val )
#print(paste(val,unitstrfrom,"equals",res,unitstrto))

# Test HasOrigin and IsTime
#utInit()
#testf <- function( ustr ) {
#	u <- utScan(ustr)
#	istime <- utIsTime(u)
#	hasorigin <- utHasOrigin(u)
#	print(paste("unit",ustr,"is time:",istime,"has origin:",hasorigin))
#}
#testf("seconds")
#testf("meters")
#testf("days since 1900-01-01")
