# Use the 'inverse' calendar function.  I.e., convert a known
# date into some amount of the given units.  Note that dates must
# be given as the class "utDate".
#
date <- list()
class(date) <- "utDate"
date$year   <- 1990
date$month  <- 6
date$day    <- 15
date$hour   <- 12
date$minute <- 30
date$second <- 10.0

unitstring <- "days since 1900-01-01"

utInit()	# Initialize package
u   <- utScan(unitstring)
val <- utInvCalendar( date, u )
print(paste("amount of",unitstring,"corresponding to date",print(date,quiet=T),":",val))

# Above way is the most efficient for more than one conversion because it
# calls utScan() only once.  However, it is also possible to directly pass
# the units string to utInvCalendar:

val <- utInvCalendar( date, unitstring )
print(paste("Should be same amount as above:",val))

