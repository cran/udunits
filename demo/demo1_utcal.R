# Scan a units string defining a calendar, then convert a given 
# value to a date.

unitstring <- "days since 1900-01-01"
ndays      <- 33037.53

utInit()	# Initialize package first!
u <- utScan(unitstring)

date  <- utCalendar( ndays, u )	
print(paste("date corresponding to",ndays, unitstring))
print(date)			# default print format
print(date,format="unix")	# some optional formats
print(date,format="sci")	# some optional formats
print(date,format="slashes")	# some optional formats
print(date,format="underscore") # some optional formats

# For multiple conversions, it is more efficient to call "utScan()" once to 
# convert a human-readable units string into the internal units format, as is 
# done above. However, it is also possible to simply pass the units string like this:

date2 <- utCalendar( ndays, unitstring )	# less efficient but works

# Note how we use print (which calls print.utDate) to explicitly convert the
# date into a formatted string.  Have to do this because it is *inside* the
# paste, and paste doesn't seem to call different methods for different classes:

print(paste("this should be the same date:",print(date2,quiet=TRUE)))

