# Demo showing use of a few utilities included in the package.

# Calculate the day of the week for a given date...
yr  <- 2000
mo  <- 1
day <- 1
dow <- utDayOfWeek( yr, mo, day, aschar=T )
print(paste(yr,"/",mo,"/",day," fell on a ",dow,sep=""))

# Illustrate testing whether units are timelike, and have an origin...
utInit()
ududemotestf <- function( unitstring ) {
	u <- utScan(unitstring)
	istime <- utIsTime(u)
	hasorigin <- utHasOrigin(u)
	print(paste('unit <',unitstring,'> is temporal:',istime,'. Has origin:',hasorigin,sep=''))
}
ududemotestf("seconds")
ududemotestf("meters")
ududemotestf("days since 1870-01-01")
ududemotestf("degK after 273.15")
