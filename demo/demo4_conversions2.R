# Conversions can be done 2 ways.  You can call the convert
# routine to get the slope and intercept necessary for the conversion,
# then do the conversion yourself.  Or, you can have the convert routine
# do the conversion for you.  This example demonstrates the second of
# those two ways.  See demo3_conversions1.R for the other method.

unitstrfrom <- "degC"	# Convert degrees Celsius....
unitstrto   <- "degF"	# ...to degrees F

valstoconvert <- c(0.,10.,20.,30.,40.)

utInit()		# Initlialize the package
unitfrom <- utScan(unitstrfrom)
unitto   <- utScan(unitstrto)
newvals  <- utConvert( unitfrom, unitto, val=valstoconvert )

print(paste(valstoconvert,unitstrfrom,"equals",newvals,unitstrto))

