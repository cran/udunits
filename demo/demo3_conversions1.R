# Conversions can be done 2 ways.  You can call the convert
# routine to get the slope and intercept necessary for the conversion,
# then do the conversion yourself.  Or, you can have the convert routine
# do the conversion for you.  This example demonstrates the first of
# those two ways.  See demo4_conversions2.R for the other method.

unitstrfrom <- "m"	# Convert meters....
unitstrto   <- "in"	# ...to inches

valstoconvert <- c(3.,4.)

utInit()		# Initlialize the package
unitfrom <- utScan(unitstrfrom)
unitto   <- utScan(unitstrto)
res      <- utConvert( unitfrom, unitto )

print(paste(unitstrto,"=",unitstrfrom,"*",res$slope,"+",res$intercept)) # tell us what the conversion is

newvals <- res$slope*valstoconvert + res$intercept  # do the conversion

print(paste(valstoconvert,unitstrfrom,"equals",newvals,unitstrto))

# Above way is the most efficient for doing multiple conversions
# because it calls utScan() only once per unit.  However, it is also
# possible to directly pass the units string to utConvert():

res2 <- utConvert( unitstrfrom, unitstrto )
print(paste("should be same slope:",res2$slope," Should be the same intercept:",res$intercept))

