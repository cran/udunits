#include <stdio.h>
#include <udunits.h>
#include <string.h>

#include <Rdefines.h>

/******************************************************************/
/* Initializes the udunits package.
 * Does NOT support the 'path' argument -- set it using UDUNITS_PATH
 * environmental variable instead!
 * Return value is 0 on success, not zero otherwise.
 */
void R_utInit( int *retval )
{
	*retval = utInit(NULL);
	if( *retval == 0 )
		return;

	if( *retval == UT_ENOFILE ) {
		fprintf( stderr, "utInit (R version): error: units file not found!\n");
		fprintf( stderr, "Set environmental variable UDUNITS_PATH to the fully\n");
		fprintf( stderr, "qualified filename of the udunits units file\n");
		fprintf( stderr, "(usually named udunits.dat, often found in /usr/local/lib)\n");
		return;
		}

	if( *retval == UT_EALLOC ) {
		fprintf( stderr, "utInit (R version): memory allocation error!!\n");
		return;
		}

	if( *retval == UT_EIO ) {
		fprintf( stderr, "utInit (R version): I/O error occurred while processing\n");
		fprintf( stderr, "the udunits file!\n" );
		return;
		}

	if( *retval == UT_EUNKNOWN ) {
		fprintf( stderr, "utInit (R version): error: udunits file contains an\n");
		fprintf( stderr, "unknown specification!\n" );
		return;
		}

	if( *retval == UT_ESYNTAX ) {
		fprintf( stderr, "utInit (R version): error: I found a syntax error\n");
		fprintf( stderr, "in the udunits file!\n" );
		return;
		}

	fprintf( stderr, "utInit (R version): error with unknown code: %d!\n",
		*retval );
}

/******************************************************************
 * Converts from the udunits library-style unit structure to the
 * R-style version of the unit structure.
 * Input:
 * 	unit_in: the udunits-library style unit to be converted
 * 		to R-style
 * Outputs:
 * 	origin_factor, hasorigin_powers: describe the udunits-processed
 * 		unit in R style.
 */
void R_ututil_utUnit_to_Rstyle( utUnit *unit_in, double *origin_factor, 
	int *hasorigin_powers )
{
	int i;

	origin_factor[0] = (double)unit_in->origin;
	origin_factor[1] = (double)unit_in->factor;

	hasorigin_powers[0] = (int)unit_in->hasorigin;
	for( i=0; i<UT_MAXNUM_BASE_QUANTITIES; i++ )
		hasorigin_powers[i+1] = (int)unit_in->power[i];
}

/******************************************************************
 * Converts from the R-style version of the unit structure to the
 * udunits library style version of the unit structure.
 * Inputs:
 * 	origin_factor, hasorigin_powers: describe the udunits-processed
 * 		unit in R style.
 * Outputs:
 * 	unit_out: the udunits-library style unit corresponding to the
 * 		passed R-style unit.
 *
 */
void R_ututil_Rstyle_to_utUnit( double *origin_factor, int *hasorigin_powers, 
	utUnit *unit_out )
{
	int i;

	unit_out->origin    = (UtOrigin)origin_factor[0];
	unit_out->factor    = (UtFactor)origin_factor[1];
	unit_out->hasorigin = (int)hasorigin_powers[0];
	for( i=0; i<UT_MAXNUM_BASE_QUANTITIES; i++ )
		unit_out->power[i] = (short)hasorigin_powers[i+1];
}

/******************************************************************/
/* Converts a formatted units string into a group of two double
 * precisions (origin, factor) and UT_MAXNUM_BASE_QUANTITIES+1
 * integers (hasorigin, then the UT_MAXNUM_BASE_QUANTITIES powers).
 * Return value is 0 on success, not zero otherwise.
 */
void R_utScan( char **spec, double *origin_factor, int *hasorigin_powers,
		int *retval )
{
	utUnit 	u;

	if( (*retval = utScan( spec[0], &u )) != 0 ) {
		if( *retval == UT_ENOINIT ) {
			fprintf( stderr, "utScan (R version): error: udunits package not initialized yet!\n");
			fprintf( stderr, "You must call utInit() first.\n" );
			return;
			}

		if( *retval == UT_EINVALID ) {
			fprintf( stderr, "utScan (R version): error: invalid unit argument!\n");
			return;
			}

		if( *retval == UT_EUNKNOWN ) {
			fprintf( stderr, "utScan (R version): error: your specification contains an unknown unit!\n" );
			return;
			}

		if( *retval == UT_ESYNTAX ) {
			fprintf( stderr, "utScan (R version): error: your specification contains a syntax error!\n" );
			return;
			}

		fprintf( stderr, "utScan (R version): unknown error %d!\n", *retval );
		return;
		}

	/* Convert unit to R-style for return */
	R_ututil_utUnit_to_Rstyle( &u, origin_factor, hasorigin_powers );
}

/******************************************************************/
/* Inputs:
 *	value: the amount of time units we want to convert to a
 *		calendar date.
 *	origin_factor, hasorigin_powers: describe the udunits-processed
 *		time unit.
 * Outputs:
 *	ymdhm: integer year, month, day, hour, and minute.
 *	second: seconds
 *	retval: 0 on success, not 0 otherwise.
 */
void R_utCalendar( double *value, double *origin_factor, int *hasorigin_powers,
	int *ymdhm, double *second, int *retval )
{
	utUnit 	u;
	float	f_sec;

	R_ututil_Rstyle_to_utUnit( origin_factor, hasorigin_powers, &u );

	if( (*retval = utCalendar( *value, &u, ymdhm, (ymdhm+1),
			(ymdhm+2), (ymdhm+3), (ymdhm+4), &f_sec )) != 0 ) {
		if( *retval == UT_ENOINIT ) {
			fprintf( stderr, "utCalendar (R version): error: udunits package not initialized yet!\n");
			fprintf( stderr, "You must call utInit() first.\n" );
			return;
			}

		if( *retval == UT_EINVALID ) {
			fprintf( stderr, "utCalendar (R version): error: units are not time-like!\n" );
			return;
			}

		fprintf( stderr, "utCalendar (R version): unknown error %d!\n",
			*retval );

		return;
		}

	*second = (double)f_sec;
}

/******************************************************************/
/* Note this is not the same thing as being a calendar.  A calendar
 * has BOTH a time unit and an origin.
 */
void R_utIsTime( double *origin_factor, int *hasorigin_powers, int *retval )
{
	utUnit u;

	R_ututil_Rstyle_to_utUnit( origin_factor, hasorigin_powers, &u );
	*retval = utIsTime( &u );
}

/******************************************************************/
void R_utHasOrigin( double *origin_factor, int *hasorigin_powers, int *retval )
{
	utUnit u;

	R_ututil_Rstyle_to_utUnit( origin_factor, hasorigin_powers, &u );
	*retval = utHasOrigin( &u );
}

/******************************************************************/
/* Converts a given year, month, day, hour, minute, and second
 * into a value, given an appropriate time unit to work with.
 * Inputs:
 *	ymdhm: array of year, month, day, hour, and minute integer values
 *	second: double precision value
 *	origin_factor, hasorigin_powers: describe the udunit
 * Outputs:
 * 	value: what that date corresponds to, in the described units
 *	retval: 0 if no error, not 0 if an error.
 */
void R_utInvCalendar( int *ymdhm, double *second, double *origin_factor, 
	int *hasorigin_powers, double *value, int *retval )
{
	utUnit 	u;

	R_ututil_Rstyle_to_utUnit( origin_factor, hasorigin_powers, &u );

	if( (*retval = utInvCalendar( ymdhm[0], ymdhm[1], ymdhm[2],
			ymdhm[3], ymdhm[4], *second, &u, value )) != 0 ) {
		if( *retval == UT_ENOINIT ) {
			fprintf( stderr, "utInvCalendar (R version): error: udunits package not initialized yet!\n");
			fprintf( stderr, "You must call utInit() first.\n" );
			return;
			}

		if( *retval == UT_EINVALID ) {
			fprintf( stderr, "utInvCalendar (R version): error: units are not temporal!\n" );
			return;
			}

		fprintf( stderr, "utInvCalendar (R version): unknown error %d!\n",
			*retval );
		return;
		}
}

/******************************************************************
 * Returns the coefficients for the linear transformation between
 * the "from" units and the "to" units.
 * Inputs:
 * 	from_origin_factor, from_hasorigin_powers: describe the udunit
 * 		we are converting *from*
 * 	to_origin_factor, to_hasorigin_powers: describe the udunit we
 * 		are converting *to*
 * Outputs:
 * 	slope, intercept: values for transforming the units
 * 	retval: 0 if no error, not 0 if an error
 *
 */
void R_utConvert( double *origin_factor_from, int *hasorigin_powers_from,
	double *origin_factor_to, int *hasorigin_powers_to,
	double *slope, double *intercept, int *retval )
{
	utUnit	u_from, u_to;

	R_ututil_Rstyle_to_utUnit( origin_factor_from, hasorigin_powers_from, &u_from );
	R_ututil_Rstyle_to_utUnit( origin_factor_to, hasorigin_powers_to, &u_to );

	if( (*retval = utConvert( &u_from, &u_to, slope, intercept )) != 0 ) {
		if( *retval == UT_ENOINIT ) {
			fprintf( stderr, "utConvert (R version): error: udunits package not initialized yet!\n");
			fprintf( stderr, "You must call utInit() first.\n" );
			return;
			}

		if( *retval == UT_EINVALID ) {
			fprintf( stderr, "utConvert (R version): error: passed an invalid unit structure!\n" );
			return;
			}

		if( *retval == UT_ECONVERT ) {
			fprintf( stderr, "utConvert (R version): error: units are incompatible, cannot convert between them!\n" );
			return;
			}

		fprintf( stderr, "utConvert (R version): unknown error %d!\n", *retval );
		return;
		}
}

