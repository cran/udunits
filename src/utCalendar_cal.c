#include <stdio.h>
#include <math.h>
#include <strings.h>
#include "udunits.h"
#include "utCalendar_cal.h"

/* define DEBUG */

/*                                         J   F   M   A   M   J   J   A   S   O   N   D    */
static long days_per_month_reg_year[] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };
static utUnit udu_origin_zero;
static double udu_sec_since_ref_date( double val, utUnit *dataunits, int *yr0, int *mon0, 
		int *day0, int *hr0, int *min0, float *sec0 );
static int utCalendar_noleap_inner( double val, utUnit *dataunits, int *year, int *month, int *day, int *hour, 
				int *minute, float *second, int days_per_year, long *days_per_month );
static int utCalendar_360( double val, utUnit *dataunits, int *year, int *month, int *day, int *hour, 
				int *minute, float *second );
static int utCalendar_noleap( double val, utUnit *dataunits, int *year, int *month, int *day, int *hour, 
				int *minute, float *second );

/******************************************************************************/
/* This extends the standard utCalendar call by recognizing CF-1.0 compliant
 * calendar names.
 * Here are some check values:
 *
 *   Units string		calendar 	input val	 Output date  	note
 * ---------------		--------	----------	 -----------	----
 * days since 0001-01-01	standard	146000 (400x365) 0400-09-23	400*365 days leaves us 100 leap days short of 1 Jan
 * days since 1001-01-01	standard	146000 (400x365) 1400-09-23	Advance by 1000 yrs leaves same deficit as above
 * days since 1601-01-01	standard	146000 (400x365) 2000-09-26	NOW have 3 less leap days than prev, since udunits 
 *										switches to Gregorian calendar after 1582
 * days since 1001-01-01	standard	-146000		 0601-04-11	works with neg vals too; 400*365 leaves us in day #101
 * days since 2001-01-01	standard	-146000		 1601-04-08	Gregorian calendar, vs. Julian in prev line
 *
 * days since 1001-07-15	standard	146000		 1401-04-06	Offset can be other than first day of year; fall 100 
 *										days short of going exactly 400 yrs, due to lack of leap days
 *
 * days since 0001-01-01	noleap		146000		 0401-01-01	No leap days, 400*365 = 400 yrs exactly
 * days since 1601-01-01	noleap		146000		 2001-01-01	"noleap" calendar doesn't change behavior around 1582
 * days since 2001-01-01	noleap		-146000		 1601-01-01	works with neg values too
 * days since 1001-01-01	noleap		-146000		 0601-01-01	neg values don't care before/after 1582 either
 */
int utCalendar_cal( double val, utUnit *dataunits, int *year, int *month, int *day, int *hour, 
				int *minute, float *second, char *calendar ) 
{
	int err;
	static int have_shown_warning = 0;
	static int have_initted = 0;

#ifdef DEBUG
	printf( "entering utCalendar_cal\n" );
	printf( "Input value: %lf  Input calendar: %s\n", val, calendar );
#endif

	if( have_initted == 0 ) {
#ifdef DEBUG
		printf( "utCalendar_cal: initting\n" );
#endif
		/*-------------------------------------------------------------------------------------------
		 * The idea of this snippet is to "trick" the udunits library into telling us the year, month,
		 * and date that the user specified in the units string.  This prevents us from having to 
		 * reinvent the wheel by parsing the units string ourselves.  See further comments
		 * in routine udu_sec_since_ref_date
		 *------------------------------------------------------------------------------------------*/
		err = utScan( "seconds since 1234-05-06 00:00", &udu_origin_zero );  /* YYYY-MM-DD used here is irrelevant */
		if( err == 0 ) {
			udu_origin_zero.origin = 0.0;   /* override specified YYYY-MM-DD to set to same date as lib uses internally */
			}
		else
			{
			fprintf( stderr, "Error, could not decode internal date string for reference date!\n" );
			return(-1);
			}
		have_initted = 1;
		}

	if( (calendar == NULL) || (strncasecmp(calendar,"standard",8)==0) || (strncasecmp(calendar,"gregorian",9)==0) ) {
#ifdef DEBUG
		printf( "utCalendar_cal: using standard calendar\n" );
#endif
		return( utCalendar( val, dataunits, year, month, day, hour, minute, second ));
		}
	else if( (strncasecmp(calendar,"365_day",7)==0) || (strncasecmp(calendar,"noleap",6)==0) ) {
#ifdef DEBUG
		printf( "utCalendar_cal: using 365-day calendar\n" );
#endif
		return( utCalendar_noleap( val, dataunits, year, month, day, hour, minute, second ));
		}
	else if( strncasecmp(calendar,"360_day",7)==0) {
#ifdef DEBUG
		printf( "utCalendar_cal: using 360-day calendar\n" );
#endif
		return( utCalendar_360( val, dataunits, year, month, day, hour, minute, second ));
		}
	else if( strncasecmp(calendar,"proleptic_gregorian",19)==0) {
		fprintf( stderr, "sorry, proleptic_gregorian calendar not implemented yet; using standard calendar\n" );
		return( utCalendar( val, dataunits, year, month, day, hour, minute, second ));
		}
	else if( strncasecmp(calendar,"julian",6)==0) {
		fprintf( stderr, "sorry, julian calendar not implemented yet; using standard calendar\n" );
		return( utCalendar( val, dataunits, year, month, day, hour, minute, second ));
		}
	else
		{
		if( ! have_shown_warning ) {
			fprintf( stderr, "WARNING: unknown calendar: \"%s\". Using standard calendar instead!\n", calendar );
			have_shown_warning = 1;
			return( utCalendar( val, dataunits, year, month, day, hour, minute, second ));
			}
		}
}

/******************************************************************************/
double udu_sec_since_ref_date( double val, utUnit *dataunits, int *yr0, int *mon0, 
		int *day0, int *hr0, int *min0, float *sec0 )
{
	double retval;
	int err;

        /*---------------------------------------------------------------------
         * Use a bit of a trick to get the year, month, and day that the
         * original user specified in the units string and was subsequently
         * parsed by the udunits library.  We make use of the fact that the
         * udunits offset is set to some specific date -- it doesn't matter
         * which.  But everything is referenced to this date.  So we make
         * a time units, and SET ITS ORIGIN TO ZERO, so that it is "as if"
         * we specified the date relative to udunit's internal reference
         * date (currently, 2001-01-01).  We then convert the origin of the
         * user-specified units, which is the number of seconds since the
         * udunits reference date, into a calendar date.  Voila!  We then
         * have the year, month, day, etc. that the user specified.
         *--------------------------------------------------------------------*/
	err = utCalendar( dataunits->origin, &udu_origin_zero, yr0,
		mon0, day0, hr0, min0, sec0 );  /* note: all these yr0 etc values are RETURNED */

	retval = val * dataunits->factor;

	return( retval );
}

/*************************************************************************************/
/* A calendar with no leap days; days per month and year are passed in, not assumed!  
 * Can pass in days_per_year=365 and days_per_month={30,28,31,30, etc} for a "noleap" calendar,
 * or days_per_year=360 and days_per_month={30,30,30,...} for a "360 day" calendar
 */
int utCalendar_noleap_inner( double val, utUnit *dataunits, int *year, int *month, int *day, int *hour, 
				int *minute, float *second, int days_per_year, long *days_per_month )
{
	int yr0, mon0, day0, hr0, min0;
	float sec0;
	double ss, ss_extra;
	long dy, ds, sec_per_day, sec_per_hour, sec_per_min, ss_i, nny;
	long ndays, nhrs, nmin;

	sec_per_day   = 86400;
	sec_per_hour  = 3600;
	sec_per_min   = 60;

	/* -------------------------------------------------------------------------------------
	 * Get both the REFERENCE TIME that the netCDF file specifies for the units string
	 * (yr0, mon0, day0, etc) and the number of seconds since that reference date.  I.e.,
	 * if the units string is "days since 1979-01-01", then the reference date is 1 Jan 1979
	 * and 'ss' is the number of seconds since that date that we want to turn into a calendar
	 * date, given the specified calendar.
	 *--------------------------------------------------------------------------------------*/
	ss = udu_sec_since_ref_date( val, dataunits, &yr0, &mon0, &day0, &hr0, &min0, &sec0 );
#ifdef DEBUG
 	printf( "converting time %lf seconds since %04d-%02d-%02d %02d:%02d\n", ss, yr0, mon0, day0, hr0, min0 ); 
#endif

	/*--------------------------------------------------------------------------
	 * If we have a date before our reference date (indicated by a negative ss),
	 * then wind back the reference date to to be before the target
	 * date.  This avoids having to muck around with negative offsets.
	 *------------------------------------------------------------------------*/
	if( ss < 0 ) {
		nny = -ss / (sec_per_day*days_per_year) + 1;
		yr0 -= nny;
		ss  += nny * sec_per_day*days_per_year;
		}

	/*-------------------------------------------------------------------
	 * We now have seconds since (ss) yr0, mon0, day0, hr0, min0, sec0.
	 * Try to turn this into integer days since reference date and seconds
	 * extra, avoiding problems with roundoff and limited precision.  Not
	 * an exact science.  We use days since (ds) instead of sticking 
	 * strictly with seconds since (ss) becuase we can overflow longs in
	 * fairly routine circumstances, if tring to put a century or so of
	 * seconds into a long.
	 *-------------------------------------------------------------------*/
	ds = (long)((ss + .01)/(double)sec_per_day);
	ss_extra = ss - ((double)ds)*((double)sec_per_day);  /* need to be careful of overflow here */
	if( ss_extra < 0. )
		ss_extra = 0;

#ifdef DEBUG
	printf( "# of days since ref date: %ld   # of extra seconds after days taken out: %lf\n", ds, ss_extra );
#endif

	/*--------------------------------------
	 * Easier to do things relative to 1 Jan 
	 *-------------------------------------*/
	if( (min0 != 0) || (hr0 != 0) || (day0 != 1) || (mon0 != 1)) {

		ss_extra += min0 * sec_per_min;
		min0 = 0;

		ss_extra += hr0 * sec_per_hour;
		hr0 = 0;

		ds += (day0-1);
		day0 = 1;

		while( mon0 > 1 ) {
			ds += days_per_month[ mon0-2 ];	/*  -2 cuz -1 for prev month, -1 for 0 offset */
			mon0--;
			}
		}

	dy = ds / days_per_year;
	*year = yr0 + dy;
	ds = ds - dy*days_per_year;

	*month = 1;
	while( ds > days_per_month[(*month) - 1] - 1 ) {
		ds -= days_per_month[(*month) - 1];
		(*month)++;
		}

	*day = ds + 1;

	nhrs = ss_extra / sec_per_hour;
	*hour = nhrs;
	ss_extra -= nhrs * sec_per_hour;

	nmin = ss_extra / sec_per_min;
	*minute = nmin;
	ss_extra -= nmin * sec_per_min;

	*second = ss_extra;

	return(0);
}

/******************************************************************************/
int utCalendar_360( double val, utUnit *dataunits, int *year, int *month, int *day, int *hour, 
				int *minute, float *second )
{
	long days_per_year;

	days_per_year = 360L;
	long days_per_month_360[] = { 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30 };

	return( utCalendar_noleap_inner( val, dataunits, year, month, day, hour, minute, second,
		days_per_year, days_per_month_360 ));
}

/******************************************************************************/
int utCalendar_noleap( double val, utUnit *dataunits, int *year, int *month, int *day, int *hour, 
				int *minute, float *second )
{
	long days_per_year;

	days_per_year = 365L;

	return( utCalendar_noleap_inner( val, dataunits, year, month, day, hour, minute, second,
		days_per_year, days_per_month_reg_year ));
}


