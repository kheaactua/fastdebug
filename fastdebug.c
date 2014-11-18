#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <time.h>
#include <alloca.h> 

// If I want a no mpi version, define this macro
#ifdef NOMPI
#define MPI_COMM_WORLD 0
int MPI_Initialized(int *result) { *result=1; }
int MPI_Comm_rank(int unused, int *myrank ){ *myrank=0 ; }
int MPI_Init(int *argc, char ***argv) { return 0; }
int MPI_Finalize() { return 0 ;}
#else
#include <mpi.h>
#endif

// Default value of whether this routine does anything
#define START_AS_STUB 0

static int fp=-1;
static int mypid=-1;
static int myrank=999;
static int fast_debug_error=0;
static int is_stubbed = START_AS_STUB;

#ifdef DEBUG
#define VERBOSE 1
#else
#define VERBOSE 0
#endif
#define USE_MPI 1
#define MAX_STR_LEN 2048
#define ROUGH_PREAMBLE_LENGTH 100

// Setters to toggle debug
void fastdebug_activate_(long on_off) {
	is_stubbed = (int)on_off;
}

//
// Returns unix time stamp with micro time appended
long long f_gettimeofday_() {
	struct timeval tv;
	long long the_time;

	gettimeofday(&tv,NULL);
	the_time=tv.tv_sec;
	the_time=the_time*1000000 + tv.tv_usec;
	return (the_time);
}

/* Simple trim function taken from
 * http://stackoverflow.com/questions/122616/how-do-i-trim-leading-trailing-whitespace-in-a-standard-way
 * This function shifts the string into the first position of the buffer.
 * This way, if the string was dynamically allocated, it can be deallocated
 * on the original pointer.
 */
char *trim(char *str) {
	size_t len = 0;
	char *frontp = str - 1;
	char *endp = NULL;

	if( str == NULL ) return NULL;

	if( str[0] == '\0' ) return str;

	len = strlen(str);
	endp = str + len;

	/* Move the front and back pointers to address
	* the first non-whitespace characters from
	* each end.
	*/
    while( isspace(*(++frontp)) );
    while( isspace(*(--endp)) && endp != frontp );

	if ( str + len - 1 != endp )
		*(endp + 1) = '\0';
	else if( frontp != str &&  endp == frontp )
		*str = '\0';

	/* Shift the string so that it starts at str so
	* that if it's dynamically allocated, we can
	* still free it on the returned pointer.  Note
	* the reuse of endp to mean the front of the
	* string buffer now.
	*/
	endp = str;
	if( frontp != str ) {
		while( *frontp ) *endp++ = *frontp++;
		*endp = '\0';
	}

	return str;
}

int fastdebug_(const char *str, int *lineno ,int str_len) {

	int flag;
	mode_t mode = S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH;
	char *msg, *nstr;
	char hostname[30], fname[50];
//	char prefix[] = "/tmp/gmdebug/debug-";
	char *prefix = "/scratch/gmdebug-";
	char preamble[ROUGH_PREAMBLE_LENGTH];

	// Bail immediately if there has been an error, or we're simply deactivated
	// and acting as a stub file.
	if (fast_debug_error || is_stubbed) return 0;

	// Get the hostname
	gethostname(hostname, sizeof hostname);

	MPI_Initialized(&flag);
	if(flag==0) return 1;
	if (fp == -1) {
		if(NULL!=getenv("MATT_PREFIX")) prefix=getenv("MATT_PREFIX");
		mypid=getpid();
		MPI_Initialized(&flag);
		if(flag) MPI_Comm_rank(MPI_COMM_WORLD, &myrank );
		else fprintf(stderr,"[fastdebug] MPI NOT STARTED\n");

		// just for debug..
 		if (VERBOSE) mypid=0;

		snprintf(fname,sizeof(fname)-1,"%s%5.5d-%3.3d",prefix,mypid,myrank);
		if (VERBOSE) printf("Opening file %s\n", fname);
		if ((fp=open(fname, O_WRONLY | O_CREAT | O_APPEND, mode)) < 0) {
			fprintf(stderr, "[fastdebug]: Cannot open file %s on %s.  Deactivating fastdebug.\n", fname, hostname);
			//exit(1);
			fast_debug_error=1;
			return 0;
		}
	}

	// Write the preable, so, time, hostname, etc.
	snprintf(preamble, ROUGH_PREAMBLE_LENGTH+1,
	   "%lld [%s:%05d:%02d]",
	   f_gettimeofday_(),
	   hostname, mypid, myrank);

	// Magic number ROUGH_PREAMBLE_LENGTH is estimated length of all the
	// host/pid/rank info in the debug line
	if ((str_len + strlen(preamble)) > MAX_STR_LEN) {
		// Truncate message
		str_len = MAX_STR_LEN - strlen(preamble);
	}
	str_len++; // null character

	// Trim white space
	if (VERBOSE) printf("[fastdebug] Trying to allocation %d characters for nstr \"%s\"\n", str_len, str);
	if ((nstr=(char *)alloca(str_len)) == NULL) {
		fprintf(stderr, "[fastdebug]: Cannot allocate msg %d characters for string.\n", (str_len+2));
		exit(1);
	}
	snprintf(nstr, str_len, "%s", str); // copy it to a mutable stirng
	nstr=trim(nstr); // trim white space
	str_len=strlen(nstr);


	// str_len will now be the entire output message (preample and all)
	// The 128 is a magic number buffer, for the spaces and line breaks, etc..
	str_len=str_len+strlen(preamble)+128;

	if (VERBOSE) printf("[fastdebug] Trying to allocation %d characters for combined string \"%s %s\"\n", str_len+2, preamble, nstr);
	if ((msg=(char *)alloca(str_len)) == NULL) {
		fprintf(stderr, "[fastdebug]: Cannot allocate msg %d characters for string.\n", (str_len+2));
		exit(1);
	}

	// Combine the string
	snprintf(msg, str_len, "%s(%5d) %s\n", preamble,*lineno, nstr);
	//snprintf(msg, str_len+2, "%s %s\n", nstr);

	if (VERBOSE) printf("Writing \"%s\" to %s\n", msg, fname);
	write(fp,msg,strlen(msg));

	/*close(fp);*/

	return 1;
}

// Function was first implemented under this name,
// so now this is just a deprecated redirection function
int mattdebug_(const char *str, int *lineno ,int str_len) {
	return fastdebug_(str, lineno, str_len);
}

// This is done to allow me to either directly build this
// as an executable, or build it with s.compile using a bidon
#if defined(TEST)
int main(int argc, char **argv) {
#else
int mattmain_(int argc, char **argv) {
#endif
	char msg[] = "Hello";
	long long the_time;
	int lineno;

	MPI_Init(&argc, &argv);

	the_time=f_gettimeofday_();
	printf("time=%lld\n", the_time);

	lineno = 1234;
	fastdebug_(msg,&lineno, strlen(msg));
	lineno = 2345;
	fastdebug_("Hello again",&lineno, 11);
	lineno = 3456;
	fastdebug_("Testing trim           ",&lineno, 23);

	MPI_Finalize();

	return 0;
}

