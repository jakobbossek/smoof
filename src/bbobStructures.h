#ifndef _bbobStructures_H
#define _bbobStructures_H

/* some structures, to try to imitate that gorgious Matlab Code !!! 
*/

/* sometimes, M_PI is not defined ????? */
#ifndef M_PI
  #define M_PI        3.14159265358979323846
#endif

/* the return type of all benchmark functions: 2 doubles, the true value and the noisy value - are equal in case of non-noisy functions */
struct twoDoubles {
double Ftrue;
double Fval;
};

typedef struct twoDoubles TwoDoubles;

/* and now the type of the benchmark functions themselves */
typedef struct twoDoubles (*bbobFunction)(double *);

/* need to put all parameters into a single structure that 
can be passed around 
*/
/* all static char* (increase if your system uses huge file names) */
#define DefaultStringLength 1024
/* for not having to allocate the x in struct  lastEvalStruct */
#define DIM_MAX 200
/* These ones might be defined somewhere in the C headers, but in case it's system-dependent ... */
/*#define MAX_FLOAT 1.0E308
#define MIN_FLOAT 1.0E-308
#define MAX_INT 32767*/

struct paramStruct{
/* all chars have a fixed length, to ease memory management */

/* a string for the name of the optimiser (used in the post-processing)
   If not specified, PARAMS.algName = 'not-specified'; */
char algName[DefaultStringLength];  

/* a string that must not contain
   end-of-line characters that will appear as comment in the index
   file (preceded by '% ') and should be used to precise some
   differentiating parameters of the optimiser.
   If not specified, PARAMS.comments = ''; */ 
char comments[DefaultStringLength]; 

/* a string for the path of the index and data files files. */
char dataPath[DefaultStringLength];

/* a string for the prefix of the index and data files names.
  If not specified, PARAMS.filePrefix = 'bbobexp'; */
char filePrefix[DefaultStringLength];

/* The dimension: in C it has to be passed as an argument at init time */
unsigned int DIM;

/* the precision */
double precision;

/* the benchmark function ID */
unsigned int funcId;

/* the instance ID */
unsigned int instanceId;

/* the path to the index file */
char indexFilePrefix[DefaultStringLength];

/* index file name */
char indexFile[DefaultStringLength]; /* will be  ('%s_f%d.info', indexFilePrefix, FUNC_ID); */

/* prefixes for data files */
char dataFilePrefix[DefaultStringLength]; 
char dataFilePrefixNameOnly[DefaultStringLength]; 

/* names of data file */
char dataFile[DefaultStringLength]; 
char dataFileNameOnly[DefaultStringLength]; 

/* names of H-data file */
char hdataFile[DefaultStringLength]; 
char hdataFileNameOnly[DefaultStringLength]; 

/* brute force suffix (no need to extract it from file name!) */
unsigned int dataFileSuffix;

/* counter for number of evaluations */
unsigned int runCounter;

} ;

typedef struct paramStruct ParamStruct;

/*********************************
BestFEval 
**********************************/
struct lastEvalStruct {
/* Number of evaluations */
unsigned int num;

/* fitness value */
double F;

/* Noisy fitness value */
double Fnoisy;

/* best noisy value up to now ??? */
double bestFnoisy;

/* value of vector x */
double x[DIM_MAX];

/* has it been written to file? */
int isWritten;
} ;

typedef struct lastEvalStruct LastEvalStruct;

/*********************************88
    The interface 
***********************************/

double fgeneric_initialize(ParamStruct PARAMS);
double  fgeneric_finalize(void);
double fgeneric_evaluate(double * X);
double fgeneric_ftarget(void);
unsigned int fgeneric_maxevals(unsigned int DIM);
unsigned int fgeneric_evaluations(void);
double fgeneric_best(void);
void fgeneric_noiseseed(unsigned int seed);
ParamStruct fgeneric_getDefaultPARAMS(void);

#endif
