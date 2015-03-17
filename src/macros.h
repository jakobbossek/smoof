// define some macros
#define ALLOC_VECTOR(type, size) (PROTECT(allocVector(type, size)))
#define ALLOC_REAL_VECTOR(size) (ALLOC_VECTOR(REALSXP, size))
#define ALLOC_INTEGER_VECTOR(size) (ALLOC_VECTOR(INTSXP, size))
#define ALLOC_LIST(size) (ALLOC_VECTOR(VECSXP, size))
