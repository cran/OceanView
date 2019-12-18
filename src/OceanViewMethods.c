#include<R_ext/Rdynload.h>
#ifndef R_R_H
#  include <R.h>
#endif

/*
  SUBROUTINE crosstab(input, ninput, colnr, rownr, valnr,                   &
    &                    cols, rows, nr, nc, cross, count, NAnum)
  SUBROUTINE crosstab2(input, ninput, colnr, rownr, valnr,                  &
    &           cols, rows, nr, nc, nrow, ncol, indrow, indcol,             &
    &           cross, count, NAnum)
*/    


void F77_NAME(crosstab)(double*, int*, int*, int*, int*,
      double*,  double*, int*, int*, double*, int*, double*);
     
void F77_NAME(crosstab2)(double*, int*, int*, int*, int*,
      double*,  double*, int*, int*, int*, int*, int*, int*,
      double*, int*, double*);
     
          
R_FortranMethodDef OeanViewfortranMethods[] = {
 {"crosstab",(DL_FUNC) &F77_SUB(crosstab), 12},
 {"crosstab2", (DL_FUNC) &F77_SUB(crosstab2), 16},
 {NULL, NULL, 0}
};

void R_init_OceanView(DllInfo *info) {
  R_registerRoutines(info, NULL, NULL, OeanViewfortranMethods, NULL);
  R_useDynamicSymbols(info, FALSE); // disable dynamic searching  
}
