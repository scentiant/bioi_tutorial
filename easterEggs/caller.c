/* Michael E. Sparks, 11-17-16 (updated 10-16-20)

   caller.c - Driver application to demo interfacing C
              with Scheme code using the Guile library. */

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <libguile.h>

int main(int argc,char **argv) {
  unsigned int eltCnt=5,
               gameIndex;
  SCM exprToCall;

  if(argc > 1 && ((eltCnt=atoi(argv[1])) < 1 || eltCnt > 15)) {
    printf("Usage: %s (1 <= N <= 15)\n",argv[0]);
    return(EXIT_FAILURE); /* synonymous with return(1) */
  }

  scm_init_guile();

  do {
    printf("\nPlease select a game to play:\n\
    (0) *quit playing these games*\n\
    (1) Conway's \"Say It!\" Sequence\n\
    (2) Prime Finder\n\
    (3) Countdown\n\n");

    scanf("%u",&gameIndex);
    printf("\n");

    switch(gameIndex) {
      case 0 :
        gameIndex=false;
        break;
      case 1 :
        scm_c_primitive_load("sayit.scm");
        exprToCall=scm_variable_ref(scm_c_lookup("sayit"));
        scm_call_1(exprToCall, scm_from_int(eltCnt));
        break;
      case 2 :
        scm_c_primitive_load("fermat-little-theorem.scm");
        exprToCall=scm_variable_ref(scm_c_lookup("list-primes"));
        scm_call_0(exprToCall);
        break;
      case 3 :
        scm_c_primitive_load("sayit.scm");
        exprToCall=scm_variable_ref(scm_c_lookup("countdown"));
        scm_call_1(exprToCall, scm_from_int(eltCnt));
        break;
      default :
        printf("I did not recognize your selection.\n");
        gameIndex=true; /* not technically necessary! */
    }
  } while(gameIndex);

  return(EXIT_SUCCESS); /* synonymous with return(0) */
}
