/* Michael E. Sparks, 16 Feb 2016
 *
 * Stablemate C function for the quorumCandidates
 * function I've written in R.
 *
 * Nothing profound here - systems with RTools installed
 * can benefit from this "portable assembly code" speedup.
 * Modify the Cavail variable in quorum_sense.R accordingly.
 * ``R CMD SHLIB quorumScoring.c"
 */

#include <R.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>

#define SCORE(VEC) \
for(i=0;i<seqlen-*wordlen+1;++i) { \
  *(VEC+i)=0.0; \
  for(j=0;j<*wordlen;++j) \
    *(VEC+i)+=log(*(probs+*(intseq+i+j)+j*5)); \
}

void scoreQuorumCandidates(
  char **seq,
  double *scoresF,
  double *scoresR,
  double *probs,
  int *wordlen
) {
  register int
      i,j,            /* iterator vars                          */
      revaux,         /* auxiliary var for reversing intseq     */
      seqlen;         /* stores length of sequence argument     */
  int *intseq=NULL;   /* storage for integer translation of seq */

  /* allocate space for sequence */
  seqlen=strlen(*seq);
  if((intseq=(int*)malloc(sizeof(int)*seqlen))==NULL) {
    Rprintf("Cannot allocate sufficient memory for sequence\n");
    exit(EXIT_FAILURE);
  }

  /* recode using integer scheme */
  for(i=0;i<seqlen;++i)
    switch(*(*seq+i)) {
      case('a') :
        *(intseq+i)=0;
        break;
      case('c') :
        *(intseq+i)=1;
        break;
      case('g') :
        *(intseq+i)=2;
        break;
      case('t') :
        *(intseq+i)=3;
        break;
      default :
        *(intseq+i)=4;
    }

  /* score forward strand */
  SCORE(scoresF)

  /* reverse strand... */
  for(i=0,j=seqlen-1;i<seqlen/2;++i,--j) {
    revaux=*(intseq+i);
    *(intseq+i)=*(intseq+j);
    *(intseq+j)=revaux;
  }

  /* ...and take its complement */
  for(i=0;i<seqlen;++i)
    switch(*(intseq+i)) {
      case(0) :
        *(intseq+i)=3;
        break;
      case(1) :
        *(intseq+i)=2;
        break;
      case(2) :
        *(intseq+i)=1;
        break;
      case(3) :
        *(intseq+i)=0;
        break;
      default :
        ;
    }

  /* score reverse strand */
  SCORE(scoresR)

  free(intseq);
  return;
}

