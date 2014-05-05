#ifndef DAGMC_IFACE_H
#define DAGMC_IFACE_H

#include "MBInterface.hpp"
#ifdef __cplusplus
extern "C" {
#endif

/* initialize DAGMC from FORTRAN main 
 * @param max_pbl - The maximum index of the pblcm (temporary particle state)
 * array
 *                  This is the largest n that will arrive in calls to savpar
 *                  and getpar
 */
  void dagmcinit_(char *cfile, int *clen);


  /* Point-in-volume query.  Determine if the particle at given coordinates
   * is inside or outside of cell i1.  Return result=1 if outside or on
   * boundary,
   * and result=0 if inside.  
   */
  void dagmcchkcel_(double *xxx,double *yyy,double *zzz, int *vol, int
*result);

  /* Funtion to return the number of volumes
   */
  int dagmc_num_vol_();

 

#ifdef __cplusplus
} // extern "C"
#endif

#endif /* DAGMC_IFACE_H */
