#include "idagmc.h"

#include "MBInterface.hpp"

#include "DagMC.hpp"
using moab::DagMC;

#include <iostream>

#define DAG DagMC::instance()

void dagmcinit_(char *cfile, int *clen)
{
 
  MBErrorCode rval;
  std::cout << cfile << ' ' << *clen << std::endl;
  // terminate all filenames with null char
  cfile[*clen] = '\0';

  // read geometry
  rval = DAG->load_file(cfile);

  if (MB_SUCCESS != rval) {
    std::cerr << "DAGMC failed to read input file: " << cfile << std::endl;
    exit(EXIT_FAILURE);
  }

  // initialize geometry
  rval = DAG->init_OBBTree();
  if (MB_SUCCESS != rval) {
    std::cerr << "DAGMC failed to initialize geometry and create OBB tree" <<std::endl;
    exit(EXIT_FAILURE);
  }


}


void dagmcchkcel_(double *xxx,double *yyy,double *zzz, int *vol_idx, int *result)
{

  int inside;
  MBEntityHandle vol = DAG->entity_by_index( 3, *vol_idx );
  double xyz[3] = {*xxx, *yyy, *zzz};
  double uvw[3] = {0,0,0};
  MBErrorCode rval = DAG->point_in_volume( vol, xyz, inside, uvw );

  if (MB_SUCCESS != rval) {
    std::cerr << "DAGMC: failed in point_in_volume" <<  std::endl;
    exit(EXIT_FAILURE);
  }

  if (MB_SUCCESS != rval) *result = -2;
  else
    switch (inside)
      {
      case 1: 
        *result = 0; // inside==  1 -> inside volume -> result=0
        break;
      case 0:
        *result = 1; // outside== 0  -> outside volume -> result=1
        break;
      case -1:
        *result = 0; // onboundary== -1 -> on boundary -> result=1 (assume leaving volume)
        break;
      default:
        std::cerr << "Impossible result in dagmcchkcel" << std::endl;
        exit(EXIT_FAILURE);
      }
}

int dagmc_num_vol_()
{
   // number of volumes
   int num_vol = DAG->num_entities(3);
   return num_vol;

}
