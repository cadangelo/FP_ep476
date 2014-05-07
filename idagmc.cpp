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

void dagmcpoint_on_surf_(double *xxx, double *yyy, double *zzz, int *vol_idx)
{
  // fire ray from point x,y,z to edge of sphere to get point on surface
  double xyz[3]={*xxx,*yyy,*zzz};
  double dir[3]={rand(),rand(),rand()};
  MBEntityHandle vol = DAG->entity_by_index(3,*vol_idx);
  MBEntityHandle next_surf; // next surface we cross
  double next_dist;
  MBErrorCode rval = DAG->ray_fire(vol, xyz, dir, next_surf, next_dist);
//  next_dist = next_dist - 1.0e-6;
  std::cout << "next_dst = " << next_dist << std::endl;
 *xxx += (dir[0]*next_dist);
 *yyy += (dir[1]*next_dist);
 *zzz += (dir[2]*next_dist);

}

void dagmcchkcel_(double *xxx,double *yyy,double *zzz,int *vol_idx, int *result)
{
  int inside;
  MBEntityHandle vol = DAG->entity_by_index( 3, *vol_idx );
  double xyz[3] = {*xxx, *yyy, *zzz};
  
  double uvw[3] = {0,0,0};
  MBErrorCode rval = DAG->point_in_volume( vol, xyz, inside);

  if (MB_SUCCESS != rval) {
    std::cerr << "DAGMC: failed in point_in_volume" <<  std::endl;
    exit(EXIT_FAILURE);
  }

  if (MB_SUCCESS != rval) 
  {
        *result = -2;
  }        
  else
  {



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

}

int dagmc_num_vol_()
{
   // number of volumes
   int num_vol = DAG->num_entities(3);
   return num_vol;

}
