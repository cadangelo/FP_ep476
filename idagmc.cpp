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
  MBEntityHandle vol = DAG->entity_by_index(3,*vol_idx);

  MBErrorCode rval;
  std::vector<EntityHandle> children;
  EntityHandle firstEnt;

  /* get first child surface */
  rval = DAG->moab_instance()->get_child_meshsets(vol,children);
  if (MB_SUCCESS != rval) 
    {
      std::cerr << "DAGMC failed to get surfaces for volume " << vol_idx << std::endl;
      exit(EXIT_FAILURE);
    }
  firstEnt = children[0];

  /* get vertices of first surface */
  rval = DAG->moab_instance()->get_entities_by_dimension(children[0],0,children);
  if (MB_SUCCESS != rval)
    {
      std::cerr << "DAGMC Failed to get a vertex from surface " << DAG->get_entity_id(firstEnt) << std::endl;
      exit(EXIT_FAILURE);
    }
  
  /* get coordinate of first vertex */
  rval = DAG->moab_instance()->get_coords(children[0],xxx,yyy,zzz);
  if (MB_SUCCSES != rval)
    {
      std::cerr << "DAGMC Failed to get coordinates" << std::endl;
      exit(EXIT_FAILURE);
    }

  

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

int dagmc_vol_id_(int *vol_idx)
{
  return DAG->id_by_index(3,*vol_idx);
}
  

int dagmc_num_vol_()
{
   // number of volumes
   int num_vol = DAG->num_entities(3);
   return num_vol;

}
