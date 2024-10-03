
*=======================================================================
*  Author                    : T.Pandiarajan
*
*  Date                      : 22.09.2023
*
*  Requester Name            : Ramakrishnan & Praveen
*
*  Request ID                : DEVK934432
*
*  Business Logic            : Material Master Maintenance Report for Safety Stock
*
*  Released on Date          :
*
*=======================================================================

REPORT zmm_material_master_update MESSAGE-ID ZMM_MAT.


*contains selection screen design
INCLUDE zmm_mat_master_s01.

*contains the class definitions & implementations
INCLUDE zmm_mat_master_c01.

*contains selection screen events
INCLUDE zmm_mat_master_e01.

*contains all PAI & PBO Modules
INCLUDE zmm_mat_master_pai.
