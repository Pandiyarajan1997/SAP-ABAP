
*=======================================================================
*  Author                    : T.Pandiarajan
*
*  Date                      : 18.10.2023
*
*  Requester Name            : Ramakrishnan
*
*  Request ID                : DEVK934530
*
*  Business Logic            : ALV Report For Customer maintanence of Sundaram Finance
*
*  Released on Date          :
*
*=======================================================================

REPORT zsd_sf_cust_inv.

*contains selection screen design
INCLUDE zsd_sf_cust_inv_s01.

*contains the class definitions & implementations
INCLUDE zsd_sf_cust_inv_c01.

*contains selection screen events
INCLUDE zsd_sf_cust_inv_e01.

*contains all PAI & PBO Modules
INCLUDE zsd_sf_cust_inv_pai.
