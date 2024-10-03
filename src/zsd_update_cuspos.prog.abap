*=======================================================================
*  Author                    : T.Pandiarajan
*
*  Date                      : 21.10.2023
*
*  Requester Name            : Ramakrishnan
*
*  Request ID                : DEVK934544
*
*  Business Logic            : Excel upload program for customer & position code
*
*  Released on Date          :
*
*=======================================================================
REPORT zsd_update_cuspos.



*contains selection screen design
INCLUDE zsd_update_cuspos_s01.

*contains the class definitions & implementations
INCLUDE zsd_update_cuspos_c01.

*contains selection screen events
INCLUDE zsd_update_cuspos_e01.

*contains all PAI & PBO Modules
INCLUDE zsd_update_cuspos_pai.
