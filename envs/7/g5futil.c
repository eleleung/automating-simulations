#include <stdio.h>
#include <math.h>
#include <assert.h>
#include "g5util.h"

void
g5_open_(void)
{
  g5_open();
}

void
g5_close_(void)
{
  g5_close();
}

/*
double
g5_get_pcibus_freq_(void)
{
    return g5_get_pcibus_freq();
}
*/

void
g5_set_range_(double *xmin, double *xmax, double *mmin)
{
  g5_set_range(*xmin, *xmax, *mmin);
}

/*
void
g5_get_range_(double *xmin, double *xmax, double *mmin)
{
  g5_get_range(xmin, xmax, mmin);
}
*/

void
g5_set_jp_(int *adr, int *nj, double *mj, double (*xj)[3])
{
    g5_set_jp(*adr, *nj, mj, xj);
}

/*
void
g5_set_xmj_(int *adr, int *nj, double (*xj)[3], double *mj)
{
    g5_set_xmj(*adr, *nj, xj, mj);
}
*/

void
g5_calculate_force_on_x_(double (*x)[3], double (*a)[3], double *p, int *ni)
{
  g5_calculate_force_on_x(x, a, p, *ni);
}

void
g5_set_xi_(int *ni, double (*xi)[3])
{
  g5_set_xi(*ni, xi);
}

void
g5_run_(void)
{
  g5_run();
}

void
g5_set_n_(int *n)
{
  g5_set_n(*n);
}

/*
void
g5_set_eps_(int *ni, double *eps)
{
  g5_set_eps(*ni, eps);
}
*/

 /*
void
g5_set_eps2_(int *ni, double *eps2)
{
  g5_set_eps2(*ni, eps2);
}
*/

/*
void
g5_set_eps_to_all_(double *eps)
{
  g5_set_eps_to_all(*eps);
}
*/

void
g5_set_eps2_to_all_(double *eps2)
{
  g5_set_eps2_to_all(*eps2);
}

void
g5_get_force_(int *ni, double (*a)[3], double *p)
{
  g5_get_force(*ni, a, p);
}

/*
void
g5_set_cards_(int *c)
{
    g5_set_cards(c);
}
*/

 /*
void
g5_get_cards_(int *c)
{
    g5_get_cards(c);
}
 */

/*
int
g5_get_number_of_cards_(void)
{
    return g5_get_number_of_cards();
}
*/

int
g5_get_number_of_pipelines_(void)
{
    return g5_get_number_of_pipelines();
}

/*
int
g5_get_number_of_real_pipelines_(void)
{
    return g5_get_number_of_real_pipelines();
}
*/

int
g5_get_jmemsize_(void)
{
    return g5_get_jmemsize();
}

void
g5_set_cutoff_table_(double (*ffunc)(double), double *fcut, double *fcor,
                     double (*pfunc)(double), double *pcut, double *pcor)
{
    g5_set_cutoff_table(ffunc, *fcut, *fcor, pfunc, *pcut, *pcor);
}

void
g5_set_eta_(double *eta)
{
    g5_set_eta(*eta);
}


/*
 * primitive functions to control individual cards.
 * the user needs to specify card's device id explicitly.
 */

void
g5_openMC_(int *devid)
{
  g5_openMC(*devid);
}

void
g5_closeMC_(int *devid)
{
  g5_closeMC(*devid);
}

/*
double
g5_get_pcibus_freqMC_(int *devid)
{
    return g5_get_pcibus_freqMC(*devid);
}
*/

/*
void
g5_get_rangeMC_(int *devid, double *xmin, double *xmax, double *mmin)
{
    g5_get_rangeMC(*devid, xmin, xmax, mmin);
}
*/

void
g5_set_rangeMC_(int *devid, double *xmin, double *xmax, double *mmin)
{
  g5_set_rangeMC(*devid, *xmin, *xmax, *mmin);
}

void
g5_set_jpMC_(int *devid, int *adr, int *nj, double *mj, double (*xj)[3])
{
    g5_set_jpMC(*devid, *adr, *nj, mj, xj);
}

/*
void
g5_set_xmjMC_(int *devid, int *adr, int *nj, double (*xj)[3], double *mj)
{
    g5_set_xmjMC(*devid, *adr, *nj, xj, mj);
}
*/

void
g5_set_xiMC_(int *devid, int *ni, double (*xi)[3])
{
  g5_set_xiMC(*devid, *ni, xi);
}

void
g5_runMC_(int *devid)
{
  g5_runMC(*devid);
}

void
g5_set_nMC_(int *devid, int *n)
{
  g5_set_nMC(*devid, *n);
}

/*
void
g5_set_epsMC_(int *devid, int *ni, double *eps)
{
  g5_set_epsMC(*devid, *ni, eps);
}
*/

/*
void
g5_set_eps2MC_(int *devid, int *ni, double *eps2)
{
  g5_set_eps2MC(*devid, *ni, eps2);
}
*/

/*
void
g5_set_eps_to_allMC_(int *devid, double *eps)
{
  g5_set_eps_to_allMC(*devid, *eps);
}
*/

void
g5_set_eps2_to_allMC_(int *devid, double *eps2)
{
  g5_set_eps2_to_allMC(*devid, *eps2);
}

void
g5_get_forceMC_(int *devid, int *ni, double (*a)[3], double *p)
{
  g5_get_forceMC(*devid, *ni, a, p);
}

int
g5_get_number_of_pipelinesMC_(int *devid)
{
    return g5_get_number_of_pipelinesMC(*devid);
}

/*
int
g5_get_number_of_real_pipelinesMC_(int *devid)
{
    return g5_get_number_of_real_pipelinesMC(*devid);
}
*/

int
g5_get_jmemsizeMC_(int *devid)
{
    return g5_get_jmemsizeMC(*devid);
}

void
g5_set_cutoff_tableMC_(int *devid, double (*ffunc)(double), double *fcut, double *fcor,
                       double (*pfunc)(double), double *pcut, double *pcor)
{
    g5_set_cutoff_tableMC(*devid, ffunc, *fcut, *fcor, pfunc, *pcut, *pcor);
}

void
g5_set_etaMC_(int *devid, double *eta)
{
    g5_set_etaMC(*devid, *eta);
}


/*
 * include definitions of library functions specific to G5nbPIPE,
 * if defined a constant 'G5NBUTIL'.
 */

#ifdef G5NBUTIL
#include "g5nbfapi.c"
#endif
