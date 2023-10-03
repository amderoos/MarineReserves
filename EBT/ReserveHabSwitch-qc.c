/***
 NAME
    ReserveHabSwitch-qc
 DESCRIPTION
    EBT implementation of a simple model for cohort competition, in which
    individuals switch between foraging on resource 1 in habitat 1 to resource 2
    in habitat 2 on reaching size ws. In habitat 2, individulas experience fishing
    mortality besides background mortality.
    The model implements a quantitative genetics approach focussed on the
    evolution of the size at habitat switch. The average size at habitat switch
    among newborn offspring is related to selection differential and the
    heritability.
 
 Last modification: AMdR - Oct 02, 2023
 ***/
#include  "escbox.h"

#define    CONS                   0


/*
 *==========================================================================
 *
 *        LABELLING ENVIRONMENT AND I-STATE VARIABLES
 *
 *==========================================================================
 */
#define time                      (env[0])
#define F1                        (env[1])
#define F2                        (env[2])
#define F3                        (env[3])

#define ADULTS                    (env[4])
#define ADULTWS                   (env[5])
#define ADULTFEC                  (env[6])
#define ADULTFECWS                (env[7])

#define JUVHARVEST                (env[8])
#define ADUHARVEST                (env[9])

#define lnsurvival                (0)
#define age                       (i_state( 0))
#define size                      (i_state( 1))

#define IDnumber                  (i_const( 0))
#define IDsmoltsize               (i_const( 1))
#define IDnbegin                  (i_const( 2))
#define IDmatured                 (i_const( 3))
#define IDsmolted                 (i_const( 4))
#define IDagematur                (i_const( 5))
#define IDagesmolt                (i_const( 6))
#define IDreserve                 (i_const( 7))                                     //0 if this individual is out of reserve 1 if it is in reserve
#define IDcohortID                (i_const( 8))

#define ismature(n)               (lrint(popIDcard[CONS][n][IDmatured]) > 0)
#define issmolted(n)              (lrint(popIDcard[CONS][n][IDsmolted]) > 0)
#define isdead(n)                 (((pop[CONS][n][lnsurvival] - 1.0) > LogMinSurvival) || (popIDcard[CONS][n][IDnumber] < MIN_COHORT_SIZE))


/*
 *==========================================================================
 *
 *        DEFINING AND LABELLING CONSTANTS AND PARAMETERS
 *
 *==========================================================================
 */

#define RHO1                      parameter[ 0]                                     // Productivity ratio resource 1 and 2
#define RHO2                      parameter[ 1]                                     // Productivity ratio resource 1 and 3
#define DELTA                     parameter[ 2]                                     // Scaled resource turn-over rate

#define ETA1                      parameter[ 3]                                     // Scaled background mortality rate small consumers
#define ETA23                     parameter[ 4]                                     // Scaled background mortality rate large consumers inside & outside reserve
#define ETA2                      parameter[ 5]                                     // Additional scaled background mortality rate large consumers outside reserve
#define ETA3                      parameter[ 6]                                     // Additional scaled background mortality rate large consumers in reserve

#define ETS                       parameter[ 7]                                     // Scaled fishing mortality rate of juveniles & adults in habitat 2 (instant mortality of fishing if permanently open)
#define ETSJ                      parameter[ 8]                                     // Additional scaled fishing mortality rate of juveniles in habitat 2 (instant mortality of fishing if permanently open)
#define ETSA                      parameter[ 9]                                     // Additional scaled fishing mortality rate of adults in habitat 2 (instant mortality of fishing if permanently open)

#define WS                        parameter[10]                                     // Default switch size (scaled)
#define Q                         parameter[11]                                     // Attack rate ratio on resource 1 and 2
#define BETA                      parameter[12]                                     // Scaled fecundity parameter
#define SIZERESERVE               parameter[13]                                     // Fraction of large consumers in reserve

#define HERITABILITY              parameter[14]                                     // Heritability of trait value (0.3)
#define GENETICVARIANCE           parameter[15]                                     // Half-width of the truncated normal distribution (0.25)

#define SEASON                    parameter[16]                                     // Duration of a season, i.e. one cycle of open and closed for fishing
#define FISHINGCLOSED             parameter[17]                                     // FRACTION of the season during which the fishing area is closed for fishing
#define TRESEST                   parameter[18]                                     // Time at which reserve is established

#define MIN_SURVIVAL              1.0E-8
#define MIN_COHORT_SIZE           1.0E-9
#define SUBCOHORTS                11                                                // Central one, 5 lower, 5 higher


/*
 *==========================================================================
 *
 *        OTHER DEFINITIONS
 *
 *==========================================================================
 */

static long int                   MaxCohortID = 0L;
static int                        MaturingCohort, SmoltingCohort;
static int                        SubCohorts;
static double *                   subcohortfrac;
static double                     LogMinSurvival;
static int                        ReserveEstablished;
static double                     InitialSmoltSize;
static double                     LastResetTime;


/*
 *==========================================================================
 *
 * USER INITIALIZATION ROUTINE ALLOWS OPERATIONS ON INITIAL POPULATIONS
 *
 *==========================================================================
 */

void    UserInit(int argc, char **argv, double *env, population *pop)

{
  int    i;
  double yval, cumuldist, sumtotnum = 0.0, sumsizesmolt = 0.0;

  switch (argc)
    {
      case 3:
        TRESEST = atof(argv[2]);
      default:
        break;
    }

  LogMinSurvival = -log(MIN_SURVIVAL);
  for (i = 0; i < cohort_no[CONS]; i++)
    {
      popIDcard[CONS][i][IDcohortID] = MaxCohortID;
      MaxCohortID++;

      if ((popIDcard[CONS][i][IDnbegin] > 0.9 * MISSING_VALUE) || (popIDcard[CONS][i][IDnbegin] < popIDcard[CONS][i][IDnumber]))
        popIDcard[CONS][i][IDnbegin] = popIDcard[CONS][i][IDnumber];

      if ((pop[CONS][i][lnsurvival] > 0.9 * MISSING_VALUE) || (pop[CONS][i][lnsurvival] < 0))
        pop[CONS][i][lnsurvival] = -log(popIDcard[CONS][i][IDnumber] / popIDcard[CONS][i][IDnbegin]);

      popIDcard[CONS][i][IDnumber] = exp(-(pop[CONS][i][lnsurvival] - 1.0)) * popIDcard[CONS][i][IDnbegin];

      if (popIDcard[CONS][i][IDsmoltsize] > 0.9 * MISSING_VALUE) popIDcard[CONS][i][IDsmoltsize] = WS;

      if ((pop[CONS][i][size] >= 1.0) || isequal(pop[CONS][i][size], 1.0))
        {
          popIDcard[CONS][i][IDmatured] = 1.0;
        }
      else
        {
          popIDcard[CONS][i][IDmatured] = 0.0;
        }

      if ((pop[CONS][i][size] >= popIDcard[CONS][i][IDsmoltsize]) || isequal(pop[CONS][i][size] / popIDcard[CONS][i][IDsmoltsize], 1.0))
        {
          popIDcard[CONS][i][IDsmolted] = 1.0;
        }
      else
        {
          popIDcard[CONS][i][IDsmolted] = 0.0;
        }

            // remove dead cohorts
      if (isdead(i)) pop[CONS][i][lnsurvival] = 0.0;

      sumtotnum    += popIDcard[CONS][i][IDnumber];
      sumsizesmolt += popIDcard[CONS][i][IDsmoltsize] * popIDcard[CONS][i][IDnumber];
    }
  SievePop();
  
  if (iszero(GENETICVARIANCE))
    {
      SubCohorts = 1;
      subcohortfrac = (double *)malloc(SubCohorts * sizeof(double));
      subcohortfrac[0] = 1.0;
      InitialSmoltSize = WS;
    }
  else
    {
      SubCohorts    = SUBCOHORTS;
      subcohortfrac = (double *)malloc(SubCohorts * sizeof(double));
      cumuldist     = 0.0;
      for (i = 0; i < (SubCohorts - 1); i++)
        {
          yval = (i + 1) * (3.0 / SubCohorts);
          if (yval < 1)
            subcohortfrac[i] = yval * yval * yval / 6.0;
          else if (yval < 2)
            subcohortfrac[i] = -1.5 * yval + 1.5 * yval * yval - yval * yval * yval / 3.0 + 0.5;
          else
            subcohortfrac[i] = 4.5 * yval - 1.5 * yval * yval + yval * yval * yval / 6.0 - 3.5;

          subcohortfrac[i] -= cumuldist;
          cumuldist += subcohortfrac[i];
        }
      subcohortfrac[SubCohorts - 1] = 1.0 - cumuldist;
      InitialSmoltSize = sumsizesmolt / sumtotnum;
    }

#if (BIFURCATION == 0)
  double maxsmoltsize = (1 + GENETICVARIANCE) * InitialSmoltSize;
  double smoltsizedif = 2 * GENETICVARIANCE * InitialSmoltSize / SubCohorts;
  for (i = 0; i < SubCohorts; i++) 
    {
      printf("\n%.8f\t%.8f", max(0.0, maxsmoltsize - ((i % SubCohorts) + 0.5) * smoltsizedif), subcohortfrac[i]);
    }
  printf("\n");
  fflush(stdout);
#endif

  ReserveEstablished = ((env[0] > TRESEST) || isequal(env[0], TRESEST)) && (!iszero(SIZERESERVE));

  if (!ReserveEstablished) cohort_limit *= 0.5;

  return;
}


/*
 *==========================================================================
 *
 *    SPECIFICATION OF THE NUMBER AND VALUES OF BOUNDARY POINTS
 *
 *==========================================================================
 */

void    SetBpointNo(double *env, population *pop, int *bpoint_no)

{
#if (BIFURCATION == 1)
  if (iszero(GENETICVARIANCE))
  {
    InitialSmoltSize = WS;
  }
#endif

  // If the reserve is not established yet, establish it at the right time
  if ((!ReserveEstablished) && (!iszero(SIZERESERVE)))
    {
      if ((env[0] > TRESEST) || isequal(env[0], TRESEST))
        {
          int                     ii, newcoh;

          for (ii = 0; ii < cohort_no[CONS]; ii++)
            {
              if ((!issmolted(ii)) || (!iszero(popIDcard[CONS][ii][IDreserve]))) continue;

              // Code copied from the Arctic Char source code
              newcoh = AddCohorts(pop, CONS, 1);

              // Copy each cohort and its IDcard to the position on top of it
              for (; newcoh > ii; newcoh--)
                {
                  memcpy(pop[CONS][newcoh],       pop[CONS][newcoh - 1],       COHORT_SIZE * sizeof(double));
                  memcpy(popIDcard[CONS][newcoh], popIDcard[CONS][newcoh - 1], I_CONST_DIM * sizeof(double));
                }

              // Cohort ii and ii+1 should now be exact copies if each other
              // Subcohort in fished area
              popIDcard[CONS][ii][IDnumber]  *= (1 - SIZERESERVE);
              popIDcard[CONS][ii][IDnbegin]  *= (1 - SIZERESERVE);
              popIDcard[CONS][ii][IDreserve]  = 0;

              // Subcohort in reserve
              popIDcard[CONS][ii + 1][IDnumber]   *= SIZERESERVE;
              popIDcard[CONS][ii + 1][IDnbegin]   *= SIZERESERVE;
              popIDcard[CONS][ii + 1][IDcohortID]  = MaxCohortID++;
              popIDcard[CONS][ii + 1][IDreserve]   = 1;
            }

          cohort_limit *= 2.0;
          ReserveEstablished = 1;
        }
    }

  // Create all the sub-cohorts as new boundary cohorts
  bpoint_no[CONS] = (1 + ReserveEstablished) * SubCohorts;

  ADULTS = ADULTWS = ADULTFEC = ADULTFECWS = 1.0;

  return;
}


/*==========================================================================*/

void    SetBpoints(double *env, population *pop, population *bpoints)

{
  register int i;

  // All sub-cohorts have identical i-state at birth
  // The size at smolting will be set when the boundary cohort is closed off
  for (i = 0; i < bpoint_no[CONS]; i++)
    {
      bpoints[CONS][i][age]  = 0.0;
      bpoints[CONS][i][size] = 0.0;
    }

  return;
}


/*
 *==========================================================================
 *
 *            SPECIFICATION OF DERIVATIVES
 *
 *==========================================================================
 */

void    Gradient(double *env, population *pop, population *ofs, double *envgrad, population *popgrad, population *ofsgrad, population *bpoints)

{
  register int i;
  double       juveniles1 = 0.0, juveniles2 = 0.0, juveniles3 = 0.0, fracAdultHab, TotBirthRate;
  double       adults2 = 0.0, adults3 = 0.0, adultws2 = 0.0, adultws3 = 0.0;
  double       juvharvmort, aduharvmort;
  double       timeinseason;

  memset(envgrad, 0, ENVIRON_DIM * sizeof(double));

  // The goal is to keep the survival throughout a season the same, irrespective of the value of
  // FISHINGCLOSED that represent the FRACTION of the season that fishing in the fished area is
  // prohibited. An entire season hence consists of a cycle of a period during which fishing is
  // prohibited (coming first) and a period that fishing is allowed
  timeinseason     = fmod(time, SEASON);
  juvharvmort      = (ETS + ETSJ) / (1.0 - FISHINGCLOSED);
  aduharvmort      = (ETS + ETSA) / (1.0 - FISHINGCLOSED);

  // If there is no part of the season during which fishing is prohibited (FISHINGCLOSED == 0) the probability to 
  // escape fishing mortality throughout the entire season would equal exp(- f0 * S) where f0 is the 
  // instantaneous fishing mortality rate (comparable with the parameters (ETS + ETSJ) and (ETS + ETSA) in the absence of a reserve)
  // and S is the season length.
  // If a fraction c of the season is closed for fishing the probability to escape fishing mortality 
  // throughout the part of the season that fishing is allowed would equal exp(- f1 * (1 - c) * S)  where f1 is the 
  // adjusted instantaneous fishing mortality rate during that part of the season that fishing is allowed. 
  // The probability to escape fishing mortality throughout the entire season can therefore be kept equal by adopting 
  // f1 = f0 / (1 - c) 

  for (i = 0; i < cohort_no[CONS]; i++)
    {
      popIDcard[CONS][i][IDnumber] = exp(-(pop[CONS][i][lnsurvival] - 1.0)) * popIDcard[CONS][i][IDnbegin];
      if (issmolted(i))
        {
          if (!iszero(popIDcard[CONS][i][IDreserve]))
            {
              popgrad[CONS][i][number] = (ETA23 + ETA3);
              popgrad[CONS][i][size]   = Q * F3;
              popgrad[CONS][i][age]    = 1.0;
              if (isdead(i)) continue;
              if (ismature(i))
                {
                  adults3  += popIDcard[CONS][i][IDnumber];
                  adultws3 += popIDcard[CONS][i][IDsmoltsize] * popIDcard[CONS][i][IDnumber];
                }
              else
                juveniles3 += popIDcard[CONS][i][IDnumber];
            }
          else
            {
              if ((!iszero(1.0 - FISHINGCLOSED)) && (timeinseason > (FISHINGCLOSED * SEASON)))
                {
                  if (ismature(i))
                    {
                      popgrad[CONS][i][number] = (ETA23 + ETA2) + aduharvmort;
                      envgrad[9] += aduharvmort * pop[CONS][i][size] * popIDcard[CONS][i][IDnumber];
                    }
                  else
                    {
                      popgrad[CONS][i][number] = (ETA23 + ETA2) + juvharvmort;
                      envgrad[8] += juvharvmort * pop[CONS][i][size] * popIDcard[CONS][i][IDnumber];
                    }
                }
              else
                {
                  popgrad[CONS][i][number] = (ETA23 + ETA2);
                }
              popgrad[CONS][i][size] = Q * F2;
              popgrad[CONS][i][age]  = 1.0;
              if (isdead(i)) continue;
              if (ismature(i))
                {
                  adults2  += popIDcard[CONS][i][IDnumber];
                  adultws2 += popIDcard[CONS][i][IDsmoltsize] * popIDcard[CONS][i][IDnumber];
                }
              else
                juveniles2 += popIDcard[CONS][i][IDnumber];
            }
        }
      else
        {
          popgrad[CONS][i][number] = ETA1;
          popgrad[CONS][i][size]   = F1;
          popgrad[CONS][i][age]    = 1.0;

          if (isdead(i)) continue;

          juveniles1 += popIDcard[CONS][i][IDnumber];
        }
    }

  TotBirthRate = BETA * Q * (F2 * adults2 + F3 * adults3);
  for (i = 0; i < bpoint_no[CONS]; i++)
    {
      // A fraction subcohortfrac[i] of the produced offspring ends up in sub-cohort i

      // There are two sets of offspring cohorts. The first set of cohorts with index
      // 0,...., (SubCohorts - 1) will eventually end up in the fished area, the
      // second set of cohorts with index SubCohorts,...., (2 * SubCohorts - 1)
      // will eventually end up in the reserve
      if (!ReserveEstablished)
        {
          fracAdultHab = subcohortfrac[(i % SubCohorts)];
        }
      else
        {
          fracAdultHab  = (i < SubCohorts) ? (1 - SIZERESERVE) : SIZERESERVE;
          fracAdultHab *= subcohortfrac[(i % SubCohorts)];
        }

      ofsgrad[CONS][i][number] = -ETA1 * ofs[CONS][i][number] + fracAdultHab * TotBirthRate;
      ofsgrad[CONS][i][age]    = -ETA1 * ofs[CONS][i][age] + ofs[CONS][i][number];
      ofsgrad[CONS][i][size]   = -ETA1 * ofs[CONS][i][size] + F1 * ofs[CONS][i][number];

      juveniles1 += ofs[CONS][i][number];
    }

  envgrad[0] = 1.0;
  envgrad[1] = 1.0  - DELTA * F1 - F1 * juveniles1;
  if (ReserveEstablished)
    {
      if (iszero(1 - SIZERESERVE)) 
        {
          envgrad[2] = RHO1 - DELTA * F2;
          envgrad[3] = RHO2 - DELTA * F3 - Q * F3 * (juveniles3 + adults3);
        }
      else
        {
          envgrad[2] = RHO1 - DELTA * F2 - Q * F2 * (juveniles2 + adults2) / (1 - SIZERESERVE);
          envgrad[3] = RHO2 - DELTA * F3 - Q * F3 * (juveniles3 + adults3) / SIZERESERVE;
        }
    }
  else
    {
      envgrad[2] = RHO1 - DELTA * F2 - Q * F2 * (juveniles2 + adults2);
      envgrad[3] = RHO2 - DELTA * F3;
    }

  envgrad[4] = adults2  + adults3;
  envgrad[5] = adultws2 + adultws3;
  envgrad[6] = TotBirthRate;
  envgrad[7] = BETA * Q * (F2 * adultws2 + F3 * adultws3);

  return;
}


/*
 *==========================================================================
 *
 *    SPECIFICATION OF EVENT LOCATION AND DYNAMIC COHORT CLOSURE
 *
 *==========================================================================
 */

void    EventLocation(double *env, population *pop, population *ofs, population *bpoints, double *events)

{
  int i;

  events[0] = -1.0;
  events[1] = -1.0;

  for (i = 0; i < cohort_no[CONS]; i++)
    {
      if (isdead(i)) continue;

      if (!ismature(i))
        {
          if ((pop[CONS][i][size] - 1.0) > events[0])
            {
              events[0]      = (pop[CONS][i][size] - 1.0);
              MaturingCohort = i;
            }
        }

      if (!issmolted(i))
        {
          if ((pop[CONS][i][size] / popIDcard[CONS][i][IDsmoltsize] - 1.0) > events[1])
            {
              events[1]      = (pop[CONS][i][size] / popIDcard[CONS][i][IDsmoltsize] - 1.0);
              SmoltingCohort = i;
            }
        }
    }

  return;
}


/*==============================================================================*/

int    ForceCohortEnd(double *env, population *pop, population *ofs, population *bpoints)

{
  int i;
#if (DEBUG)
  char tmpstr[256];
#endif

  for (i = 0; i < cohort_no[CONS]; i++)
    {
      if (isdead(i) || (ismature(i) && issmolted(i)))
        continue;                                                                   // If the cohort is already mature and smolted, continue to next i value.

      if (!ismature(i))
        {
          if (((LocatedEvent == 0) && (i == MaturingCohort)) || (pop[CONS][i][size] >= 1.0) || isequal(pop[CONS][i][size] / 1.0, 1.0))
            {
#if (DEBUG)
              if (i < MaturingCohort)
                {
                  sprintf(tmpstr, "ForceCohortEnd(): Cohort #%d is maturing at t = %.6f with size W = %.5f but did not trigger a maturation event!",
                          i, env[0], pop[CONS][i][size]);
                  Warning(tmpstr);
                }
#endif
              popIDcard[CONS][i][IDmatured]  = 1.0;
              popIDcard[CONS][i][IDagematur] = pop[CONS][i][age];

              // Force individuals to smolt even if they have not reached the smolting size yet
              if (!issmolted(i))
                {
                  popIDcard[CONS][i][IDsmolted]  = 1.0;
                  popIDcard[CONS][i][IDagesmolt] = pop[CONS][i][age];
                }
            }
        }

      if (!issmolted(i))
        {
          if (((LocatedEvent == 1) && (i == SmoltingCohort)) || (pop[CONS][i][size] >= popIDcard[CONS][i][IDsmoltsize]) ||
              isequal(pop[CONS][i][size] / popIDcard[CONS][i][IDsmoltsize], 1.0))
            {
#if (DEBUG)
              if (i < SmoltingCohort)
                {
                  sprintf(
                      tmpstr,
                      "ForceCohortEnd(): Cohort #%d is smolting at t = %.6f with size W = %.5f (target: %.5f) but did not trigger a smolting event!",
                      i, env[0], pop[CONS][i][size], popIDcard[CONS][i][IDsmoltsize]);
                  Warning(tmpstr);
                }
#endif
              popIDcard[CONS][i][IDsmolted]  = 1.0;
              popIDcard[CONS][i][IDagesmolt] = pop[CONS][i][age];
            }
        }
    }
  MaturingCohort = SmoltingCohort = -1;

  return NO_COHORT_END;
}


/*
 *==========================================================================
 *
 *        SPECIFICATION OF BETWEEN COHORT CYCLE DYNAMICS
 *
 *==========================================================================
 */

#define DOWARNINGS                0

void    InstantDynamics(double *env, population *pop, population *ofs)

{
  register int i;
  double       SelDif, meanparsize, meanofssize, newsmoltsize, maxsmoltsize, minsmoltsize, smoltsizedif;
#if (DOWARNINGS == 1)
  char         tmpstr[256];
#endif

  for (i = 0; i < cohort_no[CONS]; i++)
    {
      // remove dead cohorts
      if (isdead(i)) pop[CONS][i][lnsurvival] = 0.0;

      if ((!ismature(i)) && ((pop[CONS][i][size] >= 1.0) || isequal(pop[CONS][i][size] / 1.0, 1.0)))
        {
#if (DOWARNINGS == 1)
          sprintf(tmpstr, "InstantDynamics(): Cohort #%d is maturing at t = %.6f with size W = %.5f but did not trigger a maturation event!",
                  i, env[0], pop[CONS][i][size]);
          Warning(tmpstr);
#endif

          popIDcard[CONS][i][IDmatured]  = 1.0;
          popIDcard[CONS][i][IDagematur] = pop[CONS][i][age];

          // Force individuals to smolt even if they have not reached the smolting size yet
          if (!issmolted(i))
            {
              popIDcard[CONS][i][IDsmolted]  = 1.0;
              popIDcard[CONS][i][IDagesmolt] = pop[CONS][i][age];
            }
        }

      if ((!issmolted(i)) &&
          ((pop[CONS][i][size] >= popIDcard[CONS][i][IDsmoltsize]) || isequal(pop[CONS][i][size] / popIDcard[CONS][i][IDsmoltsize], 1.0)))
        {
#if (DOWARNINGS == 1)
          sprintf(tmpstr, "InstantDynamics(): Cohort #%d is smolting at t = %.6f with size W = %.5f (target: %.5f) but did not trigger a smolting event!",
                  i, env[0], pop[CONS][i][size], popIDcard[CONS][i][IDsmoltsize]);
          Warning(tmpstr);
#endif

          popIDcard[CONS][i][IDsmolted]  = 1.0;
          popIDcard[CONS][i][IDagesmolt] = pop[CONS][i][age];
        }
    }

  ADULTS     -= 1.0; 
  ADULTWS    -= 1.0;
  ADULTFEC   -= 1.0;
  ADULTFECWS -= 1.0;

  if (ADULTFEC && ADULTS)
    {
      if (!iszero(HERITABILITY))
        {
          meanparsize  = ADULTWS / ADULTS;
          meanofssize  = ADULTFECWS / ADULTFEC;
          SelDif       = HERITABILITY * (meanofssize - meanparsize);
          newsmoltsize = meanparsize + SelDif;
        }
      else
        newsmoltsize = InitialSmoltSize;

      maxsmoltsize = min((1 + GENETICVARIANCE) * newsmoltsize, 0.95);
      minsmoltsize = max((1 - GENETICVARIANCE) * newsmoltsize, 0.05);
      smoltsizedif = (maxsmoltsize - minsmoltsize) / SubCohorts;

      for (i = 0; i < bpoint_no[CONS]; i++)
        {
          ofsIDcard[CONS][i][IDnumber]    = ofs[CONS][i][number];
          ofsIDcard[CONS][i][IDnbegin]    = ofs[CONS][i][number];
          ofsIDcard[CONS][i][IDmatured]   = 0.0;
          ofsIDcard[CONS][i][IDsmolted]   = 0.0;
          ofsIDcard[CONS][i][IDsmoltsize] = max(0.05, maxsmoltsize - ((i % SubCohorts) + 0.5) * smoltsizedif);
          ofsIDcard[CONS][i][IDagematur]  = 0.0;
          ofsIDcard[CONS][i][IDagesmolt]  = 0.0;
          ofsIDcard[CONS][i][IDreserve]   = (i >= SubCohorts);
          ofsIDcard[CONS][i][IDcohortID]  = MaxCohortID;
          MaxCohortID++;

          ofs[CONS][i][age] += i * 1.0E-5;
          ofs[CONS][i][lnsurvival] = 1.0;
        }
    }
  else  
    {
      for (i = 0; i < bpoint_no[CONS]; i++)
        {
          ofsIDcard[CONS][i][IDnumber]    = 0.0;
          ofsIDcard[CONS][i][IDnbegin]    = 0.0;
          ofsIDcard[CONS][i][IDmatured]   = 0.0;
          ofsIDcard[CONS][i][IDsmolted]   = 0.0;
          ofsIDcard[CONS][i][IDsmoltsize] = 0.0;
          ofsIDcard[CONS][i][IDagematur]  = 0.0;
          ofsIDcard[CONS][i][IDagesmolt]  = 0.0;
          ofsIDcard[CONS][i][IDreserve]   = 0.0;
          ofsIDcard[CONS][i][IDcohortID]  = 0.0;
          ofs[CONS][i][age]               = 0.0;
          ofs[CONS][i][lnsurvival]        = 0.0;
        }
    }

  return;
}


/*
 *==========================================================================
 *
 *            SPECIFICATION OF OUTPUT VARIABLES
 *
 *==========================================================================
 */

void    DefineOutput(double *env, population *pop, double *output)

/*
 * OUTPUT VARIABLES:
 * [ 0]     2: Resource 1
 * [ 1]     3: Resource 2
 * [ 2]     4: Resource 3
 * [ 3]     5: Total number
 * [ 4]     6: Total biomass
 * [ 5]     7: Total number in habitat 1
 * [ 6]     8: Total biomass in habitat 1
 * [ 7]     9: Total number in habitat 2
 * [ 8]    10: Total biomass in habitat 2
 * [ 9]    11: Total number in habitat 3
 * [10]    12: Total biomass in habitat 3
 * [11]    13: Total number of juveniles in habitat 2
 * [12]    14: Total biomass of juveniles in habitat 2
 * [13]    15: Total number of juveniles in habitat 3
 * [14]    16: Total biomass of juveniles in habitat 3
 * [15]    17: Total number of adults in habitat 2
 * [16]    18: Total biomass of adults in habitat 2
 * [17]    19: Total number of adults in habitat 3
 * [18]    20: Total biomass of adults in habitat 3
 * [19]    21: Average smolt size in the population
 * [20]    22: Total population birth rate
 * [21]    23: Number of cohorts
 * [22]    24: Biomass yield per unit time of juvenile harvesting
 * [23]    25: Biomass yield per unit time of adult harvesting
 * [24]    26: Biomass yield per unit time of total harvesting
 */

{
  register int                    i, outnr = 0;
  static int                      first = 1;
  double                          juveniles1 = 0.0, juveniles2 = 0.0, juveniles3 = 0.0, adults2 = 0.0, adults3 = 0.0, sumsizesmolt = 0.0;
  double                          juveniles1bio = 0.0, juveniles2bio = 0.0, juveniles3bio = 0.0, adults2bio = 0.0, adults3bio = 0.0;

  for (i = 0; i < cohort_no[CONS]; i++)
    {
      popIDcard[CONS][i][IDnumber] = exp(-(pop[CONS][i][lnsurvival] - 1.0)) * popIDcard[CONS][i][IDnbegin];
      sumsizesmolt += popIDcard[CONS][i][IDsmoltsize] * popIDcard[CONS][i][IDnumber];

      if (issmolted(i))
        {
          if (!iszero(popIDcard[CONS][i][IDreserve]))
            {
              if (ismature(i))
                {
                  adults3 += popIDcard[CONS][i][IDnumber];
                  adults3bio += pop[CONS][i][size] * popIDcard[CONS][i][IDnumber];
                }
              else
                {
                  juveniles3 += popIDcard[CONS][i][IDnumber];
                  juveniles3bio += pop[CONS][i][size] * popIDcard[CONS][i][IDnumber];
                }
            }
          else
            {
              if (ismature(i))
                {
                  adults2 += popIDcard[CONS][i][IDnumber];
                  adults2bio += pop[CONS][i][size] * popIDcard[CONS][i][IDnumber];
                }
              else
                {
                  juveniles2 += popIDcard[CONS][i][IDnumber];
                  juveniles2bio += pop[CONS][i][size] * popIDcard[CONS][i][IDnumber];
                }
            }
        }
      else
        {
          juveniles1 += popIDcard[CONS][i][IDnumber];
          juveniles1bio += pop[CONS][i][size] * popIDcard[CONS][i][IDnumber];
        }
    }

  output[outnr++] = F1;
  output[outnr++] = F2;
  output[outnr++] = F3;
  output[outnr++] = juveniles1 + juveniles2 + juveniles3 + adults2 + adults3;
  output[outnr++] = juveniles1bio + juveniles2bio + juveniles3bio + adults2bio + adults3bio;
  output[outnr++] = juveniles1;
  output[outnr++] = juveniles1bio;
  output[outnr++] = juveniles2 + adults2;
  output[outnr++] = juveniles2bio + adults2bio;
  output[outnr++] = juveniles3 + adults3;
  output[outnr++] = juveniles3bio + adults3bio;
  output[outnr++] = juveniles2;
  output[outnr++] = juveniles2bio;
  output[outnr++] = juveniles3;
  output[outnr++] = juveniles3bio;
  output[outnr++] = adults2;
  output[outnr++] = adults2bio;
  output[outnr++] = adults3;
  output[outnr++] = adults3bio;
  output[outnr++] = sumsizesmolt / (juveniles1 + juveniles2 + juveniles3 + adults2 + adults3);
  output[outnr++] = BETA * Q * (F2 * adults2 + F3 * adults3);
  output[outnr++] = cohort_no[CONS];

  if (first)
    {
      output[outnr++] = (ETS + ETSJ) * juveniles2bio;
      output[outnr++] = (ETS + ETSA) * adults2bio;
      output[outnr++] = (ETS + ETSJ) * juveniles2bio + (ETS + ETSA) * adults2bio;
    }
  else
    {
      output[outnr++] = JUVHARVEST / (time - LastResetTime);
      output[outnr++] = ADUHARVEST / (time - LastResetTime);
      output[outnr++] = (JUVHARVEST + ADUHARVEST) / (time - LastResetTime);
    }
  first         = 0;
  LastResetTime = time;
  JUVHARVEST    = 0.0;
  ADUHARVEST    = 0.0;

  return;
}
