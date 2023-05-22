# Marine Reserves

This repository contains the C code implementing the model analysed in the manuscript:

**Marine reserves promote cycles in fish populations on ecological and evolutionary time scales**

by

**Renfei Chen, Catalina Chaparro-Pedraza, Suping Xiao, Pu Jia, Quan-Xing Liu & André M. de Roos**

In addition to these model files, this repository also contains files with the parameter settings and the initial conditions that are used for producing the figures included in the manuscript, the data files generated with simulations and used for producing the figures and the R scripts that generate the figures.

## Repository contents

- **EBT/ReserveHabSwitch-qc.c** \
  Model implementation for numerical simulation using the *Escalator Boxcar Train* (EBT) approach. See [the EBTtool webpage](https://staff.fnwi.uva.nl/a.m.deroos/EBT/index.html) for a description and the EBT software package.

- **EBT/ReserveHabSwitch-qc.h** \
  Header file defining the dimensions of the model, including the number of environmental variables, the number of populations and the number of state variables characterising individuals

- **EBT/Makefile** \
  Simple Makefile to produce the executables `ReserveHabSwitch-qc` (for computing model dynamics) and `ReserveHabSwitch-qc_bif` (for performing numerical simulations for bifurcation analysis)

- **EBT/Default.cvf** \
  File with default values of the model parameters and the numerical settings needed for simulations.

- **EBT/Default.isf** \
  File with the default initial state of the model for simulations.

- **Equi/HarvestWithReserve.h** \
  Model implementation for computing ecological steady states and evolutionary stable states of the model using the 
  R package [PSPManalysis](https://cran.r-project.org/package=PSPManalysis).

- **Figures/Figure2.R** \
  R script to produce Figure 2 in the main text of the manuscript

- **Figures/Figure3.R** \
  R script to produce Figure 3 in the main text of the manuscript

- **Figures/Figure4.R** \
  R script to produce Figure 4 in the main text of the manuscript

- **Figures/FigureS1.R** \
  R script to produce Figure 1 in the supplementary information of the manuscript

- **Figures/FigureS2.R** \
  R script to produce Figure 2 in the supplementary information of the manuscript

- **Figures/FigureS3.R** \
  R script to produce Figure 3 in the supplementary information of the manuscript

- **Figures/maintextfigure2.pdf** \
  PDF file of Figure 2 in the main text of the manuscript

- **Figures/maintextfigure3.pdf** \
  PDF file of Figure 3 in the main text of the manuscript

- **Figures/maintextfigure4.pdf** \
  PDF file of Figure 4 in the main text of the manuscript

- **Figures/appendixFigS1.pdf** \
  PDF file of Figure 1 in the supplementary information of the manuscript

- **Figures/appendixFigS2.pdf** \
  PDF file of Figure 2 in the supplementary information of the manuscript

- **Figures/appendixFigS3.pdf** \
  PDF file of Figure 3 in the supplementary information of the manuscript

- **Figures/EBToutput/** \
  This directory contains the files with the values of the model parameters and numerical settings (files with `.cvf` extension) and the initial state of the model (files with `.isf` extension) that are used by the EBT programs `ReserveHabSwitch-qc`, for computing results in Figure 2 and 4 in the main text and all figures in the supplementary information, and by `ReserveHabSwitch-qc_bif`, for computing the bifurcation results (minimum and maximum values during cylces) in Figure 3. Because some of these model simulations take quite a long time also the data files are included (files with `.out` extension) that are generated during these model simulations and that are used for producing the figures included in the manuscript.

*****
