# Influenza A H3N2 virus phylogeography, Kilifi, Kenya, 2015-2016

Author:	David Collins Owuor

Institution:	KEMRI-Wellcome Trust Research Programme, Kilifi, Kenya

Date Published: 30 August 2020

## Introduction

This repository contains data and input files for the phylogeographical analysis of
**influenza A H3N2 virus from Kilifi, Kenya, 2015-2016.**

## General instructions

The data and instructions will allow replication of the results in my PhD thesis available
at http://oro.open.ac.uk/75472/; Chapter 3 - Phylogeography of Influenza A(H3N2) Virus in
Kilifi, Kenya, 2015-2016.

## Data

This folder contains next generation sequencing (NGS) data and input files for the
phylogenetic and phylogeographical analyses.

1.	1_H3N2_Kilifi_and_global_genomes_2014-2016

*	H3N2_Kilifi_Kenya_2015-2016_genome_details.txt 

A list of 58 H3N2 virus (2015-2016) NGS data from Kenya showing: strain - virus strain
identity; accession - GISAID accession number; site - location of isolation; reads - number
of sequencing reads; number of influenza virus reads; Ct - PCR Ct value; date - collection
dates; and strain_accession - strain identity showing collection site and collection date
for phylogeographical analyses.

*	H3N2_global_2014-2016_genome_details.txt

A list of global influenza H3N2 virus data from 2014-16 showing: strain - virus strain
identity; accession - GISAID accession number; country - country of isolation; and
continent - continent of isolation.

2.	2_H3N2_Kilifi_phylogeography

File with KHDSS location coordinates and geojson file for phylogeographical analyses of
Kilifi virus sequence data.

* kilifi_coodinates.txt - Kilifi coordinates;

* kilifi.geojson - Kilifi geojson file for phylogeographical analysis of Kilifi NGS data.

* H3N2_KHDSS_2015-2016_WGS_GTRG_UncorrRelax_LogNorm_Bayesian_SkyGrid_BSSVS_400M_MCC.trees - 
MCC tree from BEAST analyses for annotation of phylogenetic tree with virus lineages.

3.	3_H3N2_Kilifi_BaTS
  
The edited trees file from BEAST analysis for BaTS analysis.

* H3N2_Kilifi_2015-2016_WGS_GTRG_UncorrRelax_LogNorm_Bayesian_SkyGrid_BSSVS_400M_bats_locations.trees

## Analysis

This folder contains analysis scripts and analysis files used to generate chapter figures.

1.	1_KHDSS_site_frequency

*	Site_frequency.R - R script for plotting sampled influenza viruses over time. 

*	KHDSS_site_frequency.csv - File with data on sampled influenza viruses.

*	Figure_3.1.pdf

2. 2_KHDSS_site_sequencing

*	Site_sequencing.R - R script for plotting sequenced influenza A(H3N2) viruses over time.

*	KHDSS_site_sequencing.csv - File with data on sequenced influenza viruses.

*	Figure_3.2.pdf

3. 3_KHDSS_sequencing_by_location

*	Genetic_groups_by_location.R - R script for plotting genetic groups of A(H3N2) viruses
by KHDSS location.

*	Genetic_groups_by_location.csv - File with data on genetic groups of A(H3N2) viruses.

*	Figure_3.3.pdf

4. 4_Tanglegrams

* meta.json and tree.json files of 8 gene segments for each A(H3N2) virus genome from
Kilifi for visualization of tanglegrams in Nextstrain (https://nextstrain.org) to identify
reassortment events in A(H3N2) viruses.

5. 5_Lineage_annotation

* KHDSS_lineages.R - R script for plotting viral lineages of A(H3N2) viruses by gene
sequence to identify reassortant strains.

* Lineage_metadata.txt - File with data on viral lineages of A(H3N2) viruses from Kilifi.  

##	Figures

This folder contains all the figures from the analyses.
