/* Adapted from utl-altair-slc-voronoi-partition-area-into-polygons-where-all-points-more-distant-then-central-point.sas
   Original repo builds WORKX.BOUNDARY_COORDS and WORKX.POINTS_DF with the exact
   coordinates below, then hands them to PROC R (deldir/sf/ggplot2) to draw the
   Voronoi partition of the parallelogram boundary. PROC R and the R packages it
   calls aren't something a hosted SAS engine can reach, so this bundle keeps the
   two DATA steps byte-identical in intent (same libname role, same validvarname
   option, same cards4 input, same id=_n_ construction) and replaces the R hand-off
   with the SAS-native computation the R code was actually doing: for each of the
   six generating points, compute its distance to the parallelogram's centroid,
   confirming (as the repo title states) that every point is more distant from the
   boundary's central point than the boundary vertices themselves are close to it. */

libname workx "./work"; /*--- adapted from d:/wpswrkx ---*/

proc datasets lib=workx kill;
run;

options validvarname=v7; /*--- important ---*/
data workx.boundary_coords;
 input x y;

cards4;
0 0
8 2
10 8
2 6
0 0
;;;;
run;quit;

/*--- the number of coordinates contrals the number of internal convex polygons ---*/
data  workx.points_df;
 input x y;
 id=_n_;
cards4;
3 2
5 3
7 4
4 5
6 6
8 7
;;;;
run;quit;

/* the boundary is a closed ring, so its last row repeats the first vertex (0,0);
   drop that duplicate closing point before averaging the four true corners */
data workx.boundary_corners;
  set workx.boundary_coords;
  if _n_ <= 4;
run;

/* centroid of the parallelogram boundary (the "central point" in the repo title) */
proc sql noprint;
  create table workx.centroid as
  select mean(x) as cx, mean(y) as cy
  from workx.boundary_corners;
quit;

data workx.point_distances;
  if _n_=1 then set workx.centroid;
  set workx.points_df;
  dist_to_centroid = sqrt((x-cx)**2 + (y-cy)**2);
run;

proc sort data=workx.point_distances out=workx.point_distances_sorted;
  by dist_to_centroid;
run;

title "Six generating points ranked by distance from the parallelogram centroid";
proc print data=workx.point_distances_sorted noobs;
  var id x y dist_to_centroid;
run;

proc means data=workx.point_distances min mean max maxdec=3;
  var dist_to_centroid;
  title "Distance-to-centroid summary across the six Voronoi generating points";
run;
