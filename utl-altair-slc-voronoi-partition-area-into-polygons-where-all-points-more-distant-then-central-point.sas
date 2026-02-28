%let pgm=utl-altair-slc-voronoi-partition-area-into-polygons-where-all-points-more-distant-then-central-point;

%stop_submission;

altair slc voronoi partition area into polygons where all points more distant then central points

for graphic outut see (the minimum distance is the perpendicular cord from the cental point to the boundary)
https://github.com/rogerjdeangelis/utl-altair-slc-voronoi-partition-area-into-polygons-where-all-points-more-distant-then-central-point/blob/main/voronoi_parallelogram.pdf

too long to post on a list, see github
https://github.com/rogerjdeangelis/utl-altair-slc-voronoi-partition-area-into-polygons-where-all-points-more-distant-then-central-point

related repo
https://github.com/rogerjdeangelis/utl_voronoi_diagram_on_a_shapefile_of_singapore

NOTES

http://mathworld.wolfram.com/VoronoiDiagram.html

The partitioning of a plane with n points into convex polygons such that each polygon contains
exactly one generating point and every point in a given polygon is closer to its
generating point than to any other. A Voronoi diagram is sometimes also known as a Dirichlet
tessellation. The cells are called Dirichlet regions, Thiessen polytopes, or Voronoi polygons.

A particularly notable use of a Voronoi diagram was
the analysis of the 1854 cholera epidemic in London, in which physician John Snow
determined a strong correlation of deaths with proximity to a
particular (and infected) water pump on Broad Street.

In mathematics, a Voronoi diagram is a partitioning of a plane into regions based on
distance to points in a specific subset of the plane. ... These regions are called Voronoi cells.
The Voronoi diagram of a set of points is dual to its Delaunay triangulation.

They find widespread applications in areas such as computer graphics, epidemiology,
geophysics, and meteorology.
see
https://stackoverflow.com/questions/50979999/delimit-voronoi-diagram-with-map-boundary-in-r

Robert Hijmans profile
https://stackoverflow.com/users/635245/robert-hijmans

/*                   _
(_)_ __  _ __  _   _| |_
| | `_ \| `_ \| | | | __|
| | | | | |_) | |_| | |_
|_|_| |_| .__/ \__,_|\__|
        |_|
*/

libname workx "d:/wpswrkx"; /*--- put this in your autoexec ---*/

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


/**************************************************************************************************************************/
/* WORKX.BOUNDARY_COORDS  |  WORKX.POINTS_DF                                                                              */
/*                        |                                                                                               */
/*  Obs     X    Y        |  Obs    X    Y                                                                                */
/*                        |                                                                                               */
/*   1      0    0        |   1     3    2                                                                                */
/*   2      8    2        |   2     5    3                                                                                */
/*   3     10    8        |   3     7    4                                                                                */
/*   4      2    6        |   4     4    5                                                                                */
/*   5      0    0        |   5     6    6                                                                                */
/*                        |   6     8    7                                                                                */
/**************************************************************************************************************************/

/*
 _ __  _ __ ___   ___ ___  ___ ___
| `_ \| `__/ _ \ / __/ _ \/ __/ __|
| |_) | | | (_) | (_|  __/\__ \__ \
| .__/|_|  \___/ \___\___||___/___/
|_|
*/

%utlfkil(d:/pdf/voronoi_parallelogram.pdf);

options validvarname=v7;
options set=RHOME "C:\Progra~1\R\R-4.5.2\bin\r";
proc r;
export data=workx.boundary_coords r=boundary_coords ;
export data=workx.points_df       r=points_df       ;
submit;
# Load required libraries
library(sf)
library(deldir)
library(ggplot2)
boundary_coords
points_df

boundary_coords <- as.matrix(boundary_coords);

boundary <- st_polygon(list(boundary_coords))
boundary_sf <- st_sfc(boundary, crs = 4326)  # 4326 lat lon coords;

# Create six points within the parallelogram
set.seed(456)  # for reproducibility

# max range of x and y;
rwx <- c(range(boundary_coords[, "x"]), range(boundary_coords[, "y"]))

# Create Voronoi polygons
vor <- deldir(points_df$x, points_df$y,
              rw = rwx)  # set overall range
tiles <- tile.list(vor)

# Convert to sf polygons
vor_polygons <- lapply(tiles, function(tile) {
  coords_tile <- cbind(tile$x, tile$y)
  # Close the polygon
  coords_tile <- rbind(coords_tile, coords_tile[1,])
  st_polygon(list(coords_tile))
})

# Create sf object for Voronoi
vor_sf <- st_sfc(vor_polygons, crs = 4326)

# Clip Voronoi to parallelogram boundary
vor_clipped <- st_intersection(vor_sf, boundary_sf)

# Create a more detailed ggplot version
p <- ggplot() +
  # Add the parallelogram boundary
  geom_sf(data = boundary_sf, fill = "lightyellow", alpha = 0.3,
          color = "darkred", size = 1.2, linetype = "solid") +
  # Add Voronoi cells
  geom_sf(data = st_as_sf(vor_clipped),
          aes(fill = as.factor(1:6)), alpha = 0.7, color = "black", size = 0.5) +
  # Add center points
  geom_point(data = points_df, aes(x = x, y = y),
             size = 5, shape = 21, fill = "yellow", color = "black", stroke = 1) +
  # Add point labels
  geom_text(data = points_df, aes(x = x, y = y, label = id),
            vjust = -1.2, size = 5, fontface = "bold") +
  # Add boundary vertices
  geom_point(data = data.frame(x = boundary_coords[1:4,1], y = boundary_coords[1:4,2]),
             aes(x, y), size = 4, shape = 15, color = "darkred") +
  geom_text(data = data.frame(x = boundary_coords[1:4,1], y = boundary_coords[1:4,2],
                              label = c("V1", "V2", "V3", "V4")),
            aes(x, y, label = label), vjust = c(1.5, 1.5, -1, -1),
            hjust = c(-0.5, 1.5, 1.5, -0.5), color = "darkred", fontface = "bold") +
  # Customize colors
  scale_fill_manual(name = "Voronoi Cell",
                    values = rainbow(6),
                    labels = paste("Cell", 1:6)) +
  # Labels and theme
  labs(title = "Voronoi Diagram with Parallelogram Boundary",
       subtitle = "Six center points within a custom parallelogram",
       x = "X Coordinate", y = "Y Coordinate",
       caption = "Vertices: V1(0,0), V2(8,2), V3(10,8), V4(2,6)") +
  theme_minimal() +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        panel.grid.minor = element_line(color = "gray90", linetype = "dotted"))

# Save ggplot version
ggsave("d:/pdf/voronoi_parallelogram.pdf", p, width = 6, height = 4)

# Print boundary and point information
cat("Parallelogram Boundary Vertices:\n")
print(data.frame(
  Vertex = c("V1", "V2", "V3", "V4"),
  X = boundary_coords[1:4, 1],
  Y = boundary_coords[1:4, 2]
))

cat("\nSix Center Points:\n")
print(points_df)


endsubmit;
run;

/*           _               _
  ___  _   _| |_ _ __  _   _| |_
 / _ \| | | | __| `_ \| | | | __|
| (_) | |_| | |_| |_) | |_| | |_
 \___/ \__,_|\__| .__/ \__,_|\__|
                |_|
*/

/**************************************************************************************************************************/
/*  d:/pdf/voronoi_parallelogram.pdf                                                                                      */
/**************************************************************************************************************************/

/*
| | ___   __ _
| |/ _ \ / _` |
| | (_) | (_| |
|_|\___/ \__, |
         |___/
*/

1                                          Altair SLC     11:29 Saturday, February 28, 2026

NOTE: Copyright 2002-2025 World Programming, an Altair Company
NOTE: Altair SLC 2026 (05.26.01.00.000758)
      Licensed to Roger DeAngelis
NOTE: This session is executing on the X64_WIN11PRO platform and is running in 64 bit mode

NOTE: AUTOEXEC processing beginning; file is C:\wpsoto\autoexec.sas
NOTE: AUTOEXEC source line
1       +  ï»¿ods _all_ close;
           ^
ERROR: Expected a statement keyword : found "?"
NOTE: Library workx assigned as follows:
      Engine:        SAS7BDAT
      Physical Name: d:\wpswrkx

NOTE: Library slchelp assigned as follows:
      Engine:        WPD
      Physical Name: C:\Progra~1\Altair\SLC\2026\sashelp

NOTE: Library worksas assigned as follows:
      Engine:        SAS7BDAT
      Physical Name: d:\worksas

NOTE: Library workwpd assigned as follows:
      Engine:        WPD
      Physical Name: d:\workwpd


LOG:  11:29:16
NOTE: 1 record was written to file PRINT

NOTE: The data step took :
      real time : 0.031
      cpu time  : 0.015


NOTE: AUTOEXEC processing completed

1         %utlfkil(d:/pdf/voronoi_parallelogram.pdf);
The file d:/pdf/voronoi_parallelogram.pdf does not exist
2
3         options validvarname=v7;
4         options set=RHOME "C:\Progra~1\R\R-4.5.2\bin\r";
5         proc r;
NOTE: Using R version 4.5.2 (2025-10-31 ucrt) from C:\Program Files\R\R-4.5.2
6         export data=workx.boundary_coords r=boundary_coords ;
NOTE: Creating R data frame 'boundary_coords' from data set 'WORKX.boundary_coords'

7         export data=workx.points_df       r=points_df       ;
NOTE: Creating R data frame 'points_df' from data set 'WORKX.points_df'

8         submit;
9         # Load required libraries
10        library(sf)
11        library(deldir)
12        library(ggplot2)
13        boundary_coords
14        points_df
15        # Create a parallelogram boundary
16        # Define the four corners of a parallelogram
17        # Using points: (0,0), (8,2), (10,8), (2,6)
18
19        boundary_coords <- as.matrix(boundary_coords);
20
21        boundary <- st_polygon(list(boundary_coords))
22        boundary_sf <- st_sfc(boundary, crs = 4326)  # 4326 lat lon coords;
23
24        # Create six points within the parallelogram
25        set.seed(456)  # for reproducibility
26
27        rwx <- c(range(boundary_coords[, "x"]), range(boundary_coords[, "y"]))
28        str(rwx)
29        rwx
30
31        # Create Voronoi polygons
32        vor <- deldir(points_df$x, points_df$y,
33                      rw = rwx)  # set overall range
34        tiles <- tile.list(vor)
35
36        # Convert to sf polygons
37        vor_polygons <- lapply(tiles, function(tile) {
38          coords_tile <- cbind(tile$x, tile$y)
39          # Close the polygon
40          coords_tile <- rbind(coords_tile, coords_tile[1,])
41          st_polygon(list(coords_tile))
42        })
43
44        # Create sf object for Voronoi
45        vor_sf <- st_sfc(vor_polygons, crs = 4326)
46
47        # Clip Voronoi to parallelogram boundary
48        vor_clipped <- st_intersection(vor_sf, boundary_sf)
49
50        # Create a more detailed ggplot version
51        p <- ggplot() +
52          # Add the parallelogram boundary
53          geom_sf(data = boundary_sf, fill = "lightyellow", alpha = 0.3,
54                  color = "darkred", size = 1.2, linetype = "solid") +
55          # Add Voronoi cells
56          geom_sf(data = st_as_sf(vor_clipped),
57                  aes(fill = as.factor(1:6)), alpha = 0.7, color = "black", size = 0.5) +
58          # Add center points
59          geom_point(data = points_df, aes(x = x, y = y),
60                     size = 5, shape = 21, fill = "yellow", color = "black", stroke = 1) +
61          # Add point labels
62          geom_text(data = points_df, aes(x = x, y = y, label = id),
63                    vjust = -1.2, size = 5, fontface = "bold") +
64          # Add boundary vertices
65          geom_point(data = data.frame(x = boundary_coords[1:4,1], y = boundary_coords[1:4,2]),
66                     aes(x, y), size = 4, shape = 15, color = "darkred") +
67          geom_text(data = data.frame(x = boundary_coords[1:4,1], y = boundary_coords[1:4,2],
68                                      label = c("V1", "V2", "V3", "V4")),
69                    aes(x, y, label = label), vjust = c(1.5, 1.5, -1, -1),
70                    hjust = c(-0.5, 1.5, 1.5, -0.5), color = "darkred", fontface = "bold") +
71          # Customize colors
72          scale_fill_manual(name = "Voronoi Cell",
73                            values = rainbow(6),
74                            labels = paste("Cell", 1:6)) +
75          # Labels and theme
76          labs(title = "Voronoi Diagram with Parallelogram Boundary",
77               subtitle = "Six center points within a custom parallelogram",
78               x = "X Coordinate", y = "Y Coordinate",
79               caption = "Vertices: V1(0,0), V2(8,2), V3(10,8), V4(2,6)") +
80          theme_minimal() +
81          theme(legend.position = "right",
82                plot.title = element_text(hjust = 0.5, face = "bold"),
83                plot.subtitle = element_text(hjust = 0.5),
84                panel.grid.minor = element_line(color = "gray90", linetype = "dotted"))
85
86        # Save ggplot version
87        ggsave("d:/pdf/voronoi_parallelogram.pdf", p, width = 10, height = 8)
88
89        # Print boundary and point information
90        cat("Parallelogram Boundary Vertices:\n")
91        print(data.frame(
92          Vertex = c("V1", "V2", "V3", "V4"),
93          X = boundary_coords[1:4, 1],
94          Y = boundary_coords[1:4, 2]
95        ))
96
97        cat("\nSix Center Points:\n")
98        print(points_df)
99
100
101       endsubmit;

NOTE: Submitting statements to R:

> # Load required libraries
> library(sf)
Linking to GEOS 3.13.1, GDAL 3.11.4, PROJ 9.7.0; sf_use_s2() is TRUE
> library(deldir)
deldir 2.0-4      Nickname: "Idol Comparison"
     The syntax of deldir() has changed since version
     0.0-10.  Read the help!!!.
> library(ggplot2)
> boundary_coords
> points_df
> # Create a parallelogram boundary
> # Define the four corners of a parallelogram
> # Using points: (0,0), (8,2), (10,8), (2,6)
>
> boundary_coords <- as.matrix(boundary_coords);
>
> boundary <- st_polygon(list(boundary_coords))
> boundary_sf <- st_sfc(boundary, crs = 4326)  # 4326 lat lon coords;
>
> # Create six points within the parallelogram
> set.seed(456)  # for reproducibility
>
> rwx <- c(range(boundary_coords[, "x"]), range(boundary_coords[, "y"]))
> str(rwx)
> rwx
>
> # Create Voronoi polygons
> vor <- deldir(points_df$x, points_df$y,
+               rw = rwx)  # set overall range
> tiles <- tile.list(vor)
>
> # Convert to sf polygons
> vor_polygons <- lapply(tiles, function(tile) {
+   coords_tile <- cbind(tile$x, tile$y)
+   # Close the polygon
+   coords_tile <- rbind(coords_tile, coords_tile[1,])
+   st_polygon(list(coords_tile))
+ })
>
> # Create sf object for Voronoi
> vor_sf <- st_sfc(vor_polygons, crs = 4326)
>
> # Clip Voronoi to parallelogram boundary
> vor_clipped <- st_intersection(vor_sf, boundary_sf)
>
> # Create a more detailed ggplot version
> p <- ggplot() +
+   # Add the parallelogram boundary
+   geom_sf(data = boundary_sf, fill = "lightyellow", alpha = 0.3,
+           color = "darkred", size = 1.2, linetype = "solid") +
+   # Add Voronoi cells
+   geom_sf(data = st_as_sf(vor_clipped),
+           aes(fill = as.factor(1:6)), alpha = 0.7, color = "black", size = 0.5) +
+   # Add center points
+   geom_point(data = points_df, aes(x = x, y = y),
+              size = 5, shape = 21, fill = "yellow", color = "black", stroke = 1) +
+   # Add point labels
+   geom_text(data = points_df, aes(x = x, y = y, label = id),
+             vjust = -1.2, size = 5, fontface = "bold") +
+   # Add boundary vertices
+   geom_point(data = data.frame(x = boundary_coords[1:4,1], y = boundary_coords[1:4,2]),
+              aes(x, y), size = 4, shape = 15, color = "darkred") +
+   geom_text(data = data.frame(x = boundary_coords[1:4,1], y = boundary_coords[1:4,2],
+                               label = c("V1", "V2", "V3", "V4")),
+             aes(x, y, label = label), vjust = c(1.5, 1.5, -1, -1),
+             hjust = c(-0.5, 1.5, 1.5, -0.5), color = "darkred", fontface = "bold") +
+   # Customize colors
+   scale_fill_manual(name = "Voronoi Cell",
+                     values = rainbow(6),
+                     labels = paste("Cell", 1:6)) +
+   # Labels and theme
+   labs(title = "Voronoi Diagram with Parallelogram Boundary",
+        subtitle = "Six center points within a custom parallelogram",
+        x = "X Coordinate", y = "Y Coordinate",
+        caption = "Vertices: V1(0,0), V2(8,2), V3(10,8), V4(2,6)") +
+   theme_minimal() +
+   theme(legend.position = "right",
+         plot.title = element_text(hjust = 0.5, face = "bold"),
+         plot.subtitle = element_text(hjust = 0.5),
+         panel.grid.minor = element_line(color = "gray90", linetype = "dotted"))
>
> # Save ggplot version
> ggsave("d:/pdf/voronoi_parallelogram.pdf", p, width = 10, height = 8)
>
> # Print boundary and point information
> cat("Parallelogram Boundary Vertices:\n")
> print(data.frame(
+   Vertex = c("V1", "V2", "V3", "V4"),
+   X = boundary_coords[1:4, 1],
+   Y = boundary_coords[1:4, 2]
+ ))
>
> cat("\nSix Center Points:\n")
> print(points_df)
>
>

NOTE: Processing of R statements complete

102       run;
NOTE: Procedure r step took :
      real time : 3.063
      cpu time  : 0.015


ERROR: Error printed on page 1

NOTE: Submitted statements took :
      real time : 3.185
      cpu time  : 0.093
/*              _
  ___ _ __   __| |
 / _ \ `_ \ / _` |
|  __/ | | | (_| |
 \___|_| |_|\__,_|

*/
