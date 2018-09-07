import time

starttime = time.strftime("%d %b %Y, %H:%M:%S", time.localtime())
print("Starting process, date and time: " + starttime)
print("--------------------------------------------------------")
print("")

#####################################################################################
# import additional packages
from osgeo import gdal, ogr, osr
import pandas as pd
import numpy as np
import math
import multiprocessing
import os
import glob
import random as rd
import shapely
import struct
#####################################################################################

root_folder = "//141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany"
data_path = "//141.20.140.222/Dagobah/edc"
os.chdir("//141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany")

#get driver
mem_driver = ogr.GetDriverByName('Memory')
shp_driver = ogr.GetDriverByName("ESRI Shapefile")
undist_forest = gdal.Open(root_folder + "/germany_landcover_2015_g1416lc3_dbf_undisturbed_erode3x3_mmu11.bsq")
gt_for = undist_forest.GetGeoTransform()

# open shapefiles
station_shp = shp_driver.Open(root_folder + '/dwd/stat_dwd_laea.shp', 1)
station = station_shp.GetLayer()
tiles_shp = shp_driver.Open(root_folder + "/germany.shp",1)
tiles = tiles_shp.GetLayer()

# create shapefile in Memory

data_source = mem_driver.CreateDataSource('temp.shp')
srs = osr.SpatialReference()
srs.ImportFromEPSG(3035)
layer = data_source.CreateLayer("temp", srs, ogr.wkbPolygon)
defn = layer.GetLayerDefn()
bufferDist = 2000
for feature in station:
    ingeom = feature.GetGeometryRef()
    geomBuffer = ingeom.Buffer(bufferDist)
    outFeature = ogr.Feature(defn)
    outFeature.SetGeometry(geomBuffer)
    layer.CreateFeature(outFeature)
station.ResetReading()

tile_list = []
t_feat = tiles.GetNextFeature()
while t_feat:
    geom_t = t_feat.GetGeometryRef()
    x_min_t, x_max_t, y_min_t, y_max_t = t_feat.geometry().GetEnvelope()
    tile_list.append([x_min_t, x_max_t, y_min_t, y_max_t])
    t_feat = tiles.GetNextFeature()
tiles.ResetReading()
x_min_t = [x[0] for x in tile_list]

b_feat = layer.GetNextFeature()
while b_feat:
    geom = b_feat.GetGeometryRef()
    x_min_b, x_max_b, y_min_b, y_max_b = b_feat.geometry().GetEnvelope()    # get extent of sampling area
    if x_min_b >= x_min_t: #and x_max_b <= x[1] for x in tile_list and y_min_b >= x[2] for x in tile_list and y_max_b <= x[3] for x in tile_list:
        print('yes')
    else: print('nope')
#     #         x1 = rd.uniform(x_min_b, x_max_b)
#     #         y1 = rd.uniform(y_min_b, y_max_b)
#     #         point = ogr.Geometry(ogr.wkbPoint)                              # create empty geometry and then add point geometry from coordinates
#     #         point.AddPoint(x1, y1)
#     #         if point.within(geom):                                          # check if point falls within circular area
#     #             px_ras = int((x - gt_ras[0]) / gt_ras[1])                   # calculate absolute raster coordinates
#     #             py_ras = int((y - gt_ras[3]) / gt_ras[5])
    b_feat = layer.GetNextFeature()
layer.ResetReading()
#
#
#
#



        #####################################################################################
# set ending time ###################################################################
print("")
endtime = time.strftime("%H:%M:%S", time.localtime())
print("--------------------------------------------------------")
print("Process finished at: " + endtime)
print("")