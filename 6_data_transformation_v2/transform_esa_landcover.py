import xarray as xr
import numpy as np
import pandas as pd
import geopandas as gpd
import os
from shapely.geometry import box
import dask
from dask.diagnostics import ProgressBar
import gc


#project code
reportCode = "livable_planet"
projCode = "greenwater_forests_biodiv"

#paths
proj_dir = "/Users/tejasvi/Dropbox/WB/" + reportCode + "/" + projCode + "/"
database = "/Users/tejasvi/Dropbox/Database/" 
in_dir = database + "LULC/ESA_CCI_LC_2025/0_raw_data/"
out_dir = database + "LULC/ESA_CCI_LC_2025/1_processed/"
# Create the folder if it doesn't exist
os.makedirs(out_dir, exist_ok=True)

#################################################################
# --- Step 1: Load NetCDF with Dask ---
path = in_dir + "ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992-v2.0.7cds.nc"
ds = xr.open_dataset(path) 
#Match the original chunking on disk
ds = ds.chunk({"lat": 1800, "lon": 1800})  # This helps match the coarsen operation to aggregate 300m to 0.5 degree res
#print(ds["lccs_class"].encoding)
#print(ds['crs']) 

# --- Step 2: Extract the land cover variable ---
da = ds["lccs_class"]  # replace with your actual variable name if different

# Define coarsen window size (180 pixels * 0.00277778 deg ≈ 0.5 deg)
coarsen_lat = 180
coarsen_lon = 180

# Coarsen object (lazy)
coarsened = da.coarsen(lat=coarsen_lat, lon=coarsen_lon, boundary="trim")

# Class values to count (modify if needed)
class_values = np.arange(10, 230, 10)

# Create lists to hold results
count_list = []
lat_centers = []
lon_centers = []

# Calculate new coarse lat/lon coordinates (cell centers)
lat_coarse = (
    da.lat.coarsen(lat=coarsen_lat, boundary="trim").mean().compute()
)
lon_coarse = (
    da.lon.coarsen(lon=coarsen_lon, boundary="trim").mean().compute()
)

# Loop over classes and count pixels in each coarse cell
for class_val in class_values:
    # Construct boolean mask for the class inside blocks
    mask = (coarsened.construct(lat=("lat_block", "lat"), lon=("lon_block", "lon")) == class_val)

    # Sum pixels in each block
    count = mask.sum(dim=("lat", "lon"))

    # Assign class value as coordinate
    count = count.assign_coords(lc_class=class_val)

    # Append result
    count_list.append(count)

# Concatenate all class counts along new 'lc_class' dimension
result = xr.concat(count_list, dim="lc_class")

# Assign proper coordinates for lat/lon coarse grid
result = result.assign_coords(
    lat=("lat_block", lat_coarse.data),
    lon=("lon_block", lon_coarse.data)
)

# Reorder dims for clarity (lc_class, lat, lon)
result = result.transpose("lc_class", "time", "lat_block", "lon_block")

# Convert to pandas DataFrame, with lat/lon columns
df = result.to_dataframe(name="pixel_count").reset_index()


print(df.head())

# --- Optional Step 8: Export to CSV or NetCDF ---
# df.to_csv("landcover_counts_0.5deg.csv", index=False)
# counts.to_netcdf("landcover_counts_0.5deg.nc")





# --- Optional: Save to CSV or GeoJSON later ---
#counts.to_csv("class_counts_0.5deg.csv", index=False)

# #################################################################
# # --- Step 2: Load your existing shapefile ---
# grid_path = database + "Fishnet_halfdegree/" + "global_fishnet_fixed.gpkg"  # Replace with your shapefile path
# grid = gpd.read_file(grid_path)

# # Ensure the CRS is EPSG:4326 (lat/lon), reproject if necessary
# if grid.crs != "EPSG:4326":
#     grid = grid.to_crs("EPSG:4326")

# # Add an ID column if it doesn't exist
# if "grid_id" not in grid.columns:
#     grid["grid_id"] = grid.index

# gc.collect()

# #################################################################
# # --- Step 3: Flatten the raster to point data ---
# print("Flattening raster with Dask...")

# def to_point_df(da):
#     lat_vals = da["lat"].values
#     lon_vals = da["lon"].values
#     lat_grid, lon_grid = np.meshgrid(lat_vals, lon_vals, indexing='ij')
    
#     flat = da.data.reshape(-1)
#     lat_flat = lat_grid.ravel()
#     lon_flat = lon_grid.ravel()
    
#     df = pd.DataFrame({
#         'lat': lat_flat,
#         'lon': lon_flat,
#         'class': flat
#     })
    
#     df = df.dropna()
#     df = df[df['class'] >= 0]
#     return df

# # Use Dask to compute this lazily
# point_df = dask.delayed(to_point_df)(lc_data)



# # --- Step 4: Spatial join between points and 0.5° grid ---
# def spatial_join_count(df, grid):
#     points = gpd.GeoDataFrame(df, geometry=gpd.points_from_xy(df.lon, df.lat), crs="EPSG:4326")
#     joined = gpd.sjoin(points, grid, how='inner', predicate='within')
#     counts = joined.groupby(['grid_id', 'class']).size().reset_index(name='count')
#     return counts

# counts_delayed = dask.delayed(spatial_join_count)(point_df, grid)

# # --- Step 5: Compute the result with progress bar ---
# print("Running spatial join and aggregation with Dask...")
# with ProgressBar():
#     counts = counts_delayed.compute()
