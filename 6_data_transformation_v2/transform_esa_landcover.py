import xarray as xr
import numpy as np
import pandas as pd
import geopandas as gpd
import os
from glob import glob
from shapely.geometry import Point


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


def process_tree_cover_folder(nc_folder, tree_classes=[50,60,70,80,90,100,160,170], coarsen_pixels=180):
    """
    Process multiple NetCDF files containing land cover, 
    count tree cover pixels per coarse cell and return combined DataFrame.

    Parameters:
    - nc_folder: folder path containing NetCDF files (1 file per year)
    - tree_classes: list of land cover classes representing tree cover
    - coarsen_pixels: number of pixels to aggregate (~0.5 deg)

    Returns:
    - df_combined: DataFrame with lat, lon, and tree cover counts per year
    """

    # Find NetCDF files sorted by name (assumed includes year)
    nc_files = sorted(glob(os.path.join(nc_folder, "*.nc")))

    # Prepare list to hold yearly DataFrames
    df_list = []

    for nc_path in nc_files:
        print(f"Processing {nc_path} ...")
        ds = xr.open_dataset(nc_path)
        ds = ds.chunk({"lat": coarsen_pixels*10, "lon": coarsen_pixels*10})  # chunk bigger than coarsen window

        da = ds["lccs_class"]

        # Convert water bodies (class 210) to NaN
        da = da.where(da != 210)

        # Create binary mask for tree classes
        is_tree = da.isin(tree_classes).astype(np.uint8)  # 1 = tree, 0 = not tree, NaN = water

        # Coarsen for tree pixel counts
        tree_count = is_tree.coarsen(lat=coarsen_pixels, lon=coarsen_pixels, boundary="trim").sum().compute()

        # Coarsen for valid land pixel counts
        land_pixel_count = da.notnull().coarsen(lat=coarsen_pixels, lon=coarsen_pixels, boundary="trim").sum().compute()

        # Calculate total pixels per block (e.g., 180x180 = 32400)
        total_pixels = coarsen_pixels * coarsen_pixels  # = 32400

        # Compute % tree using land pixels
        percent_tree_land = (tree_count / land_pixel_count) * 100

        # Compute % tree using all pixels (including NaNs)
        percent_tree_total = (tree_count / total_pixels) * 100

        # Assign lat/lon block centers
        lat_coarse = da.lat.coarsen(lat=coarsen_pixels, boundary="trim").mean().compute()
        lon_coarse = da.lon.coarsen(lon=coarsen_pixels, boundary="trim").mean().compute()

        # Extract year from filename
        basename = os.path.basename(nc_path)
        year = basename.split("-")[7]

        # Assign coordinates for each array
        tree_count = tree_count.assign_coords(lat=("lat", lat_coarse.data), lon=("lon", lon_coarse.data))
        percent_tree_land = percent_tree_land.assign_coords(lat=("lat", lat_coarse.data), lon=("lon", lon_coarse.data))
        percent_tree_total = percent_tree_total.assign_coords(lat=("lat", lat_coarse.data), lon=("lon", lon_coarse.data))

        # Drop 'time' coordinate (only 1 time slice per file)
        tree_count = tree_count.squeeze("time", drop=True)
        percent_tree_land = percent_tree_land.squeeze("time", drop=True)
        percent_tree_total = percent_tree_total.squeeze("time", drop=True)

        # Convert each to dataframe with year-specific column names
        df_tree = tree_count.to_dataframe(name=f"tree_cover_count_{year}").reset_index()
        df_pct_land = percent_tree_land.to_dataframe(name=f"percent_tree_land_{year}").reset_index()
        df_pct_total = percent_tree_total.to_dataframe(name=f"percent_tree_total_{year}").reset_index()

        # Merge the three metrics for this year
        df_year = df_tree.merge(df_pct_land, on=["lat", "lon"]).merge(df_pct_total, on=["lat", "lon"])

        # Append to list
        df_list.append(df_year)

    # Initialize with the first year's full DataFrame
    df_combined = df_list[0]

    # Merge subsequent years
    for dfy in df_list[1:]:
        df_combined = df_combined.merge(dfy, on=["lat", "lon"], how="outer")

    # Fill missing values (e.g., regions with no land/tree)
    df_combined = df_combined.fillna(0).sort_values(by=["lat", "lon"]).reset_index(drop=True)

    return df_combined

# Usage:
tree_class_vals = list(range(50, 101)) + [160, 170]
coarsen_val = 180 #Number of pixels to coarsen by
df_tree_counts = process_tree_cover_folder(in_dir, tree_class_vals, coarsen_val)
print(df_tree_counts.head())

#df_tree_counts.to_csv(out_dir + "tree_cover_counts_by_year.csv", index=False)


def filter_df_by_gpkg_polygons(df, gpkg_path):
    """
    Filters a DataFrame of lat/lon points to keep only those
    that fall within any polygon in the GeoPackage.

    Parameters:
    - df: pandas DataFrame with 'lat' and 'lon' columns
    - gpkg_path: path to GeoPackage file with polygon grid cells

    Returns:
    - filtered_df: subset of df where points fall inside gpkg polygons
    """

    # Load polygons from gpkg
    polygons_gdf = gpd.read_file(gpkg_path)

    # Convert df points to GeoDataFrame
    points_gdf = gpd.GeoDataFrame(
        df,
        geometry=[Point(xy) for xy in zip(df['lon'], df['lat'])],
        crs=polygons_gdf.crs  # assume both use the same CRS, if not, reproject needed
    )

    # Spatial join: keep points inside polygons
    joined = gpd.sjoin(points_gdf, polygons_gdf, how="inner", predicate="within")

    # --- Check 1-to-1 match ---
    # Group by point lat/lon and check how many unique polygon Lat/Lon combinations exist
    grouped = joined.groupby(['lat', 'lon'])[['Lat', 'Lon']].nunique()

    # Find point coords that map to exactly 1 polygon Lat/Lon
    one_to_one = (grouped['Lat'] == 1) & (grouped['Lon'] == 1)
    one_to_one_matches = one_to_one.sum()
    total_unique_points = len(grouped)

    print(f"{one_to_one_matches} out of {total_unique_points} unique lat/lon point combinations have a 1-to-1 match in polygon grid.")

    # Optional: Replace lat/lon in points with polygon Lat/Lon where 1-to-1 match
    if one_to_one_matches == total_unique_points:
        print("All point locations have a unique matching polygon location. You can safely replace lat/lon.")
        # Replace lat/lon in joined with Lat/Lon from polygon
        joined['lat'] = joined['Lat']
        joined['lon'] = joined['Lon']
        print("lat/lon columns have been replaced with polygon Lat/Lon values.")
    else:
        print("Some point locations have ambiguous or no polygon match. Be cautious before replacing lat/lon.")

    # Check exact matches
    lat_match = joined['lat'] == joined["Lat"]
    lon_match = joined['lon'] == joined["Lon"]
    exact_match = lat_match & lon_match

    total_points = len(joined)
    matched_points = exact_match.sum()

    if matched_points == total_points:
        print("All points' lat/lon exactly match the polygon lat/lon columns.")
    elif matched_points == 0:
        print("No points' lat/lon exactly match the polygon lat/lon columns.")
    else:
        print(f"{matched_points} out of {total_points} points' lat/lon exactly match the polygon lat/lon columns.")

    # Drop the geometry and polygon columns, keep original df columns
    filtered_df = joined[df.columns].reset_index(drop=True)

    return filtered_df

fishnet = '/Users/tejasvi/Dropbox/Database/Fishnet_halfdegree/global_fishnet_fixed.gpkg'
df_filtered = filter_df_by_gpkg_polygons(df_tree_counts, fishnet)
print(df_filtered.head())


#df_filtered.to_csv(out_dir + "ESA_tree_cover_by_year_05.csv", index=False)

