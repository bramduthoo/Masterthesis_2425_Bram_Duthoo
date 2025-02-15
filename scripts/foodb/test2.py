import pyarrow.json as paj
import pyarrow.parquet as pq
import pyarrow.compute as pc
import pandas as pd

# 📌 Pad naar het JSON-bestand
json_file_path = r"C:\Users\bramd_finhsgu\OneDrive - UGent\Thesis\Thesis_bestanden\__MACOSX\foodb_2020_04_07_json\Content.json"

# 📌 Pad naar het Parquet-bestand (voor snellere verwerking)
parquet_file_path = json_file_path.replace(".json", ".parquet")

# 📌 Stap 1: JSON direct omzetten naar Parquet (geen line-by-line processing!)
print("🔄 Converting JSON to Parquet (this may take a moment)...")
table = paj.read_json(json_file_path)
pq.write_table(table, parquet_file_path, compression="snappy")

# 📌 Stap 2: Snelle filtering op food_id
print("🔍 Filtering data...")
table = pq.read_table(parquet_file_path)
filtered_table = table.filter(pc.equal(table["food_id"], 506))  # Filter op food_id = 506

# 📌 Stap 3: Omzetten naar Pandas DataFrame
df_filtered = filtered_table.to_pandas()

# ✅ Resultaat tonen
print(df_filtered.head())
print(f"✅ Filtering complete! Data saved to: {csv_output_path}")
