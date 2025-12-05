import pandas as pd

# Load original dataset
df = pd.read_csv(r"B:\MY_PROJECTS\smart waste managment system\Sample_data.csv")


# Standardize column names
df = df.rename(columns={
    "area": "area",
    "ticket_date": "date",
    "waste_type": "waste_type",
    "net_weight_kg": "weight"
})

# Format date to YYYY-MM-DD
df["date"] = pd.to_datetime(df["date"]).dt.strftime("%Y-%m-%d")

# Keep only needed columns (in correct order)
df = df[["area", "date", "waste_type", "weight"]]

# Save
df.to_csv("dataset_clean.csv", index=False)

print("Conversion complete: dataset_clean.csv")
