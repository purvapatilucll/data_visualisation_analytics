import numpy as np
import pandas as pd


def recommend_similar_products(existing_csv, new_product_df, top_n=5, threshold=0.1):
    """Find similar products from existing dataset"""
    existing_df = pd.read_csv(existing_csv, low_memory=False)
    existing_df = existing_df[existing_df["active"] == 1].copy()
    existing_df = existing_df.reset_index(drop=True)

    # Ensure IDs are integers
    existing_df["id"] = (
        pd.to_numeric(existing_df["id"], errors="coerce").fillna(0).astype(int)
    )

    # Handle new_product_df - convert to DataFrame if needed
    if not isinstance(new_product_df, pd.DataFrame):
        new_product_df = pd.DataFrame(new_product_df)

    # Reset index to avoid float indexing issues
    new_product_df = new_product_df.reset_index(drop=True)
    new_product_df["id"] = (
        pd.to_numeric(new_product_df["id"], errors="coerce").fillna(0).astype(int)
    )

    # Use iloc for positional indexing
    if len(new_product_df) == 0:
        return pd.DataFrame()

    new_row = new_product_df.iloc[0].copy()  # Get first row using iloc

    nutrition_cols = ["energy", "fat", "protein", "carbohydrates", "sugar"]

    # Ensure numeric columns
    for col in nutrition_cols:
        existing_df[col] = pd.to_numeric(existing_df[col], errors="coerce").fillna(0)
        if col in new_row.index:
            new_row[col] = pd.to_numeric(new_row[col], errors="coerce")
        else:
            new_row[col] = 0

    # Calculate differences using numpy arrays safely
    new_row_values = (
        new_row[nutrition_cols].values.astype(float)
        if hasattr(new_row[nutrition_cols], "values")
        else new_row[nutrition_cols].astype(float)
    )
    existing_values = existing_df[nutrition_cols].values.astype(float)
    diff = np.abs(existing_values - new_row_values)
    diff_mean = diff.mean(axis=1)

    # Find similar products
    similar_mask = diff_mean <= threshold
    similar_products = existing_df.loc[similar_mask].copy()

    if not similar_products.empty:
        similar_products = similar_products.reset_index(drop=True)
        # Create similarity score as a new column
        similarity_scores = (1 - diff_mean[similar_mask]) * 100
        similar_products.loc[:, "similarity_score"] = similarity_scores.values
        return similar_products.head(top_n)
    else:
        return pd.DataFrame()
