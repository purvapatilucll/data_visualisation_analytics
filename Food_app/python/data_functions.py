import pandas as pd
from sklearn.metrics.pairwise import cosine_similarity
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.preprocessing import StandardScaler
from sklearn.cluster import SpectralClustering
from sklearn.decomposition import PCA
from difflib import SequenceMatcher
import numpy as np


def load_food_data(file_path):
    """Load and return the food CSV as a DataFrame"""
    return pd.read_csv(file_path, dtype=str)


def process_food_data(data):
    """Process/clean data"""
    data = pd.DataFrame(data)
    return data


def generate_summary(data):
    """Generate a text summary"""
    df = pd.DataFrame(data) if not isinstance(data, pd.DataFrame) else data
    if "category" in df.columns:
        ncat = df["category"].nunique()
    else:
        ncat = "N/A"
    return f"Total items: {len(df)}, Categories: {ncat}"


def get_unique_categories(data):
    """Extract all unique categories from semicolon-separated values"""
    df = pd.DataFrame(data)
    if "categories" not in df.columns:
        return []

    categories = set()
    for cats in df["categories"].dropna():
        if isinstance(cats, str):
            # Split by semicolon and strip whitespace
            cats_list = [c.strip() for c in cats.split(";") if c.strip()]
            categories.update(cats_list)

    return sorted(list(categories))


def cluster_foods(food_df, n_clusters=5):
    """Cluster foods based on numeric nutritional fields using Spectral Clustering (KNN affinity)."""
    if not isinstance(food_df, pd.DataFrame):
        food_df = pd.DataFrame(food_df)

    brands = set()
    for brand_str in df["brands_search"].dropna():
        if isinstance(brand_str, str):
            # Split by semicolon and strip whitespace
            brands_list = [b.strip() for b in brand_str.split(";") if b.strip()]
            brands.update(brands_list)

    scaler = StandardScaler()
    X_scaled = scaler.fit_transform(df_numeric)

    # Spectral clustering with KNN-based affinity
    clustering = SpectralClustering(
        n_clusters=n_clusters, affinity="nearest_neighbors", random_state=42
    )
    clusters = clustering.fit_predict(X_scaled)

    food_df["cluster"] = clusters

    # PCA for 3D visualization using all featuresa
    pca = PCA(n_components=3)
    pca_components = pca.fit_transform(X_scaled)
    food_df["pca1"] = pca_components[:, 0]
    food_df["pca2"] = pca_components[:, 1]
    food_df["pca3"] = pca_components[:, 2]

    return food_df


def highlight_missing(food_df):
    """Return dataframe with missing numeric fields highlighted"""
    if not isinstance(food_df, pd.DataFrame):
        food_df = pd.DataFrame(food_df)

    numeric_cols = [
        "energy",
        "protein",
        "fat",
        "carbohydrates",
        "sugar",
        "starch",
        "dietary_fiber",
    ]
    food_df["missing_fields"] = food_df[numeric_cols].isna().sum(axis=1)
    return food_df


def compute_similarity(food_df, query):
    """Compute similarity % for all products based on nutrition + name"""
    if not isinstance(food_df, pd.DataFrame):
        food_df = pd.DataFrame(food_df)

    # Handle missing values
    food_df["name_search"] = food_df["name_search"].fillna("").astype(str)

    # Name similarity
    food_df["name_similarity"] = food_df["name_search"].apply(
        lambda x: SequenceMatcher(None, x.lower(), str(query).lower()).ratio()
    )

    # Nutrition similarity
    numeric_cols = [
        "energy",
        "protein",
        "fat",
        "carbohydrates",
        "sugar",
        "starch",
        "dietary_fiber",
    ]
    df_numeric = food_df[numeric_cols].fillna(0)

    # Query vector (use mean of dataset)
    query_vector = df_numeric.mean()

    cos_sim = cosine_similarity(df_numeric, [query_vector.values]).flatten()

    # Combined similarity
    food_df["similarity_score"] = (
        food_df["name_similarity"] * 0.5 + cos_sim * 0.5
    ) * 100

    return food_df.sort_values("similarity_score", ascending=False).head(10)


# def compute_name_similarity(df, query):
#     if df.empty:
#         return pd.DataFrame()
#     df['name'] = df['name'].fillna('').astype(str)
#     vectorizer = TfidfVectorizer()
#     X = vectorizer.fit_transform(df['name'])
#     q_vec = vectorizer.transform([str(query)])
#     sim_scores = cosine_similarity(q_vec, X).flatten()
#     df['similarity_score'] = sim_scores
#     df = df[df['similarity_score'] > 0.5]  # Threshold for similarity
#     df = df.sort_values(by='similarity_score', ascending=False)
#     return df


def ensure_dataframe(obj):
    if isinstance(obj, pd.DataFrame):
        return obj.copy()

    if isinstance(obj, dict):
        return pd.DataFrame([obj])

    if isinstance(obj, list):
        try:
            return pd.DataFrame(obj)
        except:
            return pd.DataFrame({"value": obj})

    return pd.DataFrame({"value": [obj]})


def safe_tfidf_fit(text_series, analyzer="char", ngram_range=(2, 4)):
    cleaned = text_series.fillna("").astype(str).str.strip()

    if cleaned.str.len().sum() == 0:
        return None, None

    vectorizer = TfidfVectorizer(analyzer=analyzer, ngram_range=ngram_range)

    try:
        X = vectorizer.fit_transform(cleaned)
        return vectorizer, X
    except ValueError:
        return None, None


def compute_name_similarity(df, query, top_n=10):
    df = ensure_dataframe(df)

    # Ensure structure
    for col in ["name", "bron", "categories"]:
        if col not in df.columns:
            df[col] = ""

    df["name"] = df["name"].fillna("").astype(str)
    df["bron"] = df["bron"].fillna("").astype(str)
    df["categories"] = df["categories"].fillna("").astype(str)

    # name (highest priority)
    name_vec, X_name = safe_tfidf_fit(df["name"], analyzer="char", ngram_range=(2, 4))
    if name_vec is None:
        df["similarity_score"] = 0
        return df.head(top_n)

    q_name = name_vec.transform([query])
    name_sim = cosine_similarity(q_name, X_name).flatten()

    # brand similarity
    brand_vec, X_brand = safe_tfidf_fit(df["bron"], analyzer="char", ngram_range=(2, 3))
    if brand_vec:
        q_brand = brand_vec.transform([query])
        brand_sim = cosine_similarity(q_brand, X_brand).flatten()
    else:
        brand_sim = np.zeros(len(df))

    # category similarity
    cat_vec, X_cat = safe_tfidf_fit(
        df["categories"], analyzer="word", ngram_range=(1, 2)
    )
    if cat_vec:
        q_cat = cat_vec.transform([query])
        category_sim = cosine_similarity(q_cat, X_cat).flatten()
    else:
        category_sim = np.zeros(len(df))

    df["similarity_score"] = (
        name_sim * 0.70
        + (brand_sim * 0.15 * (brand_sim > 0))
        + (category_sim * 0.15 * (category_sim > 0))
    )

    return df.sort_values("similarity_score", ascending=False).head(top_n)


def detect_column(df, candidates):
    for c in candidates:
        if c in df.columns:
            return c
    return None


def get_numeric_columns(df, allowed_cols):
    cols = [c for c in allowed_cols if c in df.columns]
    df[cols] = df[cols].apply(pd.to_numeric, errors="coerce")
    return cols


def compute_variance_by_brand(food_df, numeric_cols, top_n=15):
    df = ensure_dataframe(food_df)

    # Convert top_n to int (R might send as float)
    top_n = int(top_n)

    # Detect which column represents as category
    brand_col = detect_column(df, ["brand", "brands", "bron", "brands_search"])
    if not brand_col:
        return {}

    # Keep only numeric columns that exist in df, convert to numeric
    numeric_cols = get_numeric_columns(df, numeric_cols)
    if not numeric_cols:
        return {}

    # Top brands by frequency
    top_brands = df[brand_col].value_counts().head(top_n).index.tolist()

    df_top = df[df[brand_col].isin(top_brands)]

    # Build dictionary of nutrient values per brand for plotting
    result = {}
    for nutrient in numeric_cols:
        result[nutrient] = {
            brand: df_top.loc[df_top[brand_col] == brand, nutrient]
            .dropna()
            .values.tolist()
            for brand in top_brands
        }

    return {
        "group_type": "brand",
        "group_column": brand_col,
        "groups": top_brands,
        "data": result,
    }


def compute_variance_by_category(food_df, numeric_cols, top_n=15):
    df = ensure_dataframe(food_df)

    # Convert top_n to int (R might send as float)
    top_n = int(top_n)

    # Detect which column represents the brand
    cat_col = detect_column(df, ["category", "categories", "categorie", "cats"])
    if not cat_col:
        return {}

    # Keep only numeric columns that exist in df, convert to numeric
    numeric_cols = get_numeric_columns(df, numeric_cols)
    if not numeric_cols:
        return {}

    # Top categories by frequency
    top_categories = df[cat_col].value_counts().head(top_n).index.tolist()

    df_top = df[df[cat_col].isin(top_categories)]

    # Build dictionary of nutrient values per category for plotting
    result = {}
    for nutrient in numeric_cols:
        result[nutrient] = {
            cat: df_top.loc[df_top[cat_col] == cat, nutrient].dropna().values.tolist()
            for cat in top_categories
        }

    return {
        "group_type": "category",
        "group_column": cat_col,
        "groups": top_categories,
        "data": result,
    }
