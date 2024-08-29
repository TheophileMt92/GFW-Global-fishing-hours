import requests

url = "https://api.globalfishingwatch.org/v1/fishing-activity"

# Example of parameters you might need to include (these will depend on the API documentation)
params = {
    "start_date": "2023-01-01",
    "end_date": "2023-01-31",
    "bbox": "-180,-90,180,90"  # Bounding box for the entire world
}

response = requests.get(url, params=params)

# Check if the request was successful
if response.status_code == 200:
    data = response.json()
    # Process your data here
else:
    print(f"Failed to retrieve data: {response.status_code}")
