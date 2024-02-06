from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.firefox.options import Options
from bs4 import BeautifulSoup
import pandas as pd
import time
from datetime import datetime
from pymongo import MongoClient
import traceback

def initialize_driver():
    firefox_options = Options()
    firefox_options.add_argument("--no-sandbox")
    firefox_options.add_argument("--disable-dev-shm-usage")
    driver = webdriver.Firefox(options=firefox_options)
    return driver

def remove_rank_change_from_start(rank, rank_change):
    try:
        rank_change_int = int(rank_change)
        return rank[len(rank_change):] if rank_change_int > 0 else rank
    except ValueError:
        return rank
def scrape_data(driver, category):
    driver.get("https://cryptobubbles.net")
    # Find and click the category button
    top_button = driver.find_element(By.CLASS_NAME, 'select-button')
    top_button.click()

    # Wait for the popup to appear and find the specific category button
    category_button = WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH, f'//button[text()="{category}"]')))
    category_button.click()

    # Wait for the table to load
    time.sleep(5)

    # Keep clicking the "Show More" button until it disappears
    while True:
        try:
            show_more_button = driver.find_element(By.XPATH, '//button[text()="Show More"]')
            show_more_button.click()
            time.sleep(2)  # wait for the table to expand
        except:
            break  # the "Show More" button is no longer present

    # Parse the table with BeautifulSoup
    soup = BeautifulSoup(driver.page_source, 'html.parser')
    table = soup.find('table')

    # Extract the table headers
    headers = [th.text for th in table.find_all('th')]
    headers = headers[1:]  # remove the first header
    headers = ['Rank', 'Rank Change'] + headers  # add the rank change header

    # Extract the table rows
    rows = []
    for tr in table.find_all('tr'):
        cells = tr.find_all('td')
        if cells:
            row = []
            for cell in cells:
                if cell.find(class_='currency-rank'):
                    rank = cell.find(class_='currency-rank').text
                    rank_change = cell.find(class_='currency-rank-change')
                    raw_rank_change = ''
                    if rank_change:
                        raw_rank_change = rank_change.text
                        rank_change = '-' + rank_change.text if 'rgb(255, 102, 102)' in rank_change['style'] else '+' + rank_change.text
                    else:
                        rank_change = '0'
                    nrank = remove_rank_change_from_start(rank, raw_rank_change)
                    row.append(nrank)
                    row.append(rank_change)
                else:
                    row.append(cell.text)
            rows.append(row)

    # Create a pandas dataframe
    df = pd.DataFrame(rows, columns=headers)
    return df

# Function to convert shorthand notations (B and M) to full numeric values
def convert_shorthand_notation(value):
    if value.endswith('B'):
        return "{:,.0f}".format(float(value[:-1]) * 1e9)
    elif value.endswith('M'):
        return "{:,.0f}".format(float(value[:-1]) * 1e6)
    else:
        return value

def remove_percentage_sign(value):
    try:
        # Attempt to remove any commas and percentage signs before conversion
        return float(value.replace('%', '').replace(',', '').replace('+', ''))
    except Exception as e:
        # Return the original value if conversion fails
        return value
    
def format_dataframe(dff):
    df = dff.copy()
    #Remove the last column
    df = df.iloc[:, :-1]

    # Update column names to reflect percentage where needed
    df.columns = ['Rank', 'Rank Change', 'Name', 'Price', 'Market Cap', '24h Volume', 'Hour Change (%)', 'Day Change (%)', 'Week Change (%)', 'Month Change (%)', 'Year Change (%)']

    # Convert shorthand notations and percentages
    for column in ['Market Cap', '24h Volume']:
        df[column] = df[column].apply(lambda x: x.replace('$', '').replace(',', '')).apply(convert_shorthand_notation)

    for column in ['Hour Change (%)', 'Day Change (%)', 'Week Change (%)', 'Month Change (%)', 'Year Change (%)']:
        df[column] = df[column].apply(remove_percentage_sign)


    df['Price'] = df['Price'].apply(lambda x: x.replace('$', '').replace(',', '')).apply(lambda x: "{:,.2f}".format(float(x)))

    for column in ['Hour Change (%)', 'Day Change (%)', 'Week Change (%)', 'Month Change (%)', 'Year Change (%)']:
        df[column] = df[column].apply(remove_percentage_sign)

    return df

def insert_into_mongodb(df):
    try:
        client = MongoClient('mongodb://localhost:27017/')
        db = client['Crypto']
        if "CryptoTop1000" not in db.list_collection_names():
            db.create_collection("CryptoTop1000", timeseries={'timeField': 'timestamp', 'metaField': 'metadata', 'granularity': 'hours'})
        df['timestamp'] = datetime.now()
        data_dict = df.to_dict("records")
        db['CryptoTop1000'].insert_many(data_dict)
        print("Data inserted into MongoDB.")
    except Exception as e:
        print(f"Error inserting into MongoDB: {e}")

def run_scraping_process():
    driver = initialize_driver()
    try:
        categories = ["1 - 100", "101 - 200", "201 - 300", "301 - 400", "401 - 500", "501 - 600", "601 - 700", "701 - 800", "801 - 900", "901 - 1000"]
        all_data = pd.DataFrame()
        for category in categories:
            category_data = scrape_data(driver, category)
            if not category_data.empty:
                all_data = pd.concat([all_data, category_data], ignore_index=True)
        formatted_df = format_dataframe(all_data)
        insert_into_mongodb(formatted_df)
    except Exception as e:
        print(f"Unhandled error in run_scraping_process: {traceback.format_exc()}")
    finally:
        driver.quit()


def main2():
    schedule.every(6).hours.do(run_scraping_process)
    while True:
        schedule.run_pending()
        time.sleep(1)

def main():
    run_scraping_process()

if __name__ == "__main__":
    main()
