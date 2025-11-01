# -*- coding: utf-8 -*-
"""
Created on Sun Oct 27 16:33:58 2024

@author: Adam Staley
"""


import psycopg2

def create_w_basketball_game_chart(cursor):
    w_basketball_game_chart_table_creator = """
        CREATE TABLE IF NOT EXISTS w_basketball_game_chart_T(
            Event_ID SERIAL PRIMARY KEY,
            Event TEXT,
            Event_Person INT,
            Event_Team TEXT,
            Event_X FLOAT,
            Event_Y FLOAT,
            Event_Distance FLOAT,
            Event_Zone TEXT,
            Made_Miss TEXT,
            Three_Pt INT,
            Dribble_Catch TEXT,
            FG INT,
            Off_Def INT,
            Shot_Recovery_Zone TEXT,
            H1 INT,
            H2 INT, 
            H3 INT,
            H4 INT,
            H5 INT,
            A1 INT,
            A2 INT, 
            A3 INT,
            A4 INT,
            A5 INT,
            Game_Date DATE,
            Opponent TEXT,
            Home_Away TEXT,
            Quarter INT,
            Time_of_Day TIME,
            Video_Angle1 TEXT,
            Video_Angle2 TEXT
            
        );
    """
    cursor.execute(w_basketball_game_chart_table_creator)


def main():
    try:
        # Connect to the PostgreSQL database
        connection = psycopg2.connect(
            dbname="ps1",
            user="pythoncon",
            password="password",
            host="18.217.248.114",
            port="5432"
        )

        # Create a cursor object
        with connection.cursor() as cursor:
            # Create the rom_assessments_T table
            create_w_basketball_game_chart(cursor)
            
            
            # Commit the transaction to save changes
            connection.commit()
            
            print("Table 'w_basketball_game_chart' created successfully.")

    except Exception as e:
        # Print any error that occurs
        print(f"An error occurred: {e}")

    finally:
        # Close the connection
        if connection:
            connection.close()
            print("Database connection closed.")

if __name__ == "__main__":
    main()
