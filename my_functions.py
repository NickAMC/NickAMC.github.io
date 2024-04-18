def show_summary(df) :
    
    print(80 * "+")
    print(f"DIMENSIONS : ({df.shape[0]}, {df.shape[1]})")
    
    print(80 * "+")
    print("COLUMNS: \n")
    print(df.columns.values)
    
    print(80 * "+")
    print("DATA INFO: \n")
    print(df.dtypes)
    
    print(80 * "+")
    print("MISSING VALUES: \n")
    print(df.isnull().sum())
    
    print(80 * "+")
    print("NUMBER OF UNIQUE VALUES: \n")
    print(df.nunique())
    