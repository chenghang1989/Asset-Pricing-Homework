import json

files = ['Homework1.ipynb', 'Homework 2 Anomaly.ipynb', 'Homework 2 Beta.ipynb']
for file in files:
    try:
        with open(file, 'r', encoding='utf-8') as f:
            nb = json.load(f)

        for cell in nb.get('cells', []):
            if cell['cell_type'] == 'code':
                source = cell['source']
                for i, line in enumerate(source):
                    line = line.replace('Data/TRD_Cnmont2024.csv', '/Volumes/BetAlpha/Asset Pricing/Data/Input/Market_Return/TRD_Cnmont2025.csv')
                    line = line.replace('Data/TRD_Nrrate2024.csv', '/Volumes/BetAlpha/Asset Pricing/Data/Input/Market_Return/TRD_Nrrate2025.csv')
                    line = line.replace('Data/TRD_Cndalym2024.csv', '/Volumes/BetAlpha/Asset Pricing/Data/Input/Market_Return/TRD_Cndalym2025.csv')
                    line = line.replace('Data/DPR_Acptl202412.csv', '/Volumes/BetAlpha/Asset Pricing/Data/Input/Dividend/DPR_Acptl2025.csv')
                    line = line.replace('Data/ret_day_ST2024.RDS', '/Volumes/BetAlpha/Asset Pricing/Data/Output/ret_day_ST2025.RDS')
                    line = line.replace('Data/ret_day2024.RDS', '/Volumes/BetAlpha/Asset Pricing/Data/Output/ret_day2025.RDS')
                    line = line.replace('Data/TRD_Mnth199012-202412.csv', '/Volumes/BetAlpha/Asset Pricing/Data/Input/Individual Return/TRD_Mnth199012-202512.csv')
                    line = line.replace('/Volumes/BetaAlpha/Assetpricing/Input/Individual Return/TRD_Mnth199012-202412.csv', '/Volumes/BetAlpha/Asset Pricing/Data/Input/Individual Return/TRD_Mnth199012-202512.csv')
                    line = line.replace('2023\'', '2025\'')
                    line = line.replace('2023"', '2025"')
                    line = line.replace('2024\'', '2025\'')
                    line = line.replace('2024"', '2025"')
                    source[i] = line
                cell['source'] = source

        with open(file, 'w', encoding='utf-8') as f:
            json.dump(nb, f, indent=1, ensure_ascii=False)
    except Exception as e:
        pass
