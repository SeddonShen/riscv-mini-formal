python3 parser.py ./CoreSoc_E1.sv ./CoreSoc_E1_rename.sv results_E1
python3 parser.py ./CoreSoc_E2.sv ./CoreSoc_E2_rename.sv results_E2
python3 parser.py ./CoreSoc_E3.sv ./CoreSoc_E3_rename.sv results_E3
python3 parser.py ./CoreSoc_E4.sv ./CoreSoc_E4_rename.sv results_E4
python3 parser.py ./CoreSoc_E5.sv ./CoreSoc_E5_rename.sv results_E5
python3 run_sby.py --results-dir results_E1 --end-id 16 --workers 20
python3 calculate.py --work-dir ./results_E1 > result_E1.txt
python3 run_sby.py --results-dir results_E2 --end-id 16 --workers 20
python3 calculate.py --work-dir ./results_E2 > result_E2.txt
python3 run_sby.py --results-dir results_E3 --end-id 16 --workers 20
python3 calculate.py --work-dir ./results_E3 > result_E3.txt
python3 run_sby.py --results-dir results_E4 --end-id 16 --workers 20
python3 calculate.py --work-dir ./results_E4 > result_E4.txt
python3 run_sby.py --results-dir results_E5 --end-id 16 --workers 20
python3 calculate.py --work-dir ./results_E5 > result_E5.txt



