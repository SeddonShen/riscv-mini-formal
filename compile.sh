git apply ./patches/E1_SLTI.patch
sleep 10
mill chiselModule.test.testOnly formal.RISCVMiniFormalSpecSV
mv ./test_run_dir/Elaborate_chirvformal_SystemVerilog/CoreSoc.sv ./verification_files/CoreSoc_E1.sv
git apply -R ./patches/E1_SLTI.patch
sleep 10
git apply ./patches/E2_SUB.patch
sleep 10
mill chiselModule.test.testOnly formal.RISCVMiniFormalSpecSV
mv ./test_run_dir/Elaborate_chirvformal_SystemVerilog/CoreSoc.sv ./verification_files/CoreSoc_E2.sv
git apply -R ./patches/E2_SUB.patch
sleep 10
git apply ./patches/E3_BNE.patch
sleep 10
mill chiselModule.test.testOnly formal.RISCVMiniFormalSpecSV
mv ./test_run_dir/Elaborate_chirvformal_SystemVerilog/CoreSoc.sv ./verification_files/CoreSoc_E3.sv
git apply -R ./patches/E3_BNE.patch
sleep 10
git apply ./patches/E4_BLTU.patch
sleep 10
mill chiselModule.test.testOnly formal.RISCVMiniFormalSpecSV
mv ./test_run_dir/Elaborate_chirvformal_SystemVerilog/CoreSoc.sv ./verification_files/CoreSoc_E4.sv
git apply -R ./patches/E4_BLTU.patch
sleep 10
git apply ./patches/E5_ADDI.patch
sleep 10
mill chiselModule.test.testOnly formal.RISCVMiniFormalSpecSV
mv ./test_run_dir/Elaborate_chirvformal_SystemVerilog/CoreSoc.sv ./verification_files/CoreSoc_E5.sv
git apply -R ./patches/E5_ADDI.patch