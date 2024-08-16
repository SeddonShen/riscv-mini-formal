#!/bin/bash

# 补丁文件数组
patches=("patch/E1_SLTI.patch" "patch/E2_SUB.patch" "patch/E3_BNE.patch" "patch/E4_LB.patch" "patch/E5_ADDI.patch")


# 计数器，用于文件夹命名
counter=1

# 循环应用补丁、生成文件、移除补丁
for patch in "${patches[@]}"
do
    echo "应用补丁: $patch"
    
    # 应用补丁
    git apply "$patch"
    if [ $? -ne 0 ]; then
        echo "应用补丁 $patch 失败"
        exit 1
    fi

    # 生成 SoC Verilog 文件
    target_dir="test_run_dir/GenerateSoCVerilog_$counter"
    echo "生成文件到: $target_dir"
    
    # 运行测试，生成指定目录的 SoC Verilog 文件
    sbt -DtargetDir=./$target_dir "testOnly formal.RISCVMiniFormalSpec"


    # 检查生成是否成功
    if [ $? -ne 0 ]; then
        echo "生成文件失败，补丁 $patch 可能有问题"
        exit 1
    fi

    # 移除补丁
    echo "移除补丁: $patch"
    git apply -R "$patch"
    if [ $? -ne 0 ]; then
        echo "移除补丁 $patch 失败"
        exit 1
    fi

    # 增加计数器
    counter=$((counter + 1))

    echo "--------------------------------------"
done

echo "所有补丁处理完成！"
