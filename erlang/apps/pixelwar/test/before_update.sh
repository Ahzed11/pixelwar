#!/bin/bash

./relupci/bin/pixelwar rpc pixelwar_matrix_serv set_element [matrix, {12, 12, 12}]
./relupci/bin/pixelwar rpc pixelwar_matrix_serv set_element [matrix, {222, 222, 12}]

binary1=$(./relupci/bin/pixelwar rpc pixelwar_matrix_serv get_state [matrix])
binary2='#Bin<12,0,12,0,12,0,222,0,222,0,12,0>'
echo $binary1
echo $binary2

if [[ $binary1 == $binary2 ]]; then
  echo "Success"
  exit 0
else
  echo "Fail"
  exit 1
fi
