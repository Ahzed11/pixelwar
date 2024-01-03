#!/bin/bash

./relupci/bin/pixelwar rpc pixelwar_matrix_serv set_element [matrix, {13, 13, 13}]

binary1=$(./relupci/bin/pixelwar rpc pixelwar_matrix_serv get_state [matrix])
binary2='#Bin<12,0,12,0,12,0,13,0,13,0,13,0>'
echo $binary1
echo $binary2

if [[ $binary1 == $binary2 ]]; then
  echo "Success"
  exit 0
else
  echo "Fail"
  exit 1
fi
