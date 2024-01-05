*** Variables ***
${oldVersion}    0.1.0
${newVersion}    0.2.0
${releasePath}    /home/ahzed11/ssd/Code/Erlang/pw/erlang/_build/default/rel/pixelwar/bin/pixelwar

*** Settings ***
Suite Setup     Start Release
Suite Teardown     Stop Release
Library    otp.py
Library    matrix.py
Library    String

*** Test Cases ***
Setup state before upgrade
    Send Pixel    12    12    12
    Send Pixel    222    222    222
    ${state}    Get Matrix State
    Should Be Equal As Erlang Bytes    ${state}    12,0,12,0,12,0,222,0,222,0,12,0

upgrade release
    Upgrade Release    ${newVersion}

Test state after upgrade
    ${state}    Get Matrix State
    Should Be Equal As Erlang Bytes    ${state}    12,0,12,0,12,0

Setup state before downgrade
    Send Pixel    13    13    13
    ${state}    Get Matrix State
    Should Be Equal As Erlang Bytes    ${state}    12,0,12,0,12,0,13,0,13,0,13,0

downgrade release
    ${res}    Downgrade Release    ${oldVersion}
    Log    ${res}

Test state after downgrade
    ${state}    Get Matrix State
    Should Be Equal As Erlang Bytes    ${state}    12,0,12,0,12,0,13,0,13,0,13,0
