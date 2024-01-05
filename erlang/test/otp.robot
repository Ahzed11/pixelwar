*** Variables ***
${OLD_VERSION}    0.1.0
${NEW_VERSION}    0.2.0
${RELEASE_PATH}    erlang/_build/default/rel/pixelwar/bin/pixelwar

*** Settings ***
Suite Setup     Start Release
Suite Teardown     Stop Release
Library    matrix.py
Library    otp.py

*** Test Cases ***
Setup state before upgrade
    Send Pixel    12    12    12
    Send Pixel    222    222    222
    ${state}    Get Matrix State
    Should Be Equal As Erlang Bytes    ${state}    12,0,12,0,12,0,222,0,222,0,222,0
    
upgrade release
    Upgrade Release    ${NEW_VERSION}

Test state after upgrade
    ${state}    Get Matrix State
    Should Be Equal As Erlang Bytes    ${state}    12,0,12,0,12,0

Setup state before downgrade
    Send Pixel    13    13    13
    ${state}    Get Matrix State
    Should Be Equal As Erlang Bytes    ${state}    12,0,12,0,12,0,13,0,13,0,13,0

downgrade release
    Downgrade Release    ${OLD_VERSION}

Test state after downgrade
    ${state}    Get Matrix State
    Should Be Equal As Erlang Bytes    ${state}    12,0,12,0,12,0,13,0,13,0,13,0
