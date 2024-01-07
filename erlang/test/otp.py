import subprocess
from robot.api.deco import not_keyword
from robot.libraries.BuiltIn import BuiltIn

@not_keyword
def run(command):
    result = subprocess.run(command, shell = True, executable="/bin/bash", capture_output=True)
    stdout = result.stdout.decode("utf-8")
    return stdout

def start_release():
    RELEASE_PATH = BuiltIn().get_variable_value("${RELEASE_PATH}")
    START_COMMAND = f"MATRIX_WIDTH=128 MATRIX_HEIGHT=128 {RELEASE_PATH} daemon"
    return run(START_COMMAND)

def stop_release():
    RELEASE_PATH = BuiltIn().get_variable_value("${RELEASE_PATH}")
    START_COMMAND = f"{RELEASE_PATH} stop"
    return run(START_COMMAND)

def send_rpc(module, function, arguments):
    RELEASE_PATH = BuiltIn().get_variable_value("${RELEASE_PATH}")
    SEND_RPC_COMMAND = f"{RELEASE_PATH} rpc {module} {function} {arguments}"
    return run(SEND_RPC_COMMAND)

def upgrade_release(version):
    RELEASE_PATH = BuiltIn().get_variable_value("${RELEASE_PATH}")
    UPGRADE_COMMAND = f"{RELEASE_PATH} upgrade {version}"
    stdout = run(UPGRADE_COMMAND)

    if not f"Made release permanent: {version}" in stdout:
        raise AssertionError(f"Command failed: {stdout}")

def downgrade_release(version):
    RELEASE_PATH = BuiltIn().get_variable_value("${RELEASE_PATH}")
    DOWNGRADE_COMMAND = f"{RELEASE_PATH} downgrade {version}"
    stdout = run(DOWNGRADE_COMMAND)

    if not f"Made release permanent: {version}" in stdout:
        raise AssertionError(f"Command failed: {stdout}")

def should_be_equal_as_erlang_bytes(actual, expected, msg=None):
    from_as_str = actual.replace("#Bin<", "")
    from_as_str = from_as_str.replace(">\n", "")
    
    if from_as_str != expected:
        if msg:
            raise AssertionError(f"Values are not equal: {from_as_str} != {expected}. {msg}")
        else:
            raise AssertionError(f"Values are not equal: {from_as_str} != {expected}")
