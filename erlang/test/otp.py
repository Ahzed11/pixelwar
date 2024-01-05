import subprocess

RELEASE_PATH = "/home/ahzed11/ssd/Code/Erlang/pw/erlang/_build/default/rel/pixelwar/bin/pixelwar" # "./relupci/bin/pixelwar"

def run(command):
    result = subprocess.run(command, shell = True, executable="/bin/bash", capture_output=True)
    stdout = result.stdout.decode("utf-8")
    return stdout

def start_release():
    START_COMMAND = f"MATRIX_WIDTH=128 MATRIX_HEIGHT=128 {RELEASE_PATH} daemon"
    return run(START_COMMAND)

def stop_release():
    START_COMMAND = f"{RELEASE_PATH} stop"
    return run(START_COMMAND)

def send_rpc(module, function, arguments):
    SEND_RPC_COMMAND = f"{RELEASE_PATH} rpc {module} {function} {arguments}"
    return run(SEND_RPC_COMMAND)

def upgrade_release(version):
    UPGRADE_COMMAND = f"{RELEASE_PATH} upgrade {version}"
    stdout = run(UPGRADE_COMMAND)

    if stdout != "ok":
        raise AssertionError(f"Command failed: {stdout}")

def downgrade_release(version):
    DOWNGRADE_COMMAND = f"{RELEASE_PATH} downgrade {version}"
    stdout = run(DOWNGRADE_COMMAND)

    if stdout != "ok":
        raise AssertionError(f"Command failed: {stdout}")

def should_be_equal_as_erlang_bytes(actual, expected, msg=None):
    from_as_str = actual.replace("#Bin<", "")
    from_as_str = from_as_str.replace(">\n", "")
    
    if from_as_str != expected:
        if msg:
            raise AssertionError(f"Values are not equal: {from_as_str} != {expected}. {msg}")
        else:
            raise AssertionError(f"Values are not equal: {from_as_str} != {expected}")
