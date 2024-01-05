from otp import send_rpc

MODULE = "pixelwar_matrix_serv"
PROCESS = "matrix"

def send_pixel(x, y, color):
    return send_rpc(MODULE, "set_element", f"[{PROCESS}, {{ {x}, {y}, {color} }}]")

def get_matrix_state():
    return send_rpc(MODULE, "get_state", f"[{PROCESS}]")