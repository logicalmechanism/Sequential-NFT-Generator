import hashlib
import binascii

def print_token(txHash, index):

    for i in range(index+1):
        txBytes = binascii.unhexlify(txHash)
        h = hashlib.new('sha3_256')
        h.update(txBytes)
        txHash = h.hexdigest()
    print(txHash)

def print_hex(txHash, index):

    txBytes = binascii.unhexlify(txHash)
    h = hashlib.new('sha3_256')
    h.update(txBytes)
    txHash = h.hexdigest()
    x = hex(index)[-2:]
    if "x" in x:
        x = x.replace("x", "0")
    txHash = x + txHash
    print(txHash[0:64])