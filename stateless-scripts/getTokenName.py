import hashlib
import binascii

def print_token(txHash, index):

    for i in range(index+1):
        txBytes = binascii.unhexlify(txHash)
        h = hashlib.new('sha3_256')
        h.update(txBytes)
        txHash = h.hexdigest()
    print(txHash)