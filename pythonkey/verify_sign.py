from function import *
import sys


def verify_signature(public_key, message, signature):
    public_key = RSA.import_key(public_key)
    signer = PKCS1_v1_5.new(public_key)
    
    message = message.encode('utf-8')
    hashe = SHA256.new(message)
    
    return signer.verify(hashe, signature)



if __name__ == '__main__':
    public_key = sys.argv[1]
    message = sys.argv[2]
    signature = sys.argv[3]

    print(public_key, message, signature)
    boo = verify_signature(public_key, message, signature)



    print(boo)

    sys.exit(boo)


