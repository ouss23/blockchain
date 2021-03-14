from function import *
import sys
import argparse


parser = argparse.ArgumentParser(description='Description of your program')
parser.add_argument('-p','--public_key', help='Description for foo argument', required=True)
parser.add_argument('-m','--message', help='Description for bar argument', required=True)
parser.add_argument('-s','--signature', help='Description for bar argument', required=True)
args = vars(parser.parse_args())

print("iciiiiiiiiiii", args)

def verify_signature(public_key, message, signature):
    public_key = RSA.import_key(public_key)
    signer = PKCS1_v1_5.new(public_key)
    
    message = message.encode('utf-8')
    hashe = SHA256.new(message)
    
    return signer.verify(hashe, signature)



if __name__ == '__main__':

    public_key = args['public_key'] 
    message = args['message'] 
    signature = bytes.fromhex(args['signature'])

    boo = verify_signature(public_key, message, signature)



    print(boo)

    sys.exit(boo)


