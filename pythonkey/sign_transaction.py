
from function import *
import sys



if __name__ == '__main__':
    private_key = sys.argv[1].encode('utf-8')
    message = sys.argv[2]
    signature = create_transaction(private_key, message)




    print(signature, type(signature), signature)
    sys.exit(signature)