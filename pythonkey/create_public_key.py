from function import *
import sys
if __name__ == '__main__':
    private_key = sys.argv[1].encode('utf-8')




    key = create_public_key(private_key).decode("utf-8")
    sys.exit(key)