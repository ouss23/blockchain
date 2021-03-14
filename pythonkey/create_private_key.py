from function import *
import sys
if __name__ == '__main__':

    key = create_private_key().decode("utf-8")
    print(key)
    sys.exit(key)
