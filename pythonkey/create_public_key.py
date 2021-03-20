from function import *
import sys
if __name__ == '__main__':
    try:

        private_key = sys.argv[1].encode('utf-8')
        key = create_public_key(private_key).decode("utf-8")

        print(key)
    except Exception as e:
        print(False)
        sys.exit(e)
