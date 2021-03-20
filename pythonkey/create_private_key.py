from function import *
import sys
if __name__ == '__main__':
    try:

        key = create_private_key().decode("utf-8")
        print(key)
    except Exception as e:
        print(False)
        sys.exit(e)

