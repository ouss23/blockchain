from Crypto.PublicKey import RSA
from Crypto.Hash import SHA256
from Crypto.Signature import PKCS1_v1_5
import codecs




def create_private_key():
    return RSA.generate(1024).export_key()

def create_public_key(private_key):
    return RSA.import_key(private_key).public_key().export_key()

def create_transaction(private_key, message):
    private_key = RSA.import_key(private_key)
    message = message.encode('utf-8')
    hashe = SHA256.new(message)
    
    signer = PKCS1_v1_5.new(private_key)
    signature = signer.sign(hashe)
    
    return signature

def verify_signature(public_key, message, signature):
    public_key = RSA.import_key(public_key)
    signer = PKCS1_v1_5.new(public_key)
    
    message = message.encode('utf-8')
    hashe = SHA256.new(message)
    
    return signer.verify(hashe, signature)

